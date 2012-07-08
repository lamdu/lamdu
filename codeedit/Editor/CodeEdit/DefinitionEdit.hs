{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.DefinitionEdit(make, makeParts, addJumps) where

import Control.Arrow (second)
import Control.Monad (liftM)
import Data.List.Utils (atPred, pairList)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Data.Vector.Vector2 (Vector2(..))
import Editor.Anchors (ViewTag)
import Editor.CodeEdit.ExpressionEdit.ExpressionMaker(ExpressionEditMaker)
import Editor.CodeEdit.InferredTypes(addType)
import Editor.ITransaction (ITransaction)
import Editor.MonadF (MonadF)
import Editor.OTransaction (OTransaction, TWidget, WidgetT)
import qualified Data.List as List
import qualified Editor.Anchors as Anchors
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.ExpressionEdit.FuncEdit as FuncEdit
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.Config as Config
import qualified Editor.ITransaction as IT
import qualified Editor.OTransaction as OT
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Direction as Direction
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid

paramFDConfig :: FocusDelegator.Config
paramFDConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKey = E.ModKey E.noMods E.KeyEnter
  , FocusDelegator.startDelegatingDoc = "Change parameter name"
  , FocusDelegator.stopDelegatingKey = E.ModKey E.noMods E.KeyEsc
  , FocusDelegator.stopDelegatingDoc = "Stop changing name"
  }

data Side = LHS | RHS
  deriving (Show, Eq)

makeNameEdit :: MonadF m => Widget.Id -> Guid -> TWidget t m
makeNameEdit myId ident =
  BWidgets.wrapDelegated paramFDConfig FocusDelegator.NotDelegating id
  (BWidgets.setTextColor Config.definitionColor .
   BWidgets.makeNameEdit "<unnamed>" ident)
  myId

makeLHSEdit
  :: MonadF m
  => ExpressionEditMaker m
  -> Widget.Id
  -> Guid
  -> Maybe (Transaction ViewTag m Guid)
  -> [Sugar.FuncParam m]
  -> TWidget ViewTag m
makeLHSEdit makeExpressionEdit myId ident mAddFirstParameter params = do
  nameEdit <-
    (liftM . Widget.weakerEvents) addFirstParamEventMap $
    makeNameEdit myId ident
  liftM (BWidgets.gridHSpacedCentered . List.transpose .
         map pairList . ((nameEdit, nameTypeFiller) :) . map scaleDownType) .
    mapM (FuncEdit.makeParamEdit makeExpressionEdit) $ params
  where
    addFirstParamEventMap =
      maybe mempty
      (Widget.keysEventMapMovesCursor Config.addNextParamKeys
       "Add parameter" .
       liftM (FocusDelegator.delegatingId . WidgetIds.paramId) .
       IT.transaction)
      mAddFirstParameter
    scaleDownType = second $ Widget.scale Config.typeScaleFactor
    -- no type for def name (yet):
    nameTypeFiller = BWidgets.spaceWidget

-- from lhs->rhs and vice-versa:
addJumps
  :: Monad m
  => Widget.Id
  -> [(Maybe Side, Grid.GridElement (ITransaction ViewTag m))]
  -> [(Maybe Side, Grid.GridElement (ITransaction ViewTag m))]
addJumps cursor defKGridElements =
  addEventMap LHS RHS "right-hand side" Config.jumpToRhsKeys Direction.fromLeft .
  addEventMap RHS LHS "left-hand side"  Config.jumpToLhsKeys Direction.fromRight $
  defKGridElements
  where
    addEventMap srcSide destSide doc keys dir =
      atPred (== Just srcSide)
      (addJumpsTo doc keys dir $ Grid.getElement (Just destSide) defKGridElements)
    addJumpsTo doc keys dir =
      Grid.atGridElementW . Widget.atWEventMap . flip mappend .
      jumpToExpressionEventMap doc keys dir
    jumpToExpressionEventMap doc keys dir destElement =
      maybe mempty
      (makeJumpForEnter doc keys dir destElement) .
      Widget.wMaybeEnter $ Grid.gridElementW destElement
    makeJumpForEnter doc keys dir destElement enter =
      E.keyPresses keys ("Jump to "++doc) .
      (IT.transaction (Anchors.savePreJumpPosition cursor) >>) .
      Widget.enterResultEvent . enter . dir $
      Grid.gridElementRect destElement

makeDefBodyParts
  :: MonadF m
  => ExpressionEditMaker m
  -> Widget.Id
  -> Guid
  -> Sugar.ExpressionRef m
  -> OTransaction ViewTag m [(Maybe Side, (Grid.Alignment, WidgetT ViewTag m))]
makeDefBodyParts makeExpressionEdit myId guid exprRef = do
  let
    sExpr = Sugar.rExpression exprRef
    func =
      case sExpr of
      Sugar.ExpressionFunc _ x -> x
      _ -> Sugar.Func [] exprRef
  lhsEdit <-
    makeLHSEdit makeExpressionEdit myId guid
    ((fmap Sugar.lambdaWrap . Sugar.eActions . Sugar.rEntity) exprRef) (Sugar.fParams func)
  equals <- BWidgets.makeLabel "=" $ Widget.toAnimId myId
  rhsEdit <- makeExpressionEdit $ Sugar.fBody func
  return $
    [(Just LHS, (Vector2 1 0.5, lhsEdit))
    ,(Nothing, (0, BWidgets.spaceWidget))
    ,(Nothing, (0.5, equals))
    ,(Nothing, (0, BWidgets.spaceWidget))
    ,(Just RHS, (Vector2 0 0.5, rhsEdit))
    ]

makeParts
  :: MonadF m
  => ExpressionEditMaker m
  -> Widget.Id -> Guid -> Sugar.ExpressionRef m -> Sugar.ExpressionRef m
  -> OTransaction ViewTag m [[(Maybe Side, (Grid.Alignment, WidgetT ViewTag m))]]
makeParts makeExpressionEdit myId guid defBody defType = do
  typeEdit <- makeExpressionEdit defType
  colon <- BWidgets.makeLabel ":" $ Widget.toAnimId myId
  name <- makeNameEdit (Widget.joinId myId ["typeDeclName"]) guid
  let
    typeLineParts =
      [ (Just LHS, (Vector2 1 0.5, name))
      , (Nothing, (0.5, BWidgets.spaceWidget))
      , (Nothing, (0.5, colon))
      , (Nothing, (0.5, BWidgets.spaceWidget))
      , (Just RHS, (Vector2 0 0.5, typeEdit))
      ]
  defBodyParts <- makeDefBodyParts makeExpressionEdit myId guid defBody
  return [typeLineParts, defBodyParts]

make
  :: MonadF m
  => ExpressionEditMaker m
  -> Guid
  -> Sugar.ExpressionRef m
  -> Sugar.ExpressionRef m
  -> [Sugar.ExpressionRef m]
  -> TWidget ViewTag m
make makeExpressionEdit guid defBody defType inferredTypes = do
  cursor <- OT.readCursor
  parts <-
    makeParts makeExpressionEdit
    (WidgetIds.fromGuid guid) guid defBody defType
  inferredTypesEdits <- mapM makeExpressionEdit inferredTypes
  return .
    addType exprId inferredTypesEdits .
    Grid.toWidget .
    (Grid.atGridContent . map) (addJumps cursor) .
    Grid.makeKeyed $
    parts
  where
    exprId = WidgetIds.fromGuid . Sugar.guid . Sugar.rEntity $ defBody
