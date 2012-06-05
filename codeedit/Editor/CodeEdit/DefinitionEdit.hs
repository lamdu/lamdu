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
import Editor.CTransaction (CTransaction, TWidget, readCursor)
import Editor.CodeEdit.ExpressionEdit.ExpressionMaker(ExpressionEditMaker)
import Editor.CodeEdit.InferredTypes(addType)
import Editor.MonadF (MonadF)
import Graphics.UI.Bottle.Widget (Widget)
import qualified Data.List as List
import qualified Editor.Anchors as Anchors
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.ExpressionEdit.FuncEdit as FuncEdit
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.Config as Config
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Direction as Direction
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid

data Side = LHS | RHS
  deriving (Show, Eq)

makeNameEdit :: MonadF m => Widget.Id -> Guid -> TWidget t m
makeNameEdit myId ident =
  BWidgets.wrapDelegated FocusDelegator.NotDelegating
  (BWidgets.setTextColor Config.definitionColor .
   BWidgets.makeNameEdit Config.unnamedStr ident)
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
  liftM (BWidgets.gridHSpaced . List.transpose .
         map pairList . ((nameEdit, nameTypeFiller) :) . map scaleDownType) .
    mapM (FuncEdit.makeParamEdit makeExpressionEdit) $ params
  where
    addFirstParamEventMap =
      maybe mempty
      (Widget.actionEventMapMovesCursor Config.addNextParamKeys
       "Add parameter" .
       liftM (FocusDelegator.delegatingId . WidgetIds.paramId))
      mAddFirstParameter
    scaleDownType = second $ Widget.scale Config.typeScaleFactor
    -- no type for def name (yet):
    nameTypeFiller = BWidgets.spaceWidget

-- from lhs->rhs and vice-versa:
addJumps
  :: Monad m
  => Widget.Id
  -> [(Maybe Side, Grid.GridElement (Transaction ViewTag m))]
  -> [(Maybe Side, Grid.GridElement (Transaction ViewTag m))]
addJumps cursor defKGridElements =
  addEventMap LHS RHS "right-hand side" Config.jumpToRhsKeys Direction.fromLeft .
  addEventMap RHS LHS "left-hand side"  Config.jumpToLhsKeys Direction.fromRight $
  defKGridElements
  where
    addEventMap srcSide destSide doc keys dir =
      atPred (== Just srcSide)
      (addJumpsTo doc keys dir $ Grid.getElement (Just destSide) defKGridElements)
    addJumpsTo doc keys dir =
      Grid.atGridElementSdwd . Widget.atSdwdEventMap . flip mappend .
      jumpToExpressionEventMap doc keys dir
    jumpToExpressionEventMap doc keys dir destElement =
      maybe mempty
      (makeJumpForEnter doc keys dir destElement) .
      Widget.sdwdMaybeEnter $ Grid.gridElementSdwd destElement
    makeJumpForEnter doc keys dir destElement enter =
      E.fromEventTypes keys ("Jump to "++doc) .
      (Anchors.savePreJumpPosition cursor >>) .
      Widget.enterResultEvent . enter . dir $
      Grid.gridElementRect destElement

makeDefBodyParts
  :: MonadF m
  => ExpressionEditMaker m
  -> Widget.Id
  -> Guid
  -> Sugar.ExpressionRef m
  -> CTransaction ViewTag m [(Maybe Side, Widget (Transaction ViewTag m))]
makeDefBodyParts makeExpressionEdit myId guid exprRef = do
  let
    sExpr = Sugar.rExpression exprRef
    func =
      case sExpr of
      Sugar.ExpressionFunc _ x -> x
      _ -> Sugar.Func [] exprRef
  lhsEdit <-
    makeLHSEdit makeExpressionEdit myId guid
    (Sugar.lambdaWrap (Sugar.rActions exprRef)) (Sugar.fParams func)
  equals <- BWidgets.makeLabel "=" $ Widget.toAnimId myId
  rhsEdit <- makeExpressionEdit $ Sugar.fBody func
  return $
    zipWith (second . Widget.align . (`Vector2` 0.5)) [1, 0.5, 0.5, 0.5, 0]
    [(Just LHS, lhsEdit)
    ,(Nothing, BWidgets.spaceWidget)
    ,(Nothing, equals)
    ,(Nothing, BWidgets.spaceWidget)
    ,(Just RHS, rhsEdit)
    ]

makeParts
  :: MonadF m
  => ExpressionEditMaker m
  -> Widget.Id -> Guid -> Sugar.ExpressionRef m -> Sugar.ExpressionRef m
  -> CTransaction ViewTag m [[(Maybe Side, Widget (Transaction ViewTag m))]]
makeParts makeExpressionEdit myId guid defBody defType = do
  typeEdit <- makeExpressionEdit defType
  colon <- BWidgets.makeLabel ":" $ Widget.toAnimId myId
  name <- makeNameEdit (Widget.joinId myId ["typeDeclName"]) guid
  let
    typeLineParts =
      [ (Just LHS, name)
      , (Nothing, BWidgets.spaceWidget)
      , (Nothing, colon)
      , (Nothing, BWidgets.spaceWidget)
      , (Just RHS, typeEdit)
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
  cursor <- readCursor
  parts <-
    makeParts makeExpressionEdit
    (WidgetIds.fromGuid guid) guid defBody defType
  inferredTypesEdits <- mapM makeExpressionEdit inferredTypes
  let
    exprId = WidgetIds.fromGuid . Sugar.guid . Sugar.rActions $ defBody
  return .
    addType exprId inferredTypesEdits .
    Grid.toWidget .
    (Grid.atGridContent . fmap . map) (addJumps cursor) .
    Grid.makeKeyed .
    (map . map . second) (Widget.align (Vector2 0 0.5)) $
    parts
