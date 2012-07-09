{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.DefinitionEdit(make, makeParts) where

import Control.Arrow (second)
import Control.Monad (liftM)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import Editor.CodeEdit.InferredTypes(addType)
import Editor.MonadF (MonadF)
import Editor.OTransaction (OTransaction, TWidget, WidgetT)
import qualified Data.List as List
import qualified Data.List.Utils as ListUtils
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Editor.CodeEdit.ExpressionEdit.FuncEdit as FuncEdit
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.Config as Config
import qualified Editor.ITransaction as IT
import qualified Editor.WidgetIds as WidgetIds
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

makeNameEdit :: MonadF m => Widget.Id -> Guid -> TWidget t m
makeNameEdit myId ident =
  BWidgets.wrapDelegated paramFDConfig FocusDelegator.NotDelegating id
  (BWidgets.setTextColor Config.definitionColor .
   BWidgets.makeNameEdit "<unnamed>" ident)
  myId

makeLHSEdit
  :: MonadF m
  => ExpressionGui.Maker m
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
         map ListUtils.pairList . ((nameEdit, nameTypeFiller) :) . map scaleDownType) .
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

makeDefBodyParts
  :: MonadF m
  => ExpressionGui.Maker m
  -> Widget.Id
  -> Guid
  -> Sugar.ExpressionRef m
  -> OTransaction ViewTag m [WidgetT ViewTag m]
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
    [ lhsEdit
    , BWidgets.spaceWidget
    , equals
    , BWidgets.spaceWidget
    , rhsEdit
    ]

makeParts
  :: MonadF m
  => ExpressionGui.Maker m
  -> Widget.Id -> Guid -> Sugar.ExpressionRef m -> Sugar.ExpressionRef m
  -> OTransaction ViewTag m [[WidgetT ViewTag m]]
makeParts makeExpressionEdit myId guid defBody defType = do
  typeEdit <- makeExpressionEdit defType
  colon <- BWidgets.makeLabel ":" $ Widget.toAnimId myId
  name <- makeNameEdit (Widget.joinId myId ["typeDeclName"]) guid
  let
    typeLineParts =
      [ name
      , BWidgets.spaceWidget
      , colon
      , BWidgets.spaceWidget
      , typeEdit
      ]
  defBodyParts <- makeDefBodyParts makeExpressionEdit myId guid defBody
  return [typeLineParts, defBodyParts]

make
  :: MonadF m
  => ExpressionGui.Maker m
  -> Guid
  -> Sugar.ExpressionRef m
  -> Sugar.ExpressionRef m
  -> [Sugar.ExpressionRef m]
  -> TWidget ViewTag m
make makeExpressionEdit guid defBody defType inferredTypes = do
  parts <-
    makeParts makeExpressionEdit
    (WidgetIds.fromGuid guid) guid defBody defType
  inferredTypesEdits <- mapM makeExpressionEdit inferredTypes
  return .
    addType exprId inferredTypesEdits .
    Grid.toWidget $
    Grid.makeAlign 0 parts
  where
    exprId = WidgetIds.fromGuid . Sugar.guid . Sugar.rEntity $ defBody
