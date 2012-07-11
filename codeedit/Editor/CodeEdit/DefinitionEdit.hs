{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.DefinitionEdit(make, makeParts) where

import Control.Arrow (second)
import Control.Monad (liftM)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import Editor.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Editor.MonadF (MonadF)
import Editor.OTransaction (OTransaction, TWidget)
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
  -> (E.Doc, Sugar.ExpressionRef m)
  -> [Sugar.FuncParam m]
  -> TWidget ViewTag m
makeLHSEdit makeExpressionEdit myId ident mAddFirstParameter rhs params = do
  nameEdit <-
    liftM (FuncEdit.addJumpToRHS rhs . Widget.weakerEvents addFirstParamEventMap) $
    makeNameEdit myId ident
  -- no type for def name (yet):
  nameTypeFiller <- BWidgets.spaceWidget
  BWidgets.gridHSpacedCentered . List.transpose .
    map ListUtils.pairList . ((nameEdit, nameTypeFiller) :) . map scaleDownType =<<
    mapM (FuncEdit.makeParamEdit makeExpressionEdit rhs) params
  where
    addFirstParamEventMap =
      maybe mempty
      (Widget.keysEventMapMovesCursor Config.addNextParamKeys
       "Add parameter" .
       liftM (FocusDelegator.delegatingId . WidgetIds.paramId) .
       IT.transaction)
      mAddFirstParameter
    scaleDownType = second $ Widget.scale Config.typeScaleFactor

makeParts
  :: MonadF m
  => ExpressionGui.Maker m
  -> Widget.Id
  -> Guid
  -> Sugar.ExpressionRef m
  -> OTransaction ViewTag m [ExpressionGui m]
makeParts makeExpressionEdit myId guid exprRef = do
  let
    sExpr = Sugar.rExpression exprRef
    func =
      case sExpr of
      Sugar.ExpressionFunc _ x -> x
      _ -> Sugar.Func [] exprRef
  lhsEdit <-
    makeLHSEdit makeExpressionEdit myId guid
    ((fmap Sugar.lambdaWrap . Sugar.eActions . Sugar.rEntity) exprRef)
    ("Def Body", Sugar.fBody func) $ Sugar.fParams func
  equals <- BWidgets.makeLabel "=" $ Widget.toAnimId myId
  let
    lhs = myId : map (WidgetIds.paramId . Sugar.guid . Sugar.fpEntity) (Sugar.fParams func)
  rhsEdit <-
    FuncEdit.makeBodyEdit makeExpressionEdit lhs $ Sugar.fBody func
  space <- BWidgets.spaceWidget
  return
    [ ExpressionGui.fromValueWidget lhsEdit
    , ExpressionGui.fromValueWidget space
    , ExpressionGui.fromValueWidget equals
    , ExpressionGui.fromValueWidget space
    , rhsEdit
    ]

make
  :: MonadF m
  => ExpressionGui.Maker m
  -> Sugar.DefinitionRef m
  -> TWidget ViewTag m
make makeExpressionEdit (Sugar.DefinitionRef guid defBody) =
  liftM (ExpressionGui.egWidget . ExpressionGui.hbox) $
    makeParts makeExpressionEdit
    (WidgetIds.fromGuid guid) guid defBody
