{-# LANGUAGE OverloadedStrings #-}

module Editor.CodeEdit.ExpressionEdit.FuncEdit
  (make, makeParamEdit, makeParamNameEdit, makeBodyEdit, addJumpToRHS) where

import Control.Monad (liftM)
import Data.Monoid (mempty, mconcat)
import Data.Store.Guid (Guid)
import Editor.Anchors (ViewTag)
import Editor.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Editor.MonadF (MonadF)
import Editor.OTransaction (OTransaction, WidgetT)
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.Config as Config
import qualified Editor.ITransaction as IT
import qualified Editor.OTransaction as OT
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

makeParamNameEdit
  :: MonadF m
  => Guid
  -> OTransaction t m (WidgetT t m)
makeParamNameEdit ident =
  BWidgets.wrapDelegated paramFDConfig FocusDelegator.NotDelegating id
  (liftM (Widget.tint Config.parameterColor) .
   BWidgets.makeNameEdit ident) $ WidgetIds.fromGuid ident

addJumpToRHS
  :: MonadF m => (E.Doc, Sugar.ExpressionRef m) -> WidgetT ViewTag m -> WidgetT ViewTag m
addJumpToRHS (rhsDoc, rhs) =
  Widget.weakerEvents .
  Widget.keysEventMapMovesCursor Config.jumpLHStoRHSKeys ("Jump to " ++ rhsDoc) $
  return rhsId
  where
    rhsId = WidgetIds.fromGuid $ Sugar.rGuid rhs

-- exported for use in definition sugaring.
makeParamEdit
  :: MonadF m
  => ExpressionGui.Maker m
  -> (E.Doc, Sugar.ExpressionRef m)
  -> Sugar.FuncParam m
  -> OTransaction ViewTag m (ExpressionGui m)
makeParamEdit makeExpressionEdit rhs param =
  (liftM . ExpressionGui.atEgWidget)
  (addJumpToRHS rhs . Widget.weakerEvents paramEventMap) $ do
    paramNameEdit <- makeParamNameEdit ident
    paramTypeEdit <- makeExpressionEdit $ Sugar.fpType param
    return . ExpressionGui.addType (WidgetIds.fromGuid ident)
      [ExpressionGui.egWidget paramTypeEdit] $
      ExpressionGui.fromValueWidget paramNameEdit
  where
    ident = Sugar.fpGuid param
    paramEventMap = mconcat
      [ paramDeleteEventMap
      , paramAddNextEventMap
      ]
    paramAddNextEventMap =
      maybe mempty
      (Widget.keysEventMapMovesCursor Config.addNextParamKeys "Add next parameter" .
       liftM (FocusDelegator.delegatingId . WidgetIds.fromGuid) .
       IT.transaction . Sugar.fpaAddNextParam) $
      Sugar.fpMActions param
    paramDeleteEventMap =
      maybe mempty
      (Widget.keysEventMapMovesCursor Config.delKeys "Delete parameter" .
       liftM WidgetIds.fromGuid .
       IT.transaction . Sugar.fpaDelete) $
      Sugar.fpMActions param

makeParamsEdit
  :: MonadF m
  => ExpressionGui.Maker m
  -> (E.Doc, Sugar.ExpressionRef m)
  -> [Sugar.FuncParam m]
  -> OTransaction ViewTag m (ExpressionGui m)
makeParamsEdit makeExpressionEdit rhs =
  liftM ExpressionGui.hboxSpaced .
  mapM (makeParamEdit makeExpressionEdit rhs)

makeBodyEdit
  :: MonadF m
  => ExpressionGui.Maker m
  -> [Widget.Id]
  -> Sugar.ExpressionRef m
  -> OTransaction ViewTag m (ExpressionGui m)
makeBodyEdit makeExpressionEdit lhs body =
  liftM ((ExpressionGui.atEgWidget . Widget.weakerEvents) jumpToLhsEventMap) $
  makeExpressionEdit body
  where
    lastParam = case lhs of
      [] -> error "BodyEdit given empty LHS"
      xs -> last xs
    jumpToLhsEventMap =
      Widget.keysEventMapMovesCursor Config.jumpRHStoLHSKeys "Jump to last param" $
      return lastParam

make
  :: MonadF m
  => ExpressionGui.Maker m
  -> Sugar.Func m
  -> Widget.Id
  -> OTransaction ViewTag m (ExpressionGui m)
make makeExpressionEdit (Sugar.Func params body) myId =
  OT.assignCursor myId bodyId $ do
    lambdaLabel <-
      liftM ExpressionGui.fromValueWidget .
      OT.setTextSizeColor Config.lambdaTextSize Config.lambdaColor .
      BWidgets.makeLabel "λ" $ Widget.toAnimId myId
    rightArrowLabel <-
      liftM ExpressionGui.fromValueWidget .
      OT.setTextSizeColor Config.rightArrowTextSize Config.rightArrowColor .
      BWidgets.makeLabel "→" $ Widget.toAnimId myId
    bodyEdit <- makeBodyEdit makeExpressionEdit lhs body
    paramsEdit <- makeParamsEdit makeExpressionEdit ("Func Body", body) params
    return $ ExpressionGui.hboxSpaced [ lambdaLabel, paramsEdit, rightArrowLabel, bodyEdit ]
  where
    lhs = map (WidgetIds.fromGuid . Sugar.fpGuid) params
    bodyId = WidgetIds.fromGuid $ Sugar.rGuid body
