{-# LANGUAGE OverloadedStrings #-}

module Editor.CodeEdit.ExpressionEdit.FuncEdit
  (make, makeParamNameEdit, addJumpToRHS, makeResultEdit, makeParamsAndResultEdit) where

import Control.Monad (liftM)
import Data.Monoid (mempty, mconcat)
import Data.Store.Guid (Guid)
import Editor.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Editor.CodeEdit.VarAccess (VarAccess, WidgetT)
import Editor.MonadF (MonadF)
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Editor.CodeEdit.Parens as Parens
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.CodeEdit.VarAccess as VarAccess
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
  => (VarAccess.NameSource, String) -> Guid
  -> VarAccess m (WidgetT m)
makeParamNameEdit name ident =
  ExpressionGui.wrapDelegated paramFDConfig FocusDelegator.NotDelegating id
  (VarAccess.atEnv (OT.setTextColor Config.paramOriginColor) .
   ExpressionGui.makeNameEdit name ident) $ WidgetIds.fromGuid ident

addJumpToRHS
  :: MonadF m => (E.Doc, Sugar.Expression m) -> WidgetT m -> WidgetT m
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
  -> (E.Doc, Sugar.Expression m)
  -> (VarAccess.NameSource, String)
  -> Widget.Id
  -> Sugar.FuncParam m (Sugar.Expression m)
  -> VarAccess m (ExpressionGui m)
makeParamEdit makeExpressionEdit rhs name prevId param =
  (liftM . ExpressionGui.atEgWidget)
  (addJumpToRHS rhs . Widget.weakerEvents paramEventMap) .
  assignCursor $ do
    paramTypeEdit <- makeExpressionEdit $ Sugar.fpType param
    paramNameEdit <- makeParamNameEdit name ident
    return . ExpressionGui.addType ExpressionGui.HorizLine (WidgetIds.fromGuid ident)
      [ExpressionGui.egWidget paramTypeEdit] $
      ExpressionGui.fromValueWidget paramNameEdit
  where
    assignCursor =
      case Sugar.fpHiddenLambdaGuid param of
      Nothing -> id
      Just g ->
        VarAccess.assignCursor (WidgetIds.fromGuid g) $ WidgetIds.fromGuid ident
    ident = Sugar.fpGuid param
    paramEventMap = mconcat
      [ paramDeleteEventMap Config.delForwardKeys "" id
      , paramDeleteEventMap Config.delBackwordKeys " backwards" (const prevId)
      , paramAddNextEventMap
      ]
    paramAddNextEventMap =
      maybe mempty
      (Widget.keysEventMapMovesCursor Config.addNextParamKeys "Add next parameter" .
       liftM (FocusDelegator.delegatingId . WidgetIds.fromGuid) .
       IT.transaction . Sugar.itemAddNext) $
      Sugar.fpMActions param
    paramDeleteEventMap keys docSuffix onId =
      maybe mempty
      (Widget.keysEventMapMovesCursor keys ("Delete parameter" ++ docSuffix) .
       liftM (onId . WidgetIds.fromGuid) .
       IT.transaction . Sugar.itemDelete) $
      Sugar.fpMActions param

makeResultEdit
  :: MonadF m
  => ExpressionGui.Maker m
  -> [Widget.Id]
  -> Sugar.Expression m
  -> VarAccess m (ExpressionGui m)
makeResultEdit makeExpressionEdit lhs result =
  liftM ((ExpressionGui.atEgWidget . Widget.weakerEvents) jumpToLhsEventMap) $
  makeExpressionEdit result
  where
    lastParam = case lhs of
      [] -> error "makeResultEdit given empty LHS"
      xs -> last xs
    jumpToLhsEventMap =
      Widget.keysEventMapMovesCursor Config.jumpRHStoLHSKeys "Jump to last param" $
      return lastParam

make
  :: MonadF m
  => ExpressionGui.Maker m
  -> Sugar.HasParens
  -> Sugar.Func m (Sugar.Expression m)
  -> Widget.Id
  -> VarAccess m (ExpressionGui m)
make makeExpressionEdit hasParens (Sugar.Func params body) =
  ExpressionGui.wrapParenify hasParens Parens.addHighlightedTextParens $ \myId ->
  VarAccess.assignCursor myId bodyId $ do
    lambdaLabel <-
      liftM ExpressionGui.fromValueWidget .
      VarAccess.atEnv (OT.setTextSizeColor Config.lambdaTextSize Config.lambdaColor) .
      VarAccess.otransaction . BWidgets.makeLabel "λ" $ Widget.toAnimId myId
    rightArrowLabel <-
      liftM ExpressionGui.fromValueWidget .
      VarAccess.atEnv (OT.setTextSizeColor Config.rightArrowTextSize Config.rightArrowColor) .
      VarAccess.otransaction . BWidgets.makeLabel "→" $ Widget.toAnimId myId
    (paramsEdits, bodyEdit) <-
      makeParamsAndResultEdit makeExpressionEdit lhs ("Func Body", body) myId params
    return . ExpressionGui.hboxSpaced $
      lambdaLabel : paramsEdits ++ [ rightArrowLabel, bodyEdit ]
  where
    bodyId = WidgetIds.fromGuid $ Sugar.rGuid body
    lhs = map (WidgetIds.fromGuid . Sugar.fpGuid) params

makeParamsAndResultEdit ::
  MonadF m =>
  ExpressionGui.Maker m ->
  [Widget.Id] ->
  (E.Doc, Sugar.Expression m) ->
  Widget.Id ->
  [Sugar.FuncParam m (Sugar.Expression m)] ->
  VarAccess m ([ExpressionGui m], ExpressionGui m)
makeParamsAndResultEdit makeExpressionEdit lhs rhs@(_, result) =
  go
  where
    go _ [] = liftM ((,) []) $ makeResultEdit makeExpressionEdit lhs result
    go prevId (param:params) = do
      let guid = Sugar.fpGuid param
      (name, (paramEdits, resultEdit)) <-
        VarAccess.withParamName guid $
        \name -> liftM ((,) name) $ go (WidgetIds.fromGuid guid) params
      paramEdit <- makeParamEdit makeExpressionEdit rhs name prevId param
      return (paramEdit : paramEdits, resultEdit)
