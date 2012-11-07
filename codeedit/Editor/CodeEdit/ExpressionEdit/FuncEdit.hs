{-# LANGUAGE OverloadedStrings #-}

module Editor.CodeEdit.ExpressionEdit.FuncEdit
  (make, makeParamNameEdit, jumpToRHS, makeResultEdit, makeParamsAndResultEdit) where

import Control.Monad (liftM)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import Editor.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Editor.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM, WidgetT)
import Editor.MonadF (MonadF)
import Graphics.UI.Bottle.Widget (Widget)
import qualified Control.Lens as Lens
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Editor.CodeEdit.Parens as Parens
import qualified Editor.CodeEdit.Settings as Settings
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.Config as Config
import qualified Editor.WidgetEnvT as WE
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
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
  => (ExprGuiM.NameSource, String) -> Guid
  -> ExprGuiM m (WidgetT m)
makeParamNameEdit name ident =
  ExpressionGui.wrapDelegated paramFDConfig FocusDelegator.NotDelegating id
  (ExprGuiM.atEnv (WE.setTextColor Config.paramOriginColor) .
   ExpressionGui.makeNameEdit name ident) $ WidgetIds.fromGuid ident

jumpToRHS ::
  (MonadF m, MonadF f) =>
  [E.ModKey] -> (E.Doc, Sugar.Expression m) -> Widget.EventHandlers f
jumpToRHS keys (rhsDoc, rhs) =
  Widget.keysEventMapMovesCursor keys ("Jump to " ++ rhsDoc) $
  return rhsId
  where
    rhsId = WidgetIds.fromGuid $ Sugar.rGuid rhs

-- exported for use in definition sugaring.
makeParamEdit ::
  MonadF m =>
  ((ExprGuiM.NameSource, String) ->
   Widget (Transaction ViewTag m) -> Widget (Transaction ViewTag m)) ->
  (E.Doc, Sugar.Expression m) ->
  (ExprGuiM.NameSource, String) -> Widget.Id ->
  Sugar.FuncParam m (Sugar.Expression m) ->
  ExprGuiM m (ExpressionGui m)
makeParamEdit atParamWidgets rhs name prevId param = do
  infoMode <- liftM (Lens.view Settings.sInfoMode) ExprGuiM.readSettings
  (liftM . ExpressionGui.atEgWidget) onFinalWidget . assignCursor $ do
    paramTypeEdit <- ExprGuiM.makeSubexpresion $ Sugar.fpType param
    paramNameEdit <- makeParamNameEdit name ident
    let typeWidget = ExpressionGui.egWidget paramTypeEdit
    infoWidget <-
      case (infoMode, mActions) of
      (Settings.InfoExamples, Just actions) -> do
        exampleSugar <- ExprGuiM.transaction $ Sugar.fpGetExample actions
        exampleGui <-
          liftM ExpressionGui.egWidget $
          ExprGuiM.makeSubexpresion exampleSugar
        return $ Box.vboxCentered [exampleGui, typeWidget]
      _ -> return typeWidget
    return .
      ExpressionGui.addType ExpressionGui.HorizLine myId [infoWidget] $
      ExpressionGui.fromValueWidget paramNameEdit
  where
    onFinalWidget =
      Widget.weakerEvents
      (jumpToRHS Config.jumpLHStoRHSKeys rhs `mappend` paramEventMap) .
      atParamWidgets name
    assignCursor =
      case Sugar.fpHiddenLambdaGuid param of
      Nothing -> id
      Just g ->
        ExprGuiM.assignCursor (WidgetIds.fromGuid g) myId
    myId = WidgetIds.fromGuid ident
    ident = Sugar.fpGuid param
    paramEventMap = mconcat
      [ paramDeleteEventMap Config.delForwardKeys "" id
      , paramDeleteEventMap Config.delBackwordKeys " backwards" (const prevId)
      , paramAddNextEventMap
      ]
    mActions = Sugar.fpMActions param
    paramAddNextEventMap =
      maybe mempty
      (Widget.keysEventMapMovesCursor Config.addNextParamKeys "Add next parameter" .
       liftM (FocusDelegator.delegatingId . WidgetIds.fromGuid) .
       Sugar.itemAddNext . Sugar.fpListItemActions)
      mActions
    paramDeleteEventMap keys docSuffix onId =
      maybe mempty
      (Widget.keysEventMapMovesCursor keys ("Delete parameter" ++ docSuffix) .
       liftM (onId . WidgetIds.fromGuid) .
       Sugar.itemDelete . Sugar.fpListItemActions)
      mActions

makeResultEdit
  :: MonadF m
  => [Widget.Id]
  -> Sugar.Expression m
  -> ExprGuiM m (ExpressionGui m)
makeResultEdit lhs result =
  liftM ((ExpressionGui.atEgWidget . Widget.weakerEvents) jumpToLhsEventMap) $
  ExprGuiM.makeSubexpresion result
  where
    lastParam = case lhs of
      [] -> error "makeResultEdit given empty LHS"
      xs -> last xs
    jumpToLhsEventMap =
      Widget.keysEventMapMovesCursor Config.jumpRHStoLHSKeys "Jump to last param" $
      return lastParam

make
  :: MonadF m
  => Sugar.HasParens
  -> Sugar.Func m (Sugar.Expression m)
  -> Widget.Id
  -> ExprGuiM m (ExpressionGui m)
make hasParens (Sugar.Func params body) =
  ExpressionGui.wrapParenify hasParens Parens.addHighlightedTextParens $ \myId ->
  ExprGuiM.assignCursor myId bodyId $ do
    lambdaLabel <-
      liftM ExpressionGui.fromValueWidget .
      ExprGuiM.atEnv (WE.setTextSizeColor Config.lambdaTextSize Config.lambdaColor) .
      ExprGuiM.widgetEnv . BWidgets.makeLabel "λ" $ Widget.toAnimId myId
    rightArrowLabel <-
      liftM ExpressionGui.fromValueWidget .
      ExprGuiM.atEnv (WE.setTextSizeColor Config.rightArrowTextSize Config.rightArrowColor) .
      ExprGuiM.widgetEnv . BWidgets.makeLabel "→" $ Widget.toAnimId myId
    (paramsEdits, bodyEdit) <-
      makeParamsAndResultEdit (const id) lhs ("Func Body", body) myId params
    return . ExpressionGui.hboxSpaced $
      lambdaLabel : paramsEdits ++ [ rightArrowLabel, bodyEdit ]
  where
    bodyId = WidgetIds.fromGuid $ Sugar.rGuid body
    lhs = map (WidgetIds.fromGuid . Sugar.fpGuid) params

makeParamsAndResultEdit ::
  MonadF m =>
  ((ExprGuiM.NameSource, String) ->
   Widget (Transaction ViewTag m) -> Widget (Transaction ViewTag m)) ->
  [Widget.Id] -> (E.Doc, Sugar.Expression m) ->
  Widget.Id -> [Sugar.FuncParam m (Sugar.Expression m)] ->
  ExprGuiM m ([ExpressionGui m], ExpressionGui m)
makeParamsAndResultEdit atParamWidgets lhs rhs@(_, result) =
  go
  where
    go _ [] = liftM ((,) []) $ makeResultEdit lhs result
    go prevId (param:params) = do
      let guid = Sugar.fpGuid param
      (name, (paramEdits, resultEdit)) <-
        ExprGuiM.withParamName guid $
        \name -> liftM ((,) name) $ go (WidgetIds.fromGuid guid) params
      paramEdit <- makeParamEdit atParamWidgets rhs name prevId param
      return (paramEdit : paramEdits, resultEdit)
