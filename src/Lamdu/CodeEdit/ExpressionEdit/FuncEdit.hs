{-# LANGUAGE OverloadedStrings #-}

module Lamdu.CodeEdit.ExpressionEdit.FuncEdit
  (make, makeParamNameEdit, jumpToRHS, makeResultEdit, makeParamsAndResultEdit) where

import Control.Lens ((^.))
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM, WidgetT)
import Control.MonadA (MonadA)
import Graphics.UI.Bottle.Widget (Widget)
import qualified Control.Lens as Lens
import qualified Lamdu.BottleWidgets as BWidgets
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.Parens as Parens
import qualified Lamdu.CodeEdit.Settings as Settings
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.Config as Config
import qualified Lamdu.WidgetEnvT as WE
import qualified Lamdu.WidgetIds as WidgetIds
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
  :: MonadA m
  => (ExprGuiM.NameSource, String) -> Guid
  -> ExprGuiM m (WidgetT m)
makeParamNameEdit name ident =
  ExprGuiM.wrapDelegated paramFDConfig FocusDelegator.NotDelegating id
  (ExprGuiM.atEnv (WE.setTextColor Config.paramOriginColor) .
   ExpressionGui.makeNameEdit name ident) $ WidgetIds.fromGuid ident

jumpToRHS ::
  (MonadA m, MonadA f) =>
  [E.ModKey] -> (E.Doc, Sugar.Expression m) -> Widget.EventHandlers f
jumpToRHS keys (rhsDoc, rhs) =
  Widget.keysEventMapMovesCursor keys ("Jump to " ++ rhsDoc) $
  return rhsId
  where
    rhsId = WidgetIds.fromGuid $ rhs ^. Sugar.rGuid

-- exported for use in definition sugaring.
makeParamEdit ::
  MonadA m =>
  ((ExprGuiM.NameSource, String) ->
   Widget (Transaction m) -> Widget (Transaction m)) ->
  (E.Doc, Sugar.Expression m) ->
  Widget.Id -> (ExprGuiM.NameSource, String) ->
  Sugar.FuncParam m (Sugar.Expression m) ->
  ExprGuiM m (ExpressionGui m)
makeParamEdit atParamWidgets rhs prevId name param = do
  infoMode <- fmap (Lens.view Settings.sInfoMode) ExprGuiM.readSettings
  (fmap . Lens.over ExpressionGui.egWidget) onFinalWidget . assignCursor $ do
    paramTypeEdit <- ExprGuiM.makeSubexpresion $ param ^. Sugar.fpType
    paramNameEdit <- makeParamNameEdit name ident
    let typeWidget = paramTypeEdit ^. ExpressionGui.egWidget
    infoWidget <-
      case (infoMode, mActions) of
      (Settings.InfoExamples, Just actions) -> do
        exampleSugar <- ExprGuiM.fmapemoT $ Lens.view Sugar.fpGetExample actions
        exampleGui <-
          fmap (Lens.view ExpressionGui.egWidget) $
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
      case param ^. Sugar.fpHiddenLambdaGuid of
      Nothing -> id
      Just g ->
        ExprGuiM.assignCursor (WidgetIds.fromGuid g) myId
    myId = WidgetIds.fromGuid ident
    ident = param ^. Sugar.fpGuid
    paramEventMap = mconcat
      [ paramDeleteEventMap Config.delForwardKeys "" id
      , paramDeleteEventMap Config.delBackwordKeys " backwards" (const prevId)
      , paramAddNextEventMap
      ]
    mActions = param ^. Sugar.fpMActions
    paramAddNextEventMap =
      maybe mempty
      (Widget.keysEventMapMovesCursor Config.addNextParamKeys "Add next parameter" .
       fmap (FocusDelegator.delegatingId . WidgetIds.fromGuid) .
       Lens.view (Sugar.fpListItemActions . Sugar.itemAddNext))
      mActions
    paramDeleteEventMap keys docSuffix onId =
      maybe mempty
      (Widget.keysEventMapMovesCursor keys ("Delete parameter" ++ docSuffix) .
       fmap (onId . WidgetIds.fromGuid) .
       Lens.view (Sugar.fpListItemActions . Sugar.itemDelete))
      mActions

makeResultEdit
  :: MonadA m
  => [Widget.Id]
  -> Sugar.Expression m
  -> ExprGuiM m (ExpressionGui m)
makeResultEdit lhs result =
  fmap ((Lens.over ExpressionGui.egWidget . Widget.weakerEvents) jumpToLhsEventMap) $
  ExprGuiM.makeSubexpresion result
  where
    lastParam = case lhs of
      [] -> error "makeResultEdit given empty LHS"
      xs -> last xs
    jumpToLhsEventMap =
      Widget.keysEventMapMovesCursor Config.jumpRHStoLHSKeys "Jump to last param" $
      return lastParam

make
  :: MonadA m
  => Sugar.HasParens
  -> Sugar.Func m (Sugar.Expression m)
  -> Widget.Id
  -> ExprGuiM m (ExpressionGui m)
make hasParens (Sugar.Func depParams params body) =
  ExpressionGui.wrapParenify hasParens Parens.addHighlightedTextParens $ \myId ->
  ExprGuiM.assignCursor myId bodyId $ do
    lambdaLabel <-
      fmap ExpressionGui.fromValueWidget .
      ExprGuiM.atEnv (WE.setTextSizeColor Config.lambdaTextSize Config.lambdaColor) .
      ExprGuiM.widgetEnv . BWidgets.makeLabel "λ" $ Widget.toAnimId myId
    rightArrowLabel <-
      fmap ExpressionGui.fromValueWidget .
      ExprGuiM.atEnv (WE.setTextSizeColor Config.rightArrowTextSize Config.rightArrowColor) .
      ExprGuiM.widgetEnv . BWidgets.makeLabel "→" $ Widget.toAnimId myId
    (depParamsEdits, paramsEdits, bodyEdit) <-
      makeParamsAndResultEdit (const id) lhs ("Func Body", body) myId depParams params
    return . ExpressionGui.hboxSpaced $
      concat
      [ [lambdaLabel]
      , depParamsEdits
      , paramsEdits
      , [ rightArrowLabel, bodyEdit ]
      ]
  where
    allParams = depParams ++ params
    bodyId = WidgetIds.fromGuid $ body ^. Sugar.rGuid
    lhs = map (WidgetIds.fromGuid . Lens.view Sugar.fpGuid) allParams

makeParamsAndResultEdit ::
  MonadA m =>
  ((ExprGuiM.NameSource, String) ->
   Widget (Transaction m) -> Widget (Transaction m)) ->
  [Widget.Id] -> (E.Doc, Sugar.Expression m) ->
  Widget.Id ->
  [Sugar.FuncParam m (Sugar.Expression m)] ->
  [Sugar.FuncParam m (Sugar.Expression m)] ->
  ExprGuiM m ([ExpressionGui m], [ExpressionGui m], ExpressionGui m)
makeParamsAndResultEdit atParamWidgets lhs rhs@(_, result) firstParId depParams params = do
  (depParamsEdits, (paramsEdits, resultEdit)) <-
    makeNestedParams firstParId depParams $ \nextParId ->
    makeNestedParams nextParId params $ \_ ->
    makeResultEdit lhs result
  return (depParamsEdits, paramsEdits, resultEdit)
  where
    makeNestedParams guid l mkFinal =
      makeNestedParamNames (Lens.view Sugar.fpGuid)
      (makeParamEdit atParamWidgets rhs)
      mkFinal guid l

makeNestedParamNames ::
  MonadA m =>
  (a -> Guid) ->
  (Widget.Id -> (ExprGuiM.NameSource, String) -> a -> ExprGuiM m item) ->
  (Widget.Id -> ExprGuiM m final) ->
  Widget.Id -> [a] ->
  ExprGuiM m ([item], final)
makeNestedParamNames itemGuid makeItem mkFinal = go
  where
    go wId [] = fmap ((,) []) $ mkFinal wId
    go oldWId (x:xs) = do
      let
        guid = itemGuid x
        newWId = WidgetIds.fromGuid guid
      (name, (items, final)) <-
        ExprGuiM.withParamName guid $ \name ->
        fmap ((,) name) $ go newWId xs
      item <- makeItem oldWId name x
      return (item : items, final)
