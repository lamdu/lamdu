{-# LANGUAGE OverloadedStrings #-}

module Lamdu.CodeEdit.ExpressionEdit.FuncEdit
  ( make, makeParamNameEdit, jumpToRHS, makeResultEdit
  , makeNestedParams, makeParamsAndResultEdit
  ) where

import Control.Applicative ((<$), (<$>), Applicative(..))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Data.Traversable (traverse)
import Graphics.UI.Bottle.Widget (Widget)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM, WidgetT)
import qualified Control.Lens as Lens
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.ExpressionEdit.Parens as Parens
import qualified Lamdu.CodeEdit.Settings as Settings
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.Config as Config
import qualified Lamdu.WidgetIds as WidgetIds

type T = Transaction

paramFDConfig :: FocusDelegator.Config
paramFDConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKey = E.ModKey E.noMods E.KeyEnter
  , FocusDelegator.startDelegatingDoc = E.Doc ["Edit", "Rename parameter"]
  , FocusDelegator.stopDelegatingKey = E.ModKey E.noMods E.KeyEsc
  , FocusDelegator.stopDelegatingDoc = E.Doc ["Edit", "Done renaming"]
  }

makeParamNameEdit
  :: MonadA m
  => Sugar.Name -> Guid
  -> ExprGuiM m (WidgetT m)
makeParamNameEdit name ident =
  ExprGuiM.wrapDelegated paramFDConfig FocusDelegator.NotDelegating id
  (ExprGuiM.withFgColor Config.paramOriginColor .
   ExpressionGui.makeNameEdit name ident) $ WidgetIds.fromGuid ident

jumpToRHS ::
  (MonadA m, MonadA f) =>
  [E.ModKey] -> (String, Sugar.ExpressionN m) ->
  ExprGuiM f (Widget.EventHandlers (T f))
jumpToRHS keys (rhsDoc, rhs) = do
  savePos <- ExprGuiM.mkPrejumpPosSaver
  return $
    Widget.keysEventMapMovesCursor keys (E.Doc ["Navigation", "Jump to " ++ rhsDoc]) $
      rhsId <$ savePos
  where
    rhsId = WidgetIds.fromGuid $ rhs ^. Sugar.rGuid

-- exported for use in definition sugaring.
makeParamEdit ::
  MonadA m =>
  (Sugar.Name -> Widget (T m) -> Widget (T m)) ->
  Widget.Id -> Settings.InfoMode ->
  Widget.EventHandlers (T m) ->
  Sugar.FuncParam Sugar.Name m (Sugar.ExpressionN m) ->
  ExprGuiM m (ExpressionGui m)
makeParamEdit atParamWidgets prevId infoMode rhsJumper param =
  (Lens.mapped . ExpressionGui.egWidget %~ onFinalWidget) . assignCursor $ do
    paramTypeEdit <- ExprGuiM.makeSubexpresion $ param ^. Sugar.fpType
    paramNameEdit <- makeParamNameEdit name ident
    let typeWidget = paramTypeEdit ^. ExpressionGui.egWidget
    infoWidget <-
      case (infoMode, mActions) of
      (Settings.InfoExamples, Just actions) -> do
        exampleSugar <- ExprGuiM.liftMemoT $ Lens.view Sugar.fpGetExample actions
        exampleGui <-
          fmap (Lens.view ExpressionGui.egWidget) $
          ExprGuiM.makeSubexpresion exampleSugar
        return $ Box.vboxCentered [exampleGui, typeWidget]
      _ -> return typeWidget
    return .
      ExpressionGui.addType ExpressionGui.HorizLine myId [infoWidget] $
      ExpressionGui.fromValueWidget paramNameEdit
  where
    name = param ^. Sugar.fpName
    onFinalWidget =
      Widget.weakerEvents
      (rhsJumper `mappend` paramEventMap) .
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
      , paramDeleteEventMap Config.delBackwardKeys " backwards" (const prevId)
      , paramAddNextEventMap
      ]
    mActions = param ^. Sugar.fpMActions
    paramAddNextEventMap =
      maybe mempty
      (Widget.keysEventMapMovesCursor Config.addNextParamKeys (E.Doc ["Edit", "Add next parameter"]) .
       fmap (FocusDelegator.delegatingId . WidgetIds.fromGuid) .
       Lens.view (Sugar.fpListItemActions . Sugar.itemAddNext))
      mActions
    paramDeleteEventMap keys docSuffix onId =
      maybe mempty
      (Widget.keysEventMapMovesCursor keys (E.Doc ["Edit", "Delete parameter" ++ docSuffix]) .
       fmap (onId . WidgetIds.fromGuid) .
       Lens.view (Sugar.fpListItemActions . Sugar.itemDelete))
      mActions

makeResultEdit
  :: MonadA m
  => [Widget.Id]
  -> Sugar.ExpressionN m
  -> ExprGuiM m (ExpressionGui m)
makeResultEdit lhs result = do
  savePos <- ExprGuiM.mkPrejumpPosSaver
  let
    jumpToLhsEventMap =
      Widget.keysEventMapMovesCursor Config.jumpRHStoLHSKeys (E.Doc ["Navigation", "Jump to last param"]) $
        lastParam <$ savePos
  (Lens.over ExpressionGui.egWidget . Widget.weakerEvents) jumpToLhsEventMap <$>
    ExprGuiM.makeSubexpresion result
  where
    lastParam = case lhs of
      [] -> error "makeResultEdit given empty LHS"
      xs -> last xs

makeParamsAndResultEdit ::
  MonadA m =>
  (Sugar.Name -> Widget (T m) -> Widget (T m)) ->
  [Widget.Id] -> (String, Sugar.ExpressionN m) -> Widget.Id ->
  [Sugar.FuncParam Sugar.Name m (Sugar.ExpressionN m)] ->
  [Sugar.FuncParam Sugar.Name m (Sugar.ExpressionN m)] ->
  ExprGuiM m ([ExpressionGui m], [ExpressionGui m], ExpressionGui m)
makeParamsAndResultEdit atParamWidgets lhs rhs firstParId depParams params =
  makeNestedParams atParamWidgets rhs firstParId depParams params .
  makeResultEdit lhs $ snd rhs

makeNestedParams ::
  MonadA m =>
  (Sugar.Name -> Widget (T m) -> Widget (T m))
 -> (String, Sugar.ExpressionN m)
 -> Widget.Id
 -> [Sugar.FuncParam Sugar.Name m (Sugar.ExpressionN m)]
 -> [Sugar.FuncParam Sugar.Name m (Sugar.ExpressionN m)]
 -> ExprGuiM m a
 -> ExprGuiM m ([ExpressionGui m], [ExpressionGui m], a)
makeNestedParams atParamWidgets rhs prevId depParams params mkResultEdit = do
  infoMode <- fmap (Lens.view Settings.sInfoMode) ExprGuiM.readSettings
  rhsJumper <- jumpToRHS Config.jumpLHStoRHSKeys rhs
  let mkParam = makeParamEdit atParamWidgets prevId infoMode rhsJumper
  (,,)
    <$> traverse mkParam depParams
    <*> traverse mkParam params
    <*> mkResultEdit

make
  :: MonadA m
  => Sugar.HasParens
  -> Sugar.Func Sugar.Name m (Sugar.ExpressionN m)
  -> Widget.Id
  -> ExprGuiM m (ExpressionGui m)
make hasParens (Sugar.Func depParams params body) =
  ExpressionGui.wrapParenify hasParens Parens.addHighlightedTextParens $ \myId ->
  ExprGuiM.assignCursor myId bodyId $ do
    lambdaLabel <-
      ExpressionGui.makeColoredLabel Config.lambdaTextSize Config.lambdaColor "λ" myId
    rightArrowLabel <-
      ExpressionGui.makeColoredLabel Config.rightArrowTextSize Config.rightArrowColor "→" myId
    (depParamsEdits, paramsEdits, bodyEdit) <-
      makeParamsAndResultEdit (const id) lhs ("Func Body", body) myId depParams params
    return . ExpressionGui.hboxSpaced $
      concat
      [ [ExpressionGui.fromValueWidget lambdaLabel]
      , depParamsEdits
      , paramsEdits
      , [ ExpressionGui.fromValueWidget rightArrowLabel, bodyEdit ]
      ]
  where
    allParams = depParams ++ params
    bodyId = WidgetIds.fromGuid $ body ^. Sugar.rGuid
    lhs = map (WidgetIds.fromGuid . Lens.view Sugar.fpGuid) allParams
