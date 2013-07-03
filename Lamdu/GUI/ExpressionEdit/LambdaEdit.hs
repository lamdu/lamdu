{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.LambdaEdit
  ( make, makeParamNameEdit, makeParamEdit
  ) where

import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Lamdu.GUI.ExpressionGui (ExpressionGui)
import Lamdu.GUI.ExpressionGui.Monad (ExprGuiM, WidgetT)
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionEdit.Parens as Parens
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

paramFDConfig :: FocusDelegator.Config
paramFDConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKeys = [E.ModKey E.noMods E.KeyEnter]
  , FocusDelegator.startDelegatingDoc = E.Doc ["Edit", "Rename parameter"]
  , FocusDelegator.stopDelegatingKeys = [E.ModKey E.noMods E.KeyEsc]
  , FocusDelegator.stopDelegatingDoc = E.Doc ["Edit", "Done renaming"]
  }

makeParamNameEdit ::
  MonadA m =>
  Sugar.Name -> Guid -> Widget.Id ->
  ExprGuiM m (WidgetT m)
makeParamNameEdit name ident myId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  ExprGuiM.wrapDelegated paramFDConfig FocusDelegator.NotDelegating id
    (ExprGuiM.withFgColor (Config.paramOriginColor config) .
     ExpressionGui.makeNameEdit name ident) myId

compose :: [a -> a] -> a -> a
compose = foldr (.) id

-- exported for use in definition sugaring.
makeParamEdit ::
  MonadA m =>
  Widget.Id ->
  Sugar.FuncParam Sugar.Name m (ExprGuiM.SugarExpr m) ->
  ExprGuiM m (ExpressionGui m)
makeParamEdit prevId param =
  assignCursor $ do
    paramTypeEdit <- ExprGuiM.makeSubexpression 0 $ param ^. Sugar.fpType
    paramNameEdit <- makeParamNameEdit name (param ^. Sugar.fpGuid) myId
    config <- ExprGuiM.widgetEnv WE.readConfig
    let
      paramAddNextEventMap =
        maybe mempty
        (Widget.keysEventMapMovesCursor (Config.addNextParamKeys config)
         (E.Doc ["Edit", "Add next parameter"]) .
         fmap (FocusDelegator.delegatingId . WidgetIds.fromGuid) .
         (^. Sugar.fpListItemActions . Sugar.itemAddNext))
        mActions
      paramEventMap = mconcat
        [ paramDeleteEventMap (Config.delForwardKeys config) "" id
        , paramDeleteEventMap (Config.delBackwardKeys config) " backwards" (const prevId)
        , paramAddNextEventMap
        ]
    return .
      (ExpressionGui.egWidget %~ Widget.weakerEvents paramEventMap) .
      ExpressionGui.addType config ExpressionGui.HorizLine myId
      [paramTypeEdit ^. ExpressionGui.egWidget] $
      ExpressionGui.fromValueWidget paramNameEdit
  where
    name = param ^. Sugar.fpName
    assignGuidToMe = (`ExprGuiM.assignCursor` myId) . WidgetIds.fromGuid
    assignCursor = compose . map assignGuidToMe $ param ^. Sugar.fpAltIds
    myId = WidgetIds.fromGuid $ param ^. Sugar.fpId
    mActions = param ^. Sugar.fpMActions
    paramDeleteEventMap keys docSuffix onId =
      maybe mempty
      (Widget.keysEventMapMovesCursor keys (E.Doc ["Edit", "Delete parameter" ++ docSuffix]) .
       fmap (onId . WidgetIds.fromGuid) .
       (^. Sugar.fpListItemActions . Sugar.itemDelete))
      mActions

make ::
  MonadA m =>
  ExpressionGui.ParentPrecedence ->
  Sugar.Lam Sugar.Name m (ExprGuiM.SugarExpr m) ->
  Sugar.Payload Sugar.Name m a ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
make parentPrecedence (Sugar.Lam _ param _ body) pl =
  ExpressionGui.wrapParenify pl parentPrecedence (ExpressionGui.MyPrecedence 0)
  Parens.addHighlightedTextParens $ \myId ->
  ExprGuiM.assignCursor myId bodyId $ do
    config <- ExprGuiM.widgetEnv WE.readConfig
    lambdaLabel <-
      ExpressionGui.makeColoredLabel
      (Config.lambdaTextSize config)
      (Config.lambdaColor config) "λ" myId
    paramEdit <- makeParamEdit bodyId param
    dotLabel <-
      ExpressionGui.makeColoredLabel
      (Config.rightArrowTextSize config)
      (Config.rightArrowColor config) ". " myId
    bodyEdit <- ExprGuiM.makeSubexpression 0 body
    return $ ExpressionGui.hbox
      [ ExpressionGui.fromValueWidget lambdaLabel
      , paramEdit
      , ExpressionGui.fromValueWidget dotLabel
      , bodyEdit
      ]
  where
    bodyId = WidgetIds.fromGuid $ body ^. Sugar.rPayload . Sugar.plGuid
