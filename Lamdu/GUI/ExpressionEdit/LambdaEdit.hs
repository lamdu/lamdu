{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.LambdaEdit
  ( make, makeParamNameEdit, makeParamEdit
  ) where

import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Monoid (Monoid(..))
import Lamdu.GUI.ExpressionGui (ExpressionGui)
import Lamdu.GUI.ExpressionGui.Monad (ExprGuiM, WidgetT)
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionEdit.Parens as Parens
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

paramFDConfig :: FocusDelegator.Config
paramFDConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKeys = [E.ModKey E.noMods E.Key'Enter]
  , FocusDelegator.startDelegatingDoc = E.Doc ["Edit", "Rename parameter"]
  , FocusDelegator.stopDelegatingKeys = [E.ModKey E.noMods E.Key'Escape]
  , FocusDelegator.stopDelegatingDoc = E.Doc ["Edit", "Done renaming"]
  }

makeParamNameEdit ::
  MonadA m => Sugar.NameProperty Sugar.Name m -> Widget.Id ->
  ExprGuiM m (WidgetT m)
makeParamNameEdit nameProp myId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  ExprGuiM.wrapDelegated paramFDConfig FocusDelegator.NotDelegating id
    (ExprGuiM.withFgColor (Config.paramOriginColor config) .
     ExpressionGui.makeNameEdit nameProp) myId

-- exported for use in definition sugaring.
makeParamEdit ::
  MonadA m => Widget.Id ->
  Sugar.FuncParam Sugar.Name m ->
  ExprGuiM m (ExpressionGui m)
makeParamEdit prevId param =
  do
    -- paramTypeEdit <- ExprGuiM.makeSubexpression 0 $ param ^. Sugar.fpType
    paramNameEdit <- makeParamNameEdit (param ^. Sugar.fpName) myId
    config <- ExprGuiM.widgetEnv WE.readConfig
    let
      paramAddNextEventMap =
        maybe mempty
        (Widget.keysEventMapMovesCursor (Config.addNextParamKeys config)
         (E.Doc ["Edit", "Add next parameter"]) .
         fmap (FocusDelegator.delegatingId . WidgetIds.fromEntityId) .
         (^. Sugar.fpListItemActions . Sugar.itemAddNext))
        mActions
      paramEventMap = mconcat
        [ paramDeleteEventMap (Config.delForwardKeys config) "" id
        , paramDeleteEventMap (Config.delBackwardKeys config) " backwards" (const prevId)
        , paramAddNextEventMap
        ]
    return .
      (ExpressionGui.egWidget %~ Widget.weakerEvents paramEventMap) {- .
      ExpressionGui.addType config ExpressionGui.HorizLine myId
      [paramTypeEdit ^. ExpressionGui.egWidget] -} $
      ExpressionGui.fromValueWidget paramNameEdit
  where
    myId = WidgetIds.fromEntityId $ param ^. Sugar.fpId
    mActions = param ^. Sugar.fpMActions
    paramDeleteEventMap keys docSuffix onId =
      maybe mempty
      (Widget.keysEventMapMovesCursor keys (E.Doc ["Edit", "Delete parameter" ++ docSuffix]) .
       fmap (onId . WidgetIds.fromEntityId) .
       (^. Sugar.fpListItemActions . Sugar.itemDelete))
      mActions

make ::
  MonadA m =>
  ExpressionGui.ParentPrecedence ->
  Sugar.Lam Sugar.Name m (ExprGuiM.SugarExpr m) ->
  Sugar.Payload m ExprGuiM.Payload ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
make parentPrecedence (Sugar.Lam param body) pl =
  ExpressionGui.stdWrapParenify pl parentPrecedence (ExpressionGui.MyPrecedence 0)
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
    bodyId = WidgetIds.fromEntityId $ body ^. Sugar.rPayload . Sugar.plEntityId
