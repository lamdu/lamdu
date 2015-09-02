{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.LambdaEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.MonadA (MonadA)
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.WidgetsEnvT as WE
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionEdit.BinderEdit as BinderEdit
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.LightLambda as LightLambda
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.Names.Types (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

addScopeEdit :: MonadA m => Maybe (ExpressionGui m) -> ExpressionGui m -> ExprGuiM m (ExpressionGui m)
addScopeEdit mScopeEdit e =
    e : (mScopeEdit ^.. Lens._Just)
    <&> ExpressionGui.egAlignment . _1 .~ 0.5
    & ExpressionGui.vboxTopFocalSpaced

mkLhsEdits :: MonadA m => Maybe (ExpressionGui m) -> Maybe (ExpressionGui m) -> ExprGuiM m [ExpressionGui m]
mkLhsEdits mParamsEdit mScopeEdit =
    Lens._Just (addScopeEdit mScopeEdit) mParamsEdit
    <&> (^.. Lens._Just)

mkExpanded :: MonadA m => Maybe (ExpressionGui m) -> Maybe (ExpressionGui m) -> AnimId -> ExprGuiM m [ExpressionGui m]
mkExpanded mParamsEdit mScopeEdit animId =
    do
        lhsEdits <- mkLhsEdits mParamsEdit mScopeEdit
        labelEdit <- ExpressionGui.grammarLabel "→" animId
        lhsEdits ++ [labelEdit] & return

mkLightLambda ::
    MonadA n =>
    Maybe (ExpressionGui n) ->
    Maybe (ExpressionGui n) ->
    Sugar.BinderParams a m -> Widget.Id ->
    ExprGuiM n [ExpressionGui n]
mkLightLambda mParamsEdit mScopeEdit params myId =
    do
        let paramIds =
                params ^.. SugarLens.binderNamedParams . Sugar.fpId
        isSelected <-
            mapM (WE.isSubCursor . WidgetIds.fromEntityId) paramIds
            <&> or
            & ExprGuiM.widgetEnv
        config <- ExprGuiM.readConfig <&> Config.lightLambda
        if isSelected
            then mkExpanded mParamsEdit mScopeEdit animId
            else
                ExpressionGui.grammarLabel "λ" animId
                & LightLambda.withUnderline config
                >>= ExpressionGui.makeFocusableView
                    (Widget.joinId myId ["lam"])
                -- TODO: add event to jump to first param
                >>= addScopeEdit mScopeEdit
                <&> (:[])
    where
        animId = Widget.toAnimId myId

make ::
    MonadA m =>
    Sugar.Lambda (Name m) m (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
make lam pl =
    ExprGuiM.withLocalPrecedence (ExpressionGui.precLeft .~ 0) $
    ExpressionGui.stdWrapParenify pl (ExpressionGui.MyPrecedence 0) $ \myId ->
    ExprGuiM.assignCursor myId bodyId $
    do
        BinderEdit.Parts mParamsEdit mScopeEdit bodyEdit eventMap <-
            BinderEdit.makeParts binder bodyId myId
        let animId = Widget.toAnimId myId
        paramsAndLabelEdits <-
            case (lam ^. Sugar.lamMode, params) of
            (_, Sugar.NullParam{}) -> mkLhsEdits mParamsEdit mScopeEdit
            (Sugar.LightLambda, _) -> mkLightLambda mParamsEdit mScopeEdit params myId
            _ -> mkExpanded mParamsEdit mScopeEdit animId
        paramsAndLabelEdits ++ [bodyEdit]
            & ExpressionGui.hboxSpaced
            <&> ExpressionGui.egWidget %~ Widget.weakerEvents eventMap
    where
        params = binder ^. Sugar.bParams
        binder = lam ^. Sugar.lamBinder
        body = binder ^. Sugar.bBody
        bodyId = WidgetIds.fromExprPayload $ body ^. Sugar.rPayload
