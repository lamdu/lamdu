{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.LambdaEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.ModKey (ModKey(..))
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.WidgetsEnvT as WE
import qualified Graphics.UI.GLFW as GLFW
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

addScopeEdit :: Monad m => Maybe (ExpressionGui m) -> ExpressionGui m -> ExpressionGui m
addScopeEdit mScopeEdit e =
    e : (mScopeEdit ^.. Lens._Just)
    <&> ExpressionGui.egAlignment . _1 .~ 0.5
    & ExpressionGui.vboxTopFocal

mkLhsEdits :: Monad m => Maybe (ExpressionGui m) -> Maybe (ExpressionGui m) -> [ExpressionGui m]
mkLhsEdits mParamsEdit mScopeEdit =
    mParamsEdit <&> addScopeEdit mScopeEdit & (^.. Lens._Just)

mkExpanded :: Monad m => Maybe (ExpressionGui m) -> Maybe (ExpressionGui m) -> AnimId -> ExprGuiM m [ExpressionGui m]
mkExpanded mParamsEdit mScopeEdit animId =
    do
        labelEdit <- ExpressionGui.grammarLabel "→" animId
        lhsEdits ++ [labelEdit] & return
    where
        lhsEdits = mkLhsEdits mParamsEdit mScopeEdit

lamId :: Widget.Id -> Widget.Id
lamId = (`Widget.joinId` ["lam"])

mkShrunk ::
    Monad m => [Sugar.EntityId] -> Maybe (ExpressionGui m) -> Widget.Id ->
    ExprGuiM m [ExpressionGui m]
mkShrunk paramIds mScopeEdit myId =
    do
        config <- ExprGuiM.readConfig
        let expandEventMap =
                paramIds ^? Lens.traverse
                & maybe mempty
                  (Widget.keysEventMapMovesCursor (Config.jumpToDefinitionKeys config)
                   (E.Doc ["View", "Expand Lambda Params"]) . return .
                   WidgetIds.fromEntityId)
        ExpressionGui.makeFocusableView (lamId myId)
            <*> ExpressionGui.grammarLabel "λ" animId
            & LightLambda.withUnderline (Config.lightLambda config)
            -- TODO: add event to jump to first param
            <&> addScopeEdit mScopeEdit
            <&> ExpressionGui.egWidget %~ Widget.weakerEvents expandEventMap
            <&> (:[])
    where
        animId = Widget.toAnimId myId

mkLightLambda ::
    Monad n =>
    Maybe (ExpressionGui n) ->
    Maybe (ExpressionGui n) ->
    Sugar.BinderParams a m -> Widget.Id ->
    ExprGuiM n [ExpressionGui n]
mkLightLambda mParamsEdit mScopeEdit params myId =
    do
        isSelected <-
            mapM (WE.isSubCursor . WidgetIds.fromEntityId) paramIds
            <&> or
            & ExprGuiM.widgetEnv
        let shrinkKeys = [ModKey mempty GLFW.Key'Escape]
        let shrinkEventMap =
                Widget.keysEventMapMovesCursor shrinkKeys
                (E.Doc ["View", "Shrink Lambda Params"]) (return (lamId myId))
        if isSelected
            then mkExpanded mParamsEdit mScopeEdit animId
                 <&> Lens.mapped . ExpressionGui.egWidget %~
                     Widget.weakerEvents shrinkEventMap
            else mkShrunk paramIds mScopeEdit myId
    where
        paramIds = params ^.. SugarLens.binderNamedParams . Sugar.fpId
        animId = Widget.toAnimId myId

make ::
    Monad m =>
    Sugar.Lambda (Name m) m (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
make lam pl =
    ExprGuiM.withLocalPrecedence (ExpressionGui.precLeft .~ 0) $
    ExpressionGui.stdWrapParenify pl (ExpressionGui.MyPrecedence 0) $ \myId ->
    ExprGuiM.assignCursor myId bodyId $
    do
        BinderEdit.Parts mParamsEdit mScopeEdit bodyEdit eventMap <-
            BinderEdit.makeParts funcApplyLimit binder bodyId myId
        let animId = Widget.toAnimId myId
        paramsAndLabelEdits <-
            case (lam ^. Sugar.lamMode, params) of
            (_, Sugar.NullParam{}) -> mkLhsEdits mParamsEdit mScopeEdit & return
            (Sugar.LightLambda, _) -> mkLightLambda mParamsEdit mScopeEdit params myId
            _ -> mkExpanded mParamsEdit mScopeEdit animId
        paramsAndLabelEdits ++ [bodyEdit]
            & ExpressionGui.hboxSpaced
            <&> ExpressionGui.egWidget %~ Widget.weakerEvents eventMap
    where
        funcApplyLimit = pl ^. Sugar.plData . ExprGuiT.plShowAnnotation . ExprGuiT.funcApplyLimit
        params = binder ^. Sugar.bParams
        binder = lam ^. Sugar.lamBinder
        bodyId =
            binder ^. Sugar.bBody . Sugar.bbContent . SugarLens.binderContentEntityId
            & WidgetIds.fromEntityId
