{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.LambdaEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.MonadA (MonadA)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.WidgetsEnvT as WE
import qualified Lamdu.GUI.ExpressionEdit.BinderEdit as BinderEdit
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.Names.Types (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

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
        let addScopeEdit e =
                e : (mScopeEdit ^.. Lens._Just)
                <&> ExpressionGui.egAlignment . _1 .~ 0.5
                & ExpressionGui.vboxTopFocalSpaced
        let mkLhsEdits =
                Lens._Just addScopeEdit mParamsEdit
                <&> (^.. Lens._Just)
        let mkExpanded =
                do
                    lhsEdits <- mkLhsEdits
                    labelEdit <- ExpressionGui.grammarLabel "→" animId
                    lhsEdits ++ [labelEdit] & return
        paramsAndLabelEdits <-
            case (lam ^. Sugar.lamMode, params) of
            (_, Sugar.NullParam{}) -> mkLhsEdits
            (Sugar.LightLambda, _) ->
                do
                    let paramIds =
                            params ^.. SugarLens.binderNamedParams . Sugar.fpId
                    isSelected <-
                        mapM (WE.isSubCursor . WidgetIds.fromEntityId) paramIds
                        <&> or
                        & ExprGuiM.widgetEnv
                    if isSelected
                        then mkExpanded
                        else
                            ExpressionGui.grammarLabel "λ" animId
                            >>= ExpressionGui.makeFocusableView
                                (Widget.joinId myId ["lam"])
                            -- TODO: add event to jump to first param
                            >>= addScopeEdit
                            <&> (:[])
            _ -> mkExpanded
        paramsAndLabelEdits ++ [bodyEdit]
            & ExpressionGui.hboxSpaced
            <&> ExpressionGui.egWidget %~ Widget.weakerEvents eventMap
    where
        params = binder ^. Sugar.bParams
        binder = lam ^. Sugar.lamBinder
        body = binder ^. Sugar.bBody
        bodyId = WidgetIds.fromExprPayload $ body ^. Sugar.rPayload
