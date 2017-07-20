{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.LambdaEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import           Data.Store.Transaction (Transaction)
import           Graphics.UI.Bottle.Align (WithTextPos(..))
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.MetaKey (MetaKey(..), noMods)
import           Graphics.UI.Bottle.View ((/-/))
import qualified Graphics.UI.Bottle.View as View
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widget.TreeLayout as TreeLayout
import qualified Graphics.UI.GLFW as GLFW
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.GUI.ExpressionEdit.BinderEdit as BinderEdit
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.LightLambda as LightLambda
import qualified Lamdu.GUI.Precedence as Prec
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.Names.Types (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction

addScopeEdit ::
    Functor m =>
    Maybe (Widget (T m Widget.EventResult)) -> ExpressionGui m ->
    ExpressionGui m
addScopeEdit mScopeEdit = (/-/ maybe View.empty (WithTextPos 0) mScopeEdit)

mkLhsEdits ::
    Functor m =>
    Maybe (ExpressionGui m) ->
    Maybe (Widget (T m Widget.EventResult)) -> [ExpressionGui m]
mkLhsEdits mParamsEdit mScopeEdit =
    mParamsEdit <&> addScopeEdit mScopeEdit & (^.. Lens._Just)

mkExpanded ::
    Monad m =>
    ExprGuiM m
    (Maybe (ExpressionGui m) -> Maybe (Widget (T m Widget.EventResult)) ->
     [ExpressionGui m])
mkExpanded =
    do
        labelEdit <- ExpressionGui.grammarLabel "→" <&> TreeLayout.fromTextView
        return $ \mParamsEdit mScopeEdit ->
            mkLhsEdits mParamsEdit mScopeEdit ++ [labelEdit]

lamId :: Widget.Id -> Widget.Id
lamId = (`Widget.joinId` ["lam"])

mkShrunk ::
    Monad m => [Sugar.EntityId] -> Widget.Id ->
    ExprGuiM m (Maybe (Widget (T m Widget.EventResult)) -> [ExpressionGui m])
mkShrunk paramIds myId =
    do
        jumpKeys <- Lens.view Config.config <&> Config.jumpToDefinitionKeys
        let expandEventMap =
                paramIds ^? Lens.traverse
                & maybe mempty
                  (Widget.keysEventMapMovesCursor jumpKeys
                   (E.Doc ["View", "Expand Lambda Params"]) . return .
                   WidgetIds.fromEntityId)
        theme <- Lens.view Theme.theme
        lamLabel <-
            (Widget.makeFocusableView ?? lamId myId)
            <*> (ExpressionGui.grammarLabel "λ" <&> TreeLayout.fromTextView)
            & LightLambda.withUnderline theme
        return $ \mScopeEdit ->
            [ addScopeEdit mScopeEdit lamLabel
              & E.weakerEvents expandEventMap
            ]

mkLightLambda ::
    Monad n =>
    Sugar.BinderParams a m -> Widget.Id ->
    ExprGuiM n
    (Maybe (ExpressionGui n) -> Maybe (Widget (T n Widget.EventResult)) ->
     [ExpressionGui n])
mkLightLambda params myId =
    do
        isSelected <-
            paramIds <&> WidgetIds.fromEntityId
            & traverse (Widget.isSubCursor ??)
            <&> or
        let shrinkKeys = [MetaKey noMods GLFW.Key'Escape]
        let shrinkEventMap =
                Widget.keysEventMapMovesCursor shrinkKeys
                (E.Doc ["View", "Shrink Lambda Params"]) (return (lamId myId))
        if isSelected
            then
                 mkExpanded
                 <&> Lens.mapped . Lens.mapped . Lens.mapped %~
                     E.weakerEvents shrinkEventMap
            else mkShrunk paramIds myId
                 <&> \mk _mParamsEdit mScopeEdit -> mk mScopeEdit
    where
        paramIds = params ^.. SugarLens.binderNamedParams . Sugar.fpId

make ::
    Monad m =>
    Sugar.Lambda (Name m) m (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
make lam pl =
    do
        BinderEdit.Parts mParamsEdit mScopeEdit bodyEdit eventMap <-
            BinderEdit.makeParts funcApplyLimit binder bodyId myId
        let animId = Widget.toAnimId myId
        paramsAndLabelEdits <-
            case (lam ^. Sugar.lamMode, params) of
            (_, Sugar.NullParam{}) -> mkLhsEdits mParamsEdit mScopeEdit & return
            (Sugar.LightLambda, _) -> mkLightLambda params myId ?? mParamsEdit ?? mScopeEdit
            _ -> mkExpanded ?? mParamsEdit ?? mScopeEdit
        parentPrec <- ExprGuiM.outerPrecedence <&> Prec.ParentPrecedence
        let mParensId
                | Prec.needParens parentPrec (Prec.my 0) = Just animId
                | otherwise = Nothing
        ExpressionGui.combineSpacedMParens mParensId
            <*> (ExpressionGui.combineSpaced ?? paramsAndLabelEdits
                <&> (: [bodyEdit]))
            <&> E.weakerEvents eventMap
    & Widget.assignCursor myId bodyId
    & ExpressionGui.stdWrapParentExpr pl
    & ExprGuiM.withLocalPrecedence 0 (ExpressionGui.before .~ 0)
    where
        myId = WidgetIds.fromExprPayload pl
        funcApplyLimit = pl ^. Sugar.plData . ExprGuiT.plShowAnnotation . ExprGuiT.funcApplyLimit
        params = binder ^. Sugar.bParams
        binder = lam ^. Sugar.lamBinder
        bodyId =
            binder ^. Sugar.bBody . Sugar.bbContent . SugarLens.binderContentEntityId
            & WidgetIds.fromEntityId
