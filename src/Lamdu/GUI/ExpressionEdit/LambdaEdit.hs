{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.LambdaEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import           Data.Store.Transaction (Transaction)
import           GUI.Momentu.Align (WithTextPos(..))
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/-/))
import           GUI.Momentu.MetaKey (MetaKey(..), noMods)
import qualified GUI.Momentu.MetaKey as MetaKey
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Expression as ResponsiveExpr
import qualified GUI.Momentu.Responsive.Options as Options
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Lamdu.Config (HasConfig)
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (HasTheme)
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
addScopeEdit mScopeEdit = (/-/ maybe Element.empty (WithTextPos 0) mScopeEdit)

mkLhsEdits ::
    Functor m =>
    Maybe (ExpressionGui m) ->
    Maybe (Widget (T m Widget.EventResult)) -> [ExpressionGui m]
mkLhsEdits mParamsEdit mScopeEdit =
    mParamsEdit <&> addScopeEdit mScopeEdit & (^.. Lens._Just)

mkExpanded ::
    ( Monad m, MonadReader env f, HasTheme env, TextView.HasStyle env
    , Element.HasAnimIdPrefix env
    ) =>
    f (Maybe (ExpressionGui m) -> Maybe (Widget (T m Widget.EventResult)) ->
     [ExpressionGui m])
mkExpanded =
    do
        labelEdit <- ExpressionGui.grammarLabel "→" <&> Responsive.fromTextView
        return $ \mParamsEdit mScopeEdit ->
            mkLhsEdits mParamsEdit mScopeEdit ++ [labelEdit]

lamId :: Widget.Id -> Widget.Id
lamId = (`Widget.joinId` ["lam"])

mkShrunk ::
    ( Monad m, MonadReader env f, HasConfig env, HasTheme env
    , Widget.HasCursor env, Element.HasAnimIdPrefix env, TextView.HasStyle env
    ) => [Sugar.EntityId] -> Widget.Id ->
    f (Maybe (Widget (T m Widget.EventResult)) -> [ExpressionGui m])
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
            (Widget.makeFocusableView ?? lamId myId <&> (Align.tValue %~))
            <*> ExpressionGui.grammarLabel "λ"
            <&> Responsive.fromWithTextPos
            & LightLambda.withUnderline theme
        return $ \mScopeEdit ->
            [ addScopeEdit mScopeEdit lamLabel
              & E.weakerEvents expandEventMap
            ]

mkLightLambda ::
    ( Monad n, MonadReader env f, Widget.HasCursor env
    , Element.HasAnimIdPrefix env, TextView.HasStyle env, HasTheme env
    , HasConfig env
    ) =>
    Sugar.BinderParams a m -> Widget.Id ->
    f
    (Maybe (ExpressionGui n) -> Maybe (Widget (T n Widget.EventResult)) ->
     [ExpressionGui n])
mkLightLambda params myId =
    do
        isSelected <-
            paramIds <&> WidgetIds.fromEntityId
            & traverse (Widget.isSubCursor ??)
            <&> or
        let shrinkKeys = [MetaKey noMods MetaKey.Key'Escape]
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
        paramIds =
            case params of
            Sugar.BinderWithoutParams -> []
            Sugar.NullParam{} -> []
            Sugar.VarParam p -> [p ^. Sugar.fpInfo . Sugar.vpiId]
            Sugar.FieldParams ps -> ps <&> (^. Sugar.fpInfo . Sugar.fpiTag . Sugar.tagInfo . Sugar.tagInstance)

make ::
    Monad m =>
    Sugar.Lambda (Name m) m (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
make lam pl =
    do
        BinderEdit.Parts mParamsEdit mScopeEdit bodyEdit eventMap <-
            BinderEdit.makeParts funcApplyLimit binder (WidgetIds.fromEntityId bodyId) myId
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
        (ResponsiveExpr.boxSpacedMDisamb ?? mParensId)
            <*> (Options.boxSpaced ?? Options.disambiguationNone ?? paramsAndLabelEdits
                <&> (: [bodyEdit]))
            <&> E.weakerEvents eventMap
    & ExpressionGui.stdWrapParentExpr pl bodyId
    & ExprGuiM.withLocalPrecedence 0 (ExpressionGui.before .~ 0)
    where
        myId = WidgetIds.fromExprPayload pl
        funcApplyLimit = pl ^. Sugar.plData . ExprGuiT.plShowAnnotation . ExprGuiT.funcApplyLimit
        params = binder ^. Sugar.bParams
        binder = lam ^. Sugar.lamBinder
        bodyId = binder ^. Sugar.bBody . Sugar.bbContent . SugarLens.binderContentEntityId
