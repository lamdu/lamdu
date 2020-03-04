module Lamdu.GUI.Expr.ApplyEdit
    ( makeSimple, makeLabeled
    ) where

import           Control.Lens (Const)
import qualified Control.Lens as Lens
import           GUI.Momentu.Glue ((/|/))
import qualified GUI.Momentu.Glue as Glue
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Expression as ResponsiveExpr
import qualified GUI.Momentu.Responsive.Options as Options
import           GUI.Momentu.Responsive.TaggedList (TaggedItem(..), taggedList)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Grid as Grid
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified Lamdu.GUI.Expr.EventMap as ExprEventMap
import qualified Lamdu.GUI.Expr.GetVarEdit as GetVarEdit
import qualified Lamdu.GUI.Expr.TagEdit as TagEdit
import           Lamdu.GUI.ExpressionGui.Annotation (maybeAddAnnotationPl)
import           Lamdu.GUI.ExpressionGui.Monad (GuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as GuiM
import qualified Lamdu.GUI.ExpressionGui.Payload as ExprGui
import           Lamdu.GUI.ExpressionGui.Wrap (stdWrap, stdWrapParentExpr)
import qualified Lamdu.GUI.ExpressionGui.Wrap as Wrap
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.Definitions as Texts
import qualified Lamdu.I18N.Name as Texts
import qualified Lamdu.I18N.Navigation as Texts
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

makeFunc ::
    ( Monad i, Monad o
    , Grid.HasTexts env
    , Has (Texts.Name Text) env
    , Has (Texts.Code Text) env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Definitions Text) env
    , Has (Texts.Navigation Text) env
    ) =>
    GetVarEdit.Role ->
    Annotated (Sugar.Payload Name i o ExprGui.Payload) # Const (Sugar.BinderVarRef Name o) ->
    GuiM env i o (Responsive o)
makeFunc role func =
    GetVarEdit.makeGetBinder role (func ^. hVal . Lens._Wrapped) myId
    <&> Responsive.fromWithTextPos
    & stdWrap pl
    where
        pl = func ^. annotation
        myId = WidgetIds.fromExprPayload pl

makeLabeled ::
    ( Monad i, Monad o
    , Grid.HasTexts env
    , Has (Texts.Code Text) env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Definitions Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Navigation Text) env
    ) =>
    Sugar.Expr (Sugar.LabeledApply (Sugar.EvaluationScopes Name i)) Name i o ExprGui.Payload ->
    GuiM env i o (Responsive o)
makeLabeled (Ann (Const pl) apply) =
    ExprEventMap.add ExprEventMap.defaultOptions pl <*>
    ( Wrap.parentDelegator (WidgetIds.fromExprPayload pl) <*>
        case apply ^. Sugar.aSpecialArgs of
        Sugar.Verbose -> makeFunc GetVarEdit.Normal func >>= wrap
        Sugar.Operator l r ->
            (ResponsiveExpr.boxSpacedMDisamb ?? ExprGui.mParensId pl)
            <*> sequenceA
            [ GuiM.makeSubexpression l
            , (Options.boxSpaced ?? Options.disambiguationNone)
                <*> sequenceA
                [ makeFunc GetVarEdit.Operator func
                , GuiM.makeSubexpression r
                ]
                >>= wrap
            ]
    )
    where
        wrap x =
            (maybeAddAnnotationPl pl <&> (Widget.widget %~)) <*>
            addArgs apply x
        func = apply ^. Sugar.aFunc

makeArgRow ::
    (Monad i, Glue.HasTexts env, Has (Texts.Name Text) env) =>
    Sugar.Body (Sugar.AnnotatedArg (Sugar.EvaluationScopes Name i)) Name i o ExprGui.Payload ->
    GuiM env i o (TaggedItem o)
makeArgRow arg =
    do
        expr <- GuiM.makeSubexpression (arg ^. Sugar.aaExpr)
        pre <-
            TagEdit.makeArgTag (arg ^. Sugar.aaTag . Sugar.tagName)
            (arg ^. Sugar.aaTag . Sugar.tagInstance)
            /|/ Spacer.stdHSpace
        pure TaggedItem
            { _tagPre = pre <&> Widget.fromView & Just
            , _taggedItem = expr
            , _tagPost = Nothing
            }

addArgs ::
    ( Monad i, Monad o
    , Glue.HasTexts env
    , Has (Texts.Definitions Text) env, Has (Texts.Navigation Text) env
    , Has (Texts.Code Text) env, Has (Texts.CodeUI Text) env
    , Has (Texts.Name Text) env, Grid.HasTexts env
    ) =>
    Sugar.Body (Sugar.LabeledApply (Sugar.EvaluationScopes Name i)) Name i o ExprGui.Payload ->
    Responsive o -> GuiM env i o (Responsive o)
addArgs apply funcRow =
    do
        argRows <-
            case apply ^. Sugar.aAnnotatedArgs of
            [] -> pure []
            xs -> taggedList <*> traverse makeArgRow xs <&> (:[])
        punnedArgs <-
            case apply ^. Sugar.aPunnedArgs of
            [] -> pure []
            args -> GetVarEdit.makePunnedVars args <&> (:[])
        let extraRows = argRows ++ punnedArgs
        if null extraRows
            then pure funcRow
            else
                Styled.addValFrame
                <*> (Responsive.vboxSpaced ?? (funcRow : extraRows))

makeSimple ::
    ( Monad i, Monad o
    , Grid.HasTexts env
    , Has (Texts.Code Text) env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Definitions Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Navigation Text) env
    ) =>
    Annotated (Sugar.Payload Name i o ExprGui.Payload) #
        Sugar.App (Sugar.Term (Sugar.EvaluationScopes Name i) Name i o) ->
    GuiM env i o (Responsive o)
makeSimple (Ann (Const pl) (Sugar.App func arg)) =
    (ResponsiveExpr.boxSpacedMDisamb ?? ExprGui.mParensId pl)
    <*> sequenceA
    [ GuiM.makeSubexpression func
    , GuiM.makeSubexpression arg
    ] & stdWrapParentExpr pl
