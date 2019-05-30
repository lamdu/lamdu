{-# LANGUAGE DisambiguateRecordFields #-}
module Lamdu.GUI.Expr.ApplyEdit
    ( makeSimple, makeLabeled
    ) where

import           AST (Tree)
import           AST.Knot.Ann (Ann(..), ann, val)
import           Control.Lens (Const)
import qualified Control.Lens as Lens
import           GUI.Momentu.Animation (AnimId)
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.Glue ((/|/))
import qualified GUI.Momentu.Glue as Glue
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Expression as ResponsiveExpr
import qualified GUI.Momentu.Responsive.Options as Options
import           GUI.Momentu.State (Gui)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Grid as Grid
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified Lamdu.GUI.Expr.GetVarEdit as GetVarEdit
import qualified Lamdu.GUI.Expr.TagEdit as TagEdit
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Payload as ExprGui
import           Lamdu.GUI.ExpressionGui.Wrap (stdWrap, stdWrapParentExpr)
import           Lamdu.GUI.Styled (label, grammar)
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
    Tree (Ann (Sugar.Payload (Name o) i o ExprGui.Payload))
        (Const (Sugar.BinderVarRef (Name o) o)) ->
    ExprGuiM env i o (Gui Responsive o)
makeFunc role func =
    stdWrap pl <*>
    ( GetVarEdit.makeGetBinder role (func ^. val . Lens._Wrapped) myId
        <&> Responsive.fromWithTextPos
    )
    where
        pl = func ^. ann
        myId = WidgetIds.fromExprPayload pl

isBoxed :: Sugar.LabeledApply name i o a -> Bool
isBoxed apply =
    Lens.has (Sugar.aAnnotatedArgs . traverse) apply
    || Lens.has (Sugar.aRelayedArgs . traverse) apply

makeFuncRow ::
    ( Monad i, Monad o
    , Grid.HasTexts env
    , Has (Texts.Name Text) env
    , Has (Texts.Code Text) env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Definitions Text) env
    , Has (Texts.Navigation Text) env
    ) =>
    Maybe AnimId ->
    Tree (Sugar.LabeledApply (Name o) i o)
        (Ann (Sugar.Payload (Name o) i o ExprGui.Payload)) ->
    ExprGuiM env i o (Gui Responsive o)
makeFuncRow mParensId apply =
    case apply ^. Sugar.aSpecialArgs of
    Sugar.Verbose -> makeFunc GetVarEdit.Normal func
    Sugar.Object arg ->
        (ResponsiveExpr.boxSpacedMDisamb ?? mParensId)
        <*> sequenceA
        [ makeFunc GetVarEdit.Normal func
        , ExprGuiM.makeSubexpression arg
        ]
    Sugar.Infix l r ->
        (ResponsiveExpr.boxSpacedMDisamb ?? mParensId)
        <*> sequenceA
        [ (Options.boxSpaced ?? Options.disambiguationNone)
            <*> sequenceA
            [ ExprGuiM.makeSubexpression l
            , makeFunc GetVarEdit.Infix func
            ]
        , ExprGuiM.makeSubexpression r
        ]
    where
        func = apply ^. Sugar.aFunc

makeLabeled ::
    ( Monad i, Monad o
    , Grid.HasTexts env
    , Has (Texts.Code Text) env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Definitions Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Navigation Text) env
    ) =>
    Tree (Sugar.LabeledApply (Name o) i o)
        (Ann (Sugar.Payload (Name o) i o ExprGui.Payload)) ->
    Sugar.Payload (Name o) i o ExprGui.Payload ->
    ExprGuiM env i o (Gui Responsive o)
makeLabeled apply pl =
    stdWrapParentExpr pl
    <*> (makeFuncRow (ExprGui.mParensId pl) apply >>= addBox)
    where
        addBox
            | isBoxed apply = mkBoxed apply
            | otherwise = pure

makeArgRow ::
    ( Monad i, Monad o, Glue.HasTexts env, Has (Texts.Name Text) env ) =>
    Sugar.AnnotatedArg (Name o) (ExprGui.SugarExpr i o) ->
    ExprGuiM env i o (Gui Responsive.TaggedItem o)
makeArgRow arg =
    do
        expr <- ExprGuiM.makeSubexpression (arg ^. Sugar.aaExpr)
        pre <-
            TagEdit.makeArgTag (arg ^. Sugar.aaTag . Sugar.tagName)
            (arg ^. Sugar.aaTag . Sugar.tagInstance)
            /|/ Spacer.stdHSpace
        pure Responsive.TaggedItem
            { Responsive._tagPre = pre <&> Widget.fromView
            , Responsive._taggedItem = expr
            , Responsive._tagPost = Element.empty
            }

mkRelayedArgs ::
    ( Monad i, Monad o
    , Has (Texts.Definitions Text) env, Has (Texts.Navigation Text) env
    , Has (Texts.Code Text) env, Has (Texts.CodeUI Text) env
    , Has (Texts.Name Text) env, Grid.HasTexts env
    ) =>
    [Tree (Ann (Sugar.Payload (Name o) i o ExprGui.Payload))
        (Const (Sugar.GetVar (Name o) o))] ->
    ExprGuiM env i o (Gui Responsive o)
mkRelayedArgs args =
    do
        argEdits <- traverse (\(Ann a v) -> GetVarEdit.make (v ^. Lens._Wrapped) a) args
        collapsed <- grammar (label Texts.relay) <&> Responsive.fromTextView
        Options.boxSpaced ?? Options.disambiguationNone ?? collapsed : argEdits

mkBoxed ::
    ( Monad i, Monad o
    , Glue.HasTexts env
    , Has (Texts.Definitions Text) env, Has (Texts.Navigation Text) env
    , Has (Texts.Code Text) env, Has (Texts.CodeUI Text) env
    , Has (Texts.Name Text) env, Grid.HasTexts env
    ) =>
    Tree (Sugar.LabeledApply (Name o) i o)
        (Ann (Sugar.Payload (Name o) i o ExprGui.Payload)) ->
    Gui Responsive o -> ExprGuiM env i o (Gui Responsive o)
mkBoxed apply funcRow =
    do
        argRows <-
            case apply ^. Sugar.aAnnotatedArgs of
            [] -> pure []
            xs ->
                Responsive.taggedList
                <*> traverse makeArgRow xs
                <&> (:[])
        relayedArgs <-
            case apply ^. Sugar.aRelayedArgs of
            [] -> pure []
            args -> mkRelayedArgs args <&> (:[])
        Styled.addValFrame
            <*> (Responsive.vboxSpaced ?? (funcRow : argRows ++ relayedArgs))

makeSimple ::
    ( Monad i, Monad o
    , Grid.HasTexts env
    , Has (Texts.Code Text) env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Definitions Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Navigation Text) env
    ) =>
    Tree (Sugar.Apply (Sugar.Body (Name o) i o))
        (Ann (Sugar.Payload (Name o) i o ExprGui.Payload)) ->
    Sugar.Payload (Name o) i o ExprGui.Payload ->
    ExprGuiM env i o (Gui Responsive o)
makeSimple (Sugar.Apply func arg) pl =
    stdWrapParentExpr pl
    <*> ( (ResponsiveExpr.boxSpacedMDisamb ?? ExprGui.mParensId pl)
            <*> sequenceA
            [ ExprGuiM.makeSubexpression func
            , ExprGuiM.makeSubexpression arg
            ]
        )
