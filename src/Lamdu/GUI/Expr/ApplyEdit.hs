module Lamdu.GUI.Expr.ApplyEdit
    ( makeSimple, makeLabeled
    ) where

import           AST (Tree)
import           AST.Knot.Ann (Ann(..), ann, val)
import           Control.Lens (Const)
import qualified Control.Lens as Lens
import           GUI.Momentu.Animation (AnimId)
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
import qualified Lamdu.GUI.Expr.GetVarEdit as GetVarEdit
import qualified Lamdu.GUI.Expr.TagEdit as TagEdit
import           Lamdu.GUI.ExpressionGui.Monad (GuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as GuiM
import qualified Lamdu.GUI.ExpressionGui.Payload as ExprGui
import           Lamdu.GUI.ExpressionGui.Wrap (stdWrap, stdWrapParentExpr)
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
    Tree (Ann (Sugar.Payload Name i o ExprGui.Payload))
        (Const (Sugar.BinderVarRef Name o)) ->
    GuiM env i o (Responsive o)
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
    || Lens.has (Sugar.aPunnedArgs . traverse) apply

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
    Tree (Sugar.LabeledApply Name i o)
        (Ann (Sugar.Payload Name i o ExprGui.Payload)) ->
    GuiM env i o (Responsive o)
makeFuncRow mParensId apply =
    case apply ^. Sugar.aSpecialArgs of
    Sugar.Verbose -> makeFunc GetVarEdit.Normal func
    Sugar.Object arg ->
        (ResponsiveExpr.boxSpacedMDisamb ?? mParensId)
        <*> sequenceA
        [ makeFunc GetVarEdit.Normal func
        , GuiM.makeSubexpression arg
        ]
    Sugar.Infix l r ->
        (ResponsiveExpr.boxSpacedMDisamb ?? mParensId)
        <*> sequenceA
        [ (Options.boxSpaced ?? Options.disambiguationNone)
            <*> sequenceA
            [ GuiM.makeSubexpression l
            , makeFunc GetVarEdit.Infix func
            ]
        , GuiM.makeSubexpression r
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
    Tree (Sugar.LabeledApply Name i o)
        (Ann (Sugar.Payload Name i o ExprGui.Payload)) ->
    Sugar.Payload Name i o ExprGui.Payload ->
    GuiM env i o (Responsive o)
makeLabeled apply pl =
    stdWrapParentExpr pl
    <*> (makeFuncRow (ExprGui.mParensId pl) apply >>= addBox)
    where
        addBox
            | isBoxed apply = mkBoxed apply
            | otherwise = pure

makeArgRow ::
    (Monad i, Glue.HasTexts env, Has (Texts.Name Text) env) =>
    Sugar.AnnotatedArg Name (ExprGui.SugarExpr i o) ->
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

mkBoxed ::
    ( Monad i, Monad o
    , Glue.HasTexts env
    , Has (Texts.Definitions Text) env, Has (Texts.Navigation Text) env
    , Has (Texts.Code Text) env, Has (Texts.CodeUI Text) env
    , Has (Texts.Name Text) env, Grid.HasTexts env
    ) =>
    Tree (Sugar.LabeledApply Name i o)
        (Ann (Sugar.Payload Name i o ExprGui.Payload)) ->
    Responsive o -> GuiM env i o (Responsive o)
mkBoxed apply funcRow =
    do
        argRows <-
            case apply ^. Sugar.aAnnotatedArgs of
            [] -> pure []
            xs -> taggedList <*> traverse makeArgRow xs <&> (:[])
        punnedArgs <-
            case apply ^. Sugar.aPunnedArgs of
            [] -> pure []
            args -> GetVarEdit.makePunnedVars args <&> (:[])
        Styled.addValFrame
            <*> (Responsive.vboxSpaced ?? (funcRow : argRows ++ punnedArgs))

makeSimple ::
    ( Monad i, Monad o
    , Grid.HasTexts env
    , Has (Texts.Code Text) env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Definitions Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Navigation Text) env
    ) =>
    Tree (Sugar.App (Sugar.Body Name i o))
        (Ann (Sugar.Payload Name i o ExprGui.Payload)) ->
    Sugar.Payload Name i o ExprGui.Payload ->
    GuiM env i o (Responsive o)
makeSimple (Sugar.App func arg) pl =
    stdWrapParentExpr pl
    <*> ( (ResponsiveExpr.boxSpacedMDisamb ?? ExprGui.mParensId pl)
            <*> sequenceA
            [ GuiM.makeSubexpression func
            , GuiM.makeSubexpression arg
            ]
        )
