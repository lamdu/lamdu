module Lamdu.GUI.Expr.ApplyEdit
    ( makeSimple, makePostfix, makeLabeled, makePostfixFunc, makeOperatorRow
    ) where

import qualified Control.Lens as Lens
import qualified GUI.Momentu as M
import           GUI.Momentu.Direction (Orientation(..), Order(..))
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Expression as ResponsiveExpr
import qualified GUI.Momentu.Responsive.Options as Options
import           GUI.Momentu.Responsive.TaggedList (TaggedItem(..), taggedListIndent)
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import           GUI.Momentu.Widgets.StdKeys (dirKey)
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.Expr.CaseEdit as CaseEdit
import qualified Lamdu.GUI.Expr.EventMap as ExprEventMap
import qualified Lamdu.GUI.Expr.GetFieldEdit as GetFieldEdit
import qualified Lamdu.GUI.Expr.GetVarEdit as GetVarEdit
import qualified Lamdu.GUI.Expr.NominalEdit as NominalEdit
import qualified Lamdu.GUI.Expr.TagEdit as TagEdit
import           Lamdu.GUI.Annotation (maybeAddAnnotationPl)
import           Lamdu.GUI.Monad (GuiM)
import qualified Lamdu.GUI.Monad as GuiM
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.Types as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.GUI.Wrap (stdWrapParentExpr)
import qualified Lamdu.GUI.Wrap as Wrap
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.Navigation as Texts
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

makeLabeled :: _ => ExprGui.Expr Sugar.LabeledApply i o -> GuiM env i o (Responsive o)
makeLabeled (Ann (Const pl) apply) =
    ExprEventMap.add ExprEventMap.defaultOptions pl <*>
    ( Wrap.parentDelegator myId <*>
        case apply ^. Sugar.aMOpArgs of
        Nothing -> GetVarEdit.make GetVarEdit.Normal func >>= wrap
        Just (Sugar.OperatorArgs l r s) ->
            do
                env <- Lens.view id
                let swapAction order =
                        s <&> (\x -> if x then GuiState.updateCursor myId else mempty)
                        & E.keyPresses
                            (env ^. has . Config.orderDirKeys . Lens.cloneLens (dirKey (env ^. has) Horizontal order))
                            (E.toDoc env [has . MomentuTexts.edit, has . Texts.swapOperatorArgs])
                        & Widget.weakerEvents
                navigateOut <-
                    ExprEventMap.closeParenEvent
                    [has . MomentuTexts.navigation, has . Texts.leaveSubexpression]
                    (pure myId)
                disambRhs <-
                    if Lens.has extraArgs apply
                    then ResponsiveExpr.indent ?? Widget.toAnimId myId <> ["rhs"] <&> Responsive.vertLayoutMaybeDisambiguate
                    else pure id
                (ResponsiveExpr.boxSpacedMDisamb ?? ExprGui.mParensId pl)
                    <*> sequenceA
                    [ GuiM.makeSubexpression l <&> swapAction Forward
                    , makeOperatorRow
                        (Widget.weakerEvents navigateOut . swapAction Backward . disambRhs) func r
                        >>= wrap
                    ]
    )
    where
        myId = WidgetIds.fromExprPayload pl
        wrap x =
            (maybeAddAnnotationPl pl <&> (Widget.widget %~)) <*>
            addArgs apply x
        func = apply ^. Sugar.aFunc
        extraArgs = Sugar.aAnnotatedArgs . traverse . Lens.united <> Sugar.aPunnedArgs . traverse . Lens.united

makeOperatorRow ::
    _ =>
    (Responsive o -> Responsive o) ->
    (Annotated (ExprGui.Payload i o) # Const (Sugar.GetVar Name o)) ->
    ExprGui.Expr Sugar.Term i o ->
    GuiM env i o (Responsive o)
makeOperatorRow onR func r =
    (Options.boxSpaced ?? Options.disambiguationNone)
    <*> sequenceA
    [ GetVarEdit.make GetVarEdit.Operator func
    , GuiM.makeSubexpression r <&> onR
    ]

makeArgRow :: _ => ExprGui.Body Sugar.AnnotatedArg i o -> GuiM env i o (TaggedItem o)
makeArgRow arg =
    do
        expr <- GuiM.makeSubexpression (arg ^. Sugar.aaExpr)
        pre <- TagEdit.makeArgTag (arg ^. Sugar.aaTag)
        pure TaggedItem
            { _tagPre = pre <&> Widget.fromView & Just
            , _taggedItem = expr
            , _tagPost = Nothing
            }

addArgs :: _ => ExprGui.Body Sugar.LabeledApply i o -> Responsive o -> GuiM env i o (Responsive o)
addArgs apply funcRow =
    do
        argRows <-
            case apply ^. Sugar.aAnnotatedArgs of
            [] -> pure []
            xs -> taggedListIndent <*> traverse makeArgRow xs <&> (:[])
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
    _ =>
    Annotated (ExprGui.Payload i o)
        # Sugar.App (Sugar.Term (Sugar.Annotation (Sugar.EvaluationScopes Name i) Name) Name i o) ->
    GuiM env i o (Responsive o)
makeSimple (Ann (Const pl) (Sugar.App func arg)) =
    (ResponsiveExpr.boxSpacedMDisamb ?? ExprGui.mParensId pl)
    <*> sequenceA
    [ GuiM.makeSubexpression func
    , GuiM.makeSubexpression arg
    ] & stdWrapParentExpr pl

makePostfix :: _ => ExprGui.Expr Sugar.PostfixApply i o -> GuiM env i o (Responsive o)
makePostfix (Ann (Const pl) (Sugar.PostfixApply arg func)) =
    (ResponsiveExpr.boxSpacedMDisamb ?? ExprGui.mParensId pl)
    <*> sequenceA
    [ GuiM.makeSubexpression arg
    , makePostfixFunc func
    ] & stdWrapParentExpr pl

makePostfixFunc :: _ => ExprGui.Expr Sugar.PostfixFunc i o -> GuiM env i o (Responsive o)
makePostfixFunc (Ann (Const pl) b) =
    (ResponsiveExpr.boxSpacedMDisamb ?? ExprGui.mParensId pl) <*>
    ( case b of
        Sugar.PfCase x -> CaseEdit.make (Ann (Const pl) x)
        Sugar.PfFromNom x -> NominalEdit.makeFromNom myId x & stdWrapParentExpr pl
        Sugar.PfGetField x -> GetFieldEdit.make x & stdWrapParentExpr pl
        <&> (:[]))
    -- Without adjusting anim-ids there is clash in fragment results such as
    -- ".Maybe .case"
    & local (M.animIdPrefix .~ Widget.toAnimId (WidgetIds.fromExprPayload pl))
    where
        myId = WidgetIds.fromExprPayload pl
