module Lamdu.GUI.Expr.ApplyEdit
    ( makeSimple, makePostfix, makeLabeled, makePostfixFunc
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Reader.Extended (pushToReader)
import           GUI.Momentu (Responsive)
import qualified GUI.Momentu as M
import           GUI.Momentu.Direction (Orientation(..), Order(..))
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.I18N as MomentuTexts
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Expression as ResponsiveExpr
import qualified GUI.Momentu.Responsive.Options as Options
import           GUI.Momentu.Responsive.TaggedList (TaggedItem(..), taggedListIndent, tagPost)
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
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.Navigation as Texts
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

wrapParentNoAnn :: _ => Sugar.Payload v o -> GuiM env i o (Responsive o) -> GuiM env i o (Responsive o)
wrapParentNoAnn pl act =
    act
    >>= Wrap.parentDelegator (WidgetIds.fromExprPayload pl)
    >>= ExprEventMap.add ExprEventMap.defaultOptions pl

makeLabeled :: _ => ExprGui.Expr Sugar.LabeledApply i o -> GuiM env i o (Responsive o)
makeLabeled (Ann (Const pl) apply) =
    do
        argRows <-
            case apply ^. Sugar.aAnnotatedArgs of
            [] -> pure []
            xs -> traverse makeArgRow xs <&> Lens._last . tagPost .~ Nothing >>= taggedListIndent <&> (:[])
        punnedArgs <-
            case apply ^. Sugar.aPunnedArgs of
            [] -> pure []
            args -> GetVarEdit.makePunnedVars args <&> (:[])
        let extraRows = argRows <> punnedArgs
        let addArgs funcRow
                | null extraRows = pure funcRow
                | otherwise = Responsive.vboxSpaced (funcRow : extraRows) >>= Styled.addValFrame
        let wrap x =
                do
                    addAnnotation <- maybeAddAnnotationPl pl
                    addArgs x <&> Widget.widget %~ addAnnotation
        case apply ^. Sugar.aMOpArgs of
            Nothing -> GetVarEdit.make GetVarEdit.Normal func >>= wrap
            Just (Sugar.OperatorArgs l r s) ->
                do
                    env <- Lens.view id
                    let swapAction order =
                            s <&> (\x -> if x then GuiState.updateCursor myId else mempty)
                            & E.keyPresses
                                (env ^. has . Config.orderDirKeys . dirKey (env ^. has) Horizontal order)
                                (E.toDoc env [has . MomentuTexts.edit, has . Texts.swapOperatorArgs])
                            & Widget.weakerEvents
                    navigateOut <-
                        ExprEventMap.closeParenEvent
                        [has . MomentuTexts.navigation, has . Texts.leaveSubexpression]
                        (pure myId)
                    disambRhs <-
                        if Lens.has extraArgs apply
                        then ResponsiveExpr.indent (M.asElemId myId <> "rhs") & pushToReader <&> Responsive.vertLayoutMaybeDisambiguate
                        else pure id
                    lhs <- GuiM.makeSubexpression l <&> swapAction Forward
                    op <- GetVarEdit.make GetVarEdit.Operator func
                    rhs <- GuiM.makeSubexpression r <&> Widget.weakerEvents navigateOut . swapAction Backward . disambRhs
                    res <-
                        sequenceA
                        [ pure lhs
                        , Options.boxSpaced Options.disambiguationNone [op, rhs] >>= wrap
                        ] >>= ResponsiveExpr.boxSpacedMDisamb (ExprGui.mParensId pl)
                    if null extraRows
                        then
                            sequenceA
                            [ Options.boxSpaced Options.disambiguationNone [lhs, op]
                            , pure rhs
                            ] >>= ResponsiveExpr.boxSpacedMDisamb (ExprGui.mParensId pl)
                            >>= wrap
                            <&> Responsive.rWide .~ res ^. Responsive.rWide
                        else pure res
    & wrapParentNoAnn pl
    where
        myId = WidgetIds.fromExprPayload pl
        func = apply ^. Sugar.aFunc
        extraArgs = Sugar.aAnnotatedArgs . traverse . Lens.united <> Sugar.aPunnedArgs . traverse . Lens.united

makeArgRow :: _ => ExprGui.Body Sugar.AnnotatedArg i o -> GuiM env i o (TaggedItem o)
makeArgRow arg =
    do
        expr <- GuiM.makeSubexpression (arg ^. Sugar.aaExpr)
        pre <- TagEdit.makeArgTag (arg ^. Sugar.aaTag)
        comma <-
            Styled.label Texts.compositeSeparator
            & local (M.elemIdPrefix .~ (M.asElemId . WidgetIds.fromEntityId) (arg ^. Sugar.aaTag . Sugar.tagInstance))
        pure TaggedItem
            { _tagPre = pre <&> Widget.fromView & Just
            , _taggedItem = expr
            , _tagPost = comma <&> Widget.fromView & Just
            }

makeSimple ::
    _ =>
    Annotated (ExprGui.Payload i o)
        # Sugar.App (Sugar.Term (Sugar.Annotation (Sugar.EvaluationScopes Name i) Name) Name i o) ->
    GuiM env i o (Responsive o)
makeSimple (Ann (Const pl) (Sugar.App func arg)) =
    sequenceA
    [ GuiM.makeSubexpression func
    , GuiM.makeSubexpression arg
    ] >>= ResponsiveExpr.boxSpacedMDisamb (ExprGui.mParensId pl)
    & stdWrapParentExpr pl

makePostfix :: _ => ExprGui.Expr Sugar.PostfixApply i o -> GuiM env i o (Responsive o)
makePostfix (Ann (Const pl) (Sugar.PostfixApply arg func)) =
    sequenceA
    [ GuiM.makeSubexpression arg
    , (maybeAddAnnotationPl pl <&> (Widget.widget %~)) <*> makePostfixFunc func
    ] >>= ResponsiveExpr.boxSpacedMDisamb (ExprGui.mParensId pl)
    & wrapParentNoAnn pl

makePostfixFunc :: _ => ExprGui.Expr Sugar.PostfixFunc i o -> GuiM env i o (Responsive o)
makePostfixFunc (Ann (Const pl) b) =
    case b of
    Sugar.PfCase x -> CaseEdit.make (Ann (Const pl) x)
    Sugar.PfFromNom x -> NominalEdit.makeFromNom myId x & stdWrapParentExpr pl
    Sugar.PfGetField x -> GetFieldEdit.make x & stdWrapParentExpr pl
    <&> (:[])
    >>= ResponsiveExpr.boxSpacedMDisamb (ExprGui.mParensId pl)
    -- Without adjusting anim-ids there is clash in fragment results such as
    -- ".Maybe .case"
    & local (M.elemIdPrefix .~ M.asElemId (WidgetIds.fromExprPayload pl))
    where
        myId = WidgetIds.fromExprPayload pl
