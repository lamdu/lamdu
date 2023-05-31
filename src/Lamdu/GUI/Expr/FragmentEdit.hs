module Lamdu.GUI.Expr.FragmentEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import           Data.Foldable (Foldable(..))
import qualified GUI.Momentu as M
import           GUI.Momentu (Responsive, (/|/), noMods)
import qualified GUI.Momentu.Animation as Anim
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.I18N as MomentuTexts
import qualified GUI.Momentu.ModKey as ModKey
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Expression as ResponsiveExpr
import qualified GUI.Momentu.Responsive.Options as Options
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Label as Label
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Config.Theme.ValAnnotation as ValAnnotation
import           Lamdu.GUI.Annotation (addAnnotationBackground, addAnnotationBelow)
import qualified Lamdu.GUI.Expr.ApplyEdit as ApplyEdit
import qualified Lamdu.GUI.Expr.EventMap as ExprEventMap
import qualified Lamdu.GUI.Expr.GetVarEdit as GetVarEdit
import qualified Lamdu.GUI.Expr.HoleOptEdit as HoleOptEdit
import qualified Lamdu.GUI.Expr.InjectEdit as InjectEdit
import qualified Lamdu.GUI.Expr.NominalEdit as NominalEdit
import           Lamdu.GUI.Expr.OptionEdit
import qualified Lamdu.GUI.Expr.TagEdit as TagEdit
import           Lamdu.GUI.Monad (GuiM)
import qualified Lamdu.GUI.Monad as GuiM
import           Lamdu.GUI.Styled (label, grammar)
import qualified Lamdu.GUI.TagView as TagView
import qualified Lamdu.GUI.TypeView as TypeView
import qualified Lamdu.GUI.Types as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.GUI.Wrap as Wrap
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.CodeUI as Texts
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

allowedSearchTerm :: Text -> Bool
allowedSearchTerm x =
    ExprEventMap.allowedSearchTerm x || isWrapInRec
    where
        isWrapInRec =
            case x ^? Lens._Cons of
            Nothing -> False
            Just (p, r) -> p == '{' && ExprEventMap.isAlphaNumericName r

make :: _ => ExprGui.Expr Sugar.Fragment i o -> GuiM env i o (Responsive o)
make (Ann (Const pl) fragment) =
    do
        fragmentExprGui <- fragment ^. Sugar.fExpr & GuiM.makeSubexpression

        env <- Lens.view id
        let healDoc = E.toDoc env editFragmentHeal
        let delHealsEventMap =
                E.keysEventMapMovesCursor (Config.delKeys env) healDoc healAction
        let healEventMap =
                E.keysEventMapMovesCursor (env ^. has . Config.healKeys) healDoc healAction <>
                ExprEventMap.closeParenEvent editFragmentHeal healAction env

        optApply <- fragment ^. Sugar.fOptApply & GuiM.im
        let applyPl = optApply ^. Sugar.optionExpr . annotation
        searchTerm <- SearchMenu.readSearchTerm menuId
        applyActionsEventMap <-
            (if searchTerm == "" then  ExprEventMap.makeBaseEvents applyPl else mempty)
            <> ExprEventMap.makeLiteralCharEventMap searchTerm (applyPl ^. Sugar.plActions . Sugar.setToLiteral)
            <&> Lens.mapped %~ ((optApply ^. Sugar.optionPick) *>)

        searchMenu <-
            SearchMenu.make
            ( SearchMenu.searchTermEdit menuId (pure . allowedSearchTerm)
                <&> Lens.mapped . SearchMenu.termWidget . M.tValue %~ id
            )
            (makeResults (fragment ^. Sugar.fOptions) (fragment ^. Sugar.fTagSuffixes))
             M.empty menuId
            Menu.AnyPlace
            & local (has . SearchMenu.emptyStrings . Lens.mapped .~ "?")
            -- Space goes to next hole in target (not necessarily visible)
            & local (has . Menu.configKeysPickOptionAndGotoNext <>~ [noMods ModKey.Key'Space])
            <&> Lens.mapped %~ Widget.weakerEventsWithoutPreevents delHealsEventMap
            <&> Lens.mapped %~ Widget.strongerEventsWithoutPreevents applyActionsEventMap

        addInnerType <-
            case fragment ^. Sugar.fTypeMismatch of
            Nothing -> pure id
            Just mismatch ->
                do
                    color <- Lens.view (has . Theme.errorColor)
                    elemId <- Element.subElemId "err-line"
                    spacing <- Lens.view
                        (has . Theme.valAnnotation . ValAnnotation.valAnnotationSpacing)
                    stdFontHeight <- Spacer.stdFontHeight
                    ann <-
                        TypeView.make (mismatch ^. Sugar.tmType)
                        M./-/
                        case mismatch ^. Sugar.tmReason of
                        Sugar.TypeVarSkolemEscape -> label Texts.varSkolemEscape
                        _ -> pure M.empty
                    addAnnotationBackground <&> const >>= (`addAnnotationBelow` ann)
                        <&> (lineBelow color elemId (spacing * stdFontHeight) .)
            & Element.locallyAugmented ("inner type"::Text)

        let addEvents
                | Widget.isFocused (searchMenu ^. M.tValue) = pure
                | otherwise = ExprEventMap.add ExprEventMap.defaultOptions pl

        ResponsiveExpr.boxSpacedMDisamb (ExprGui.mParensId pl)
            [ fragmentExprGui
            , Responsive.fromWithTextPos searchMenu
            ]
            <&> Widget.widget %~ addInnerType
            >>= Wrap.parentDelegator (WidgetIds.fromExprPayload pl)
            & Wrap.wrapWithoutEvents pl
            >>= addEvents
            <&> Widget.weakerEventsWithoutPreevents healEventMap
    where
        editFragmentHeal = [has . MomentuTexts.edit, has . Texts.fragment, has . Texts.heal]
        healAction = fragment ^. Sugar.fHeal <&> WidgetIds.fromEntityId
        menuId = WidgetIds.fromExprPayload pl & WidgetIds.fragmentHoleId
        lineBelow color elemId spacing ann =
            ann
            & Element.setLayeredImage . Element.layers . Lens.ix 0 %~ (<> line)
            where
                line =
                    Anim.coloredRectangle elemId color
                    & Anim.scale (M.Vector2 (ann ^. Element.width) spacing)
                    & Anim.translate (M.Vector2 0 (ann ^. Element.height))

makeResults ::
    _ =>
    i (Sugar.Query -> i [Sugar.Option Sugar.FragOpt Name i o]) ->
    Sugar.TagSuffixes ->
    SearchMenu.ResultsContext ->
    GuiM env i o (Menu.OptionList (Menu.Option (GuiM env i o) o))
makeResults opts tagSuffixes ctx =
    do
        c <- Lens.view (Config.hasConfig . Config.completion . Config.completionResultCount)
        GuiM.im opts <*>
            makeQuery tagSuffixes ctx
            >>= GuiM.im
            <&> take c
    <&> Lens.mapped %~ makeResult makeFragOpt ctx
    <&> Menu.OptionList isTruncated
    where
        isTruncated = False -- TODO: Need to specify whether we have more options

makeFragOpt :: _ => ExprGui.Expr Sugar.FragOpt i o -> GuiM env i o (Responsive o)
makeFragOpt (Ann (Const a) b) =
    case b of
    Sugar.FragPostfix x -> traverse ApplyEdit.makePostfixFunc x >>= ResponsiveExpr.boxSpacedMDisamb Nothing
    Sugar.FragInject x -> InjectEdit.make (Ann (Const a) (Const x))
    Sugar.FragApplyFunc x -> GetVarEdit.make GetVarEdit.Normal (Ann (Const a) (Const x)) /|/ grammar (Label.make " _")
    Sugar.FragOp x -> makeFragOperator x
    Sugar.FragToNom x -> NominalEdit.makeTIdView x & fromView
    Sugar.FragIf t ->
        (grammar (label Texts.if_) /|/ grammar (Label.make ":")) /|/
        Spacer.stdHSpace /|/ GuiM.makeSubexpression t
    Sugar.FragArgument x -> HoleOptEdit.make (Ann (Const a) x)
    Sugar.FragLam -> grammar (label Texts.lam) & fromView
    Sugar.FragDefer -> grammar (label Texts.defer) & fromView
    Sugar.FragWrapInRec x ->
        grammar (label Texts.recordOpener) /|/ TagView.make (x ^. Sugar.tagRefTag) & fromView
    -- Reproduction of behaviour from Lamdu.GUI.Expr.make,
    -- otherwise fragment editors would have clashing anim ids
    & local (M.elemIdPrefix .~ M.asElemId myId)
    where
        myId = WidgetIds.fromExprPayload a
        fromView act =
            act >>= M.tValue (Widget.makeFocusableWidget myId . Widget.fromView) <&> Responsive.fromWithTextPos
            & Wrap.stdWrap a

makeFragOperator :: _ => ExprGui.Body Sugar.FragOperator i o -> GuiM env i o (Responsive o)
makeFragOperator (Sugar.FragOperator f rArg aArgs) =
    do
        -- Go to first hole in right side of operator
        nextEventMap <-
            rArg ^? SugarLens.unfinishedPayloads
            & Lens._Just (mkNextEventMap . WidgetIds.fromExprPayload) <&> fold
        sequenceA
            ( ( sequenceA
                [ GetVarEdit.make GetVarEdit.Operator f
                , GuiM.makeSubexpression rArg
                ] >>= Options.boxSpaced Options.disambiguationNone
                )
            : (aArgs <&> fmap Responsive.fromTextView . TagEdit.makeArgTag)
            ) >>= Options.boxSpaced Options.disambiguationNone
            <&> Widget.strongerEvents nextEventMap
