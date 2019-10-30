module Lamdu.GUI.Expr.FragmentEdit
    ( make
    ) where

import           Control.Applicative (liftA3)
import qualified Control.Lens as Lens
import           GUI.Momentu.Align (TextWidget)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Draw as MDraw
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.Rect (Rect(..))
import           GUI.Momentu.Responsive (Responsive(..), rWide, rWideDisambig, rNarrow)
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import           Hyper (Tree, Ann(..), hAnn)
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.GUI.Expr.EventMap as ExprEventMap
import qualified Lamdu.GUI.Expr.HoleEdit.SearchArea as SearchArea
import           Lamdu.GUI.Expr.HoleEdit.ValTerms (allowedFragmentSearchTerm)
import qualified Lamdu.GUI.Expr.HoleEdit.WidgetIds as HoleWidgetIds
import           Lamdu.GUI.ExpressionGui.Annotation (maybeAddAnnotationPl)
import           Lamdu.GUI.ExpressionGui.Monad (GuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as GuiM
import qualified Lamdu.GUI.ExpressionGui.Payload as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.Definitions as Texts
import qualified Lamdu.I18N.Name as Texts
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

-- TODO: Consider parameterizing `Responsive` such that this just becomes `liftA3`.
-- Which means:
--   Responsive a ==> Responsive (TextWidget a)
responsiveLiftA3 ::
    (TextWidget a -> TextWidget a -> TextWidget a -> TextWidget a) ->
    Responsive a -> Responsive a -> Responsive a -> Responsive a
responsiveLiftA3 f x y z =
    Responsive
    { _rWide = f (x ^. rWide) (y ^. rWide) (z ^. rWide)
    , _rWideDisambig = f (x ^. rWideDisambig) (y ^. rWideDisambig) (z ^. rWideDisambig)
    , _rNarrow = liftA3 f (x ^. rNarrow) (y ^. rNarrow) (z ^. rNarrow)
    }

fragmentDoc ::
    ( Has (MomentuTexts.Texts Text) env
    , Has (Texts.CodeUI Text) env
    ) =>
    env -> Lens.ALens' env Text -> E.Doc
fragmentDoc env lens =
    E.toDoc env
    [has . MomentuTexts.edit, has . Texts.fragment, lens]

make ::
    ( Monad i, Monad o
    , Glue.HasTexts env
    , Has (TextEdit.Texts Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Code Text) env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Definitions Text) env
    , SearchMenu.HasTexts env
    ) =>
    Tree (Sugar.Fragment Name i o)
        (Ann (Const (Sugar.Payload Name i o ExprGui.Payload))) ->
    Sugar.Payload Name i o ExprGui.Payload ->
    GuiM env i o (Responsive o)
make fragment pl =
    do
        isSelected <- GuiState.isSubCursor ?? myId
        env <- Lens.view id
        fragmentExprGui <-
            makeFragmentExprEdit fragment & GuiState.assignCursor myId innerId
        hover <- Hover.hover
        searchAreaGui <- SearchArea.make (fragment ^. Sugar.fOptions) pl allowedFragmentSearchTerm
        isHoleResult <- GuiM.isHoleResult
        let mkSearchArea
                | isHoleResult = const Element.empty
                | otherwise = searchAreaGui
        let searchAreaAbove = mkSearchArea Menu.Above
        let searchAreaBelow = mkSearchArea Menu.Below
        addAnnotation <- maybeAddAnnotationPl pl <&> (Align.tValue %~)
        Glue.Poly (|---|) <- Glue.mkPoly ?? Glue.Vertical
        anchor <- Hover.anchor
        let f fragmentExpr above below
                | isSelected
                || Widget.isFocused (fragmentExpr ^. Align.tValue) =
                    addAnnotation fragmentExpr & Align.tValue %~ Hover.hoverInPlaceOf options . anchor
                | otherwise =
                    addAnnotation fragmentExpr
                    where
                        options =
                            [ hoverFragmentExpr |---| (below <&> hover)
                            , (above <&> hover) |---| hoverFragmentExpr
                            ]
                            <&> (^. Align.tValue)
                        hoverFragmentExpr =
                            fragmentExpr
                            & Align.tValue %~ setFocalArea
                            & Align.tValue %~ anchor
                        setFocalArea w
                            | isSelected =
                                w
                                & Widget.wState . Widget._StateFocused . Lens.mapped . Widget.fFocalAreas .~
                                    [Rect 0 (w ^. Widget.wSize)]
                            | otherwise = w
        let healEventMap =
                fragment ^. Sugar.fHeal <&> WidgetIds.fromEntityId
                & E.keysEventMapMovesCursor
                    (Config.delKeys env <> env ^. has . Config.healKeys)
                    (fragmentDoc env (has . Texts.heal))
        isInAHole <- GuiM.isHoleResult
        ExprEventMap.add ExprEventMap.defaultOptions pl
            ?? responsiveLiftA3 f fragmentExprGui searchAreaAbove searchAreaBelow
            <&> Widget.widget %~ Widget.weakerEvents healEventMap
            <&> Widget.widget . Widget.wState . Widget._StateUnfocused .
                Widget.uMStroll .~
                ((strollDest ^. Lens._Unwrapped, strollDest ^. Lens._Unwrapped)
                    <$ guard (not isInAHole))
    where
        innerId = fragment ^. Sugar.fExpr . hAnn . Lens._Wrapped & WidgetIds.fromExprPayload
        myId = WidgetIds.fromExprPayload pl
        strollDest = pl ^. Sugar.plEntityId & HoleWidgetIds.make & HoleWidgetIds.hidOpen

makeFragmentExprEdit ::
    (Monad i, Functor o) =>
    Tree (Sugar.Fragment Name i o)
        (Ann (Const (Sugar.Payload Name i o ExprGui.Payload))) ->
    GuiM env i o (Responsive o)
makeFragmentExprEdit fragment =
    do
        theme <- Lens.view has
        let frameColor =
                theme ^.
                if fragment ^. Sugar.fTypeMatch
                then Theme.successColor
                else Theme.errorColor
        let frameWidth = theme ^. Theme.typeIndicatorFrameWidth
        fragmentExprGui <- GuiM.makeSubexpression (fragment ^. Sugar.fExpr)
        MDraw.addInnerFrame
            ?? frameColor ?? frameWidth
            ?? Element.padAround (frameWidth & _2 .~ 0) fragmentExprGui
