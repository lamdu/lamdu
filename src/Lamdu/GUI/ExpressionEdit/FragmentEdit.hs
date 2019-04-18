module Lamdu.GUI.ExpressionEdit.FragmentEdit
    ( make
    ) where

import           AST (Tree, Ann(..), ann)
import           Control.Applicative (liftA3)
import qualified Control.Lens as Lens
import           GUI.Momentu.Align (WithTextPos)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Draw as MDraw
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/-/))
import qualified GUI.Momentu.Hover as Hover
import           GUI.Momentu.Rect (Rect(..))
import           GUI.Momentu.Responsive (Responsive(..), rWide, rWideDisambig, rNarrow)
import           GUI.Momentu.State (Gui)
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.SearchArea as SearchArea
import           Lamdu.GUI.ExpressionEdit.HoleEdit.ValTerms (allowedFragmentSearchTerm)
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds as HoleWidgetIds
import           Lamdu.GUI.ExpressionGui.Annotation (maybeAddAnnotationPl)
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Payload as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

-- TODO: Consider parameterizing `Responsive` such that this just becomes `liftA3`.
-- Which means:
--   Responsive a ==> Responsive (WithTextPos (Widget a))
responsiveLiftA3 ::
    (WithTextPos (Widget a) -> WithTextPos (Widget a) -> WithTextPos (Widget a) -> WithTextPos (Widget a)) ->
    Responsive a -> Responsive a -> Responsive a -> Responsive a
responsiveLiftA3 f x y z =
    Responsive
    { _rWide = f (x ^. rWide) (y ^. rWide) (z ^. rWide)
    , _rWideDisambig = f (x ^. rWideDisambig) (y ^. rWideDisambig) (z ^. rWideDisambig)
    , _rNarrow = liftA3 f (x ^. rNarrow) (y ^. rNarrow) (z ^. rNarrow)
    }

make ::
    (Monad i, Monad o) =>
    Tree (Sugar.Fragment (Name o) i o)
        (Ann (Sugar.Payload (Name o) i o ExprGui.Payload)) ->
    Sugar.Payload (Name o) i o ExprGui.Payload ->
    ExprGuiM i o (Gui Responsive o)
make fragment pl =
    do
        isSelected <- GuiState.isSubCursor ?? myId
        config <- Lens.view Config.config
        fragmentExprGui <-
            makeFragmentExprEdit fragment & GuiState.assignCursor myId innerId
        hover <- Hover.hover
        searchAreaGui <- SearchArea.make (fragment ^. Sugar.fOptions) pl allowedFragmentSearchTerm
        isHoleResult <- ExprGuiM.isHoleResult
        let mkSearchArea
                | isHoleResult = const Element.empty
                | otherwise = searchAreaGui
        let searchAreaAbove = mkSearchArea Menu.Above
        let searchAreaBelow = mkSearchArea Menu.Below
        addAnnotation <- maybeAddAnnotationPl pl <&> (Align.tValue %~)
        let f fragmentExpr above below
                | isSelected
                || Widget.isFocused (fragmentExpr ^. Align.tValue) =
                    addAnnotation fragmentExpr & Align.tValue %~ Hover.hoverInPlaceOf options . Hover.anchor
                | otherwise =
                    addAnnotation fragmentExpr
                    where
                        options =
                            [ hoverFragmentExpr /-/ (below <&> hover)
                            , (above <&> hover) /-/ hoverFragmentExpr
                            ]
                            <&> (^. Align.tValue)
                        hoverFragmentExpr =
                            fragmentExpr
                            & Align.tValue %~ setFocalArea
                            & Align.tValue %~ Hover.anchor
                        setFocalArea w
                            | isSelected =
                                w
                                & Widget.wState . Widget._StateFocused . Lens.mapped . Widget.fFocalAreas .~
                                    [Rect 0 (w ^. Widget.wSize)]
                            | otherwise = w
        let healEventMap =
                case fragment ^. Sugar.fHeal of
                Sugar.TypeMismatch ->
                    pl ^. Sugar.plEntityId & HoleWidgetIds.make
                    & HoleWidgetIds.hidOpen & pure
                    & E.keysEventMapMovesCursor
                    (config ^. Config.healKeys)
                    (E.Doc ["Edit", "Fragment", "Show Results"])
                Sugar.HealAction heal ->
                    heal <&> WidgetIds.fromEntityId
                    & E.keysEventMapMovesCursor
                        (Config.delKeys config <> config ^. Config.healKeys)
                        (E.Doc ["Edit", "Fragment", "Heal"])
        isInAHole <- ExprGuiM.isHoleResult
        ExprEventMap.add ExprEventMap.defaultOptions pl
            ?? responsiveLiftA3 f fragmentExprGui searchAreaAbove searchAreaBelow
            <&> Widget.widget %~ Widget.weakerEvents healEventMap
            <&> Widget.widget . Widget.wState . Widget._StateUnfocused .
                Widget.uMStroll .~
                ((strollDest ^. Lens._Unwrapped, strollDest ^. Lens._Unwrapped)
                    <$ guard (not isInAHole))
    where
        innerId = fragment ^. Sugar.fExpr . ann & WidgetIds.fromExprPayload
        myId = WidgetIds.fromExprPayload pl
        strollDest = pl ^. Sugar.plEntityId & HoleWidgetIds.make & HoleWidgetIds.hidOpen

makeFragmentExprEdit ::
    (Monad i, Functor o) =>
    Tree (Sugar.Fragment (Name o) i o)
        (Ann (Sugar.Payload (Name o) i o ExprGui.Payload)) ->
    ExprGuiM i o (Gui Responsive o)
makeFragmentExprEdit fragment =
    do
        theme <- Lens.view Theme.theme
        let frameColor =
                theme ^.
                case fragment ^. Sugar.fHeal of
                Sugar.HealAction {} -> Theme.successColor
                Sugar.TypeMismatch {} -> Theme.errorColor
        let frameWidth = theme ^. Theme.typeIndicatorFrameWidth
        fragmentExprGui <- ExprGuiM.makeSubexpression (fragment ^. Sugar.fExpr)
        MDraw.addInnerFrame
            ?? frameColor ?? frameWidth
            ?? Element.padAround (frameWidth & _2 .~ 0) fragmentExprGui
