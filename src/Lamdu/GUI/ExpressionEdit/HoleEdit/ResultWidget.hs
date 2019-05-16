{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}

module Lamdu.GUI.ExpressionEdit.HoleEdit.ResultWidget
    ( make
    ) where

import           AST (Tree, Ann(..), Children(..), Recursive(..), RecursiveConstraint)
import           Control.Lens (Traversal')
import qualified Control.Lens.Extended as Lens
import           Data.Constraint (Dict, withDict)
import           Data.Proxy (Proxy(..))
import           GUI.Momentu (Widget, WithTextPos(..), TextWidget)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.Rect (Rect(..))
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Grid as Grid
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import           Lamdu.Config (Config(..))
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.GUI.ExpressionEdit.HoleEdit.ValTerms (getSearchStringRemainder)
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Payload as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.CodeUI as Texts
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

setFocalAreaToFullSize :: WithTextPos (Widget a) -> WithTextPos (Widget a)
setFocalAreaToFullSize =
    Align.tValue . Widget.sizedState <. Widget._StateFocused . Lens.mapped . Widget.fFocalAreas .@~
    (:[]) . Rect 0

-- | Remove unwanted event handlers from a hole result
removeUnwanted ::
    (MonadReader env m, Has Config env) => m (EventMap a -> EventMap a)
removeUnwanted =
    Lens.view has
    <&>
    \c ->
    concat
    [ Config.delKeys c
    , c ^. Config.enterSubexpressionKeys
    , c ^. Config.leaveSubexpressionKeys
    , c ^. Config.letAddItemKeys
    , Grid.stdKeys ^.. Lens.folded
    ]
    <&> MetaKey.toModKey
    <&> E.KeyEvent MetaKey.KeyState'Pressed
    & E.deleteKeys

applyResultLayout :: Responsive a -> WithTextPos (Widget a)
applyResultLayout = (^. Responsive.rWide)

makeWidget ::
    (Monad i, Monad o) =>
    Widget.Id ->
    Tree (Ann (Sugar.Payload (Name o) i o ExprGui.Payload)) (Sugar.Binder (Name o) i o) ->
    ExprGuiM env i o (TextWidget o)
makeWidget resultId holeResultConverted =
    do
        remUnwanted <- removeUnwanted
        theme <- Lens.view (has . Theme.hole)
        stdSpacing <- Spacer.getSpaceSize
        let padding = theme ^. Theme.holeResultPadding & (* stdSpacing)
        ExprGuiM.makeBinder holeResultConverted
            <&> Widget.enterResultCursor .~ resultId
            <&> Widget.widget . Widget.eventMapMaker . Lens.mapped %~ remUnwanted
            <&> applyResultLayout
            <&> setFocalAreaToFullSize
            <&> Element.padAround padding
            & ExprGuiM.withLocalIsHoleResult

make ::
    (Monad i, Monad o, Has (Texts.CodeUI Text) env) =>
    SearchMenu.ResultsContext ->
    Widget.Id ->
    o () ->
    Tree (Ann (Sugar.Payload (Name o) i o ExprGui.Payload)) (Sugar.Binder (Name o) i o) ->
    ExprGuiM env i o (Menu.RenderedOption o)
make ctx resultId pick holeResultConverted =
    (,) <$> Lens.view (has . Texts.pick) <*>
    makeWidget resultId holeResultConverted
    & GuiState.assignCursor resultId (pickResult ^. Menu.pickDest)
    <&>
    \(pickText, widget) ->
    Menu.RenderedOption
    { Menu._rPick =
        Widget.PreEvent
        { Widget._pDesc = pickText
        , Widget._pAction = pickResult <$ pick
        , Widget._pTextRemainder =
            holeResultConverted ^.
            SugarLens.binderResultExpr . Lens.asIndex .
            Lens.to (getSearchStringRemainder ctx)
        }
    , Menu._rWidget = widget
    }
    where
        holeResultId =
            holeResultConverted ^. SugarLens.binderResultExpr . Sugar.plEntityId
            & WidgetIds.fromEntityId
        mFirstHoleInside =
            holeResultConverted ^?
            unfinishedPayloads . Sugar.plEntityId
            <&> WidgetIds.fromEntityId
        pickResult =
            case mFirstHoleInside of
            Nothing ->
                Menu.PickResult
                { Menu._pickDest = holeResultId
                , Menu._pickMNextEntry = Nothing
                }
            Just innerEntryPoint ->
                Menu.PickResult
                { Menu._pickDest = innerEntryPoint
                , Menu._pickMNextEntry = Just innerEntryPoint
                }

unfinishedPayloads ::
    forall t a.
    Recursive SugarLens.SugarExpr t =>
    Traversal' (Tree (Ann a) t) a
unfinishedPayloads f (Ann a x) =
    withDict (recursive :: Dict (RecursiveConstraint t SugarLens.SugarExpr)) $
    flip Ann
    <$> children (Proxy @(Recursive SugarLens.SugarExpr)) (unfinishedPayloads f) x
    <*> (if SugarLens.isUnfinished x then f a else pure a)
