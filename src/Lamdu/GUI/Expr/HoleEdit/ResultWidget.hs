{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}

module Lamdu.GUI.Expr.HoleEdit.ResultWidget
    ( make
    ) where

import           Control.Lens (Traversal')
import qualified Control.Lens as Lens
import           GUI.Momentu.Align (TextWidget)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.I18N as MomentuTexts
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.Rect (Rect(..))
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Grid as Grid
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import           Hyper (htraverse, (#>), withDict)
import           Lamdu.Config (Config(..))
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.GUI.Monad (GuiM)
import qualified Lamdu.GUI.Monad as GuiM
import qualified Lamdu.GUI.Types as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

setFocalAreaToFullSize :: TextWidget a -> TextWidget a
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

applyResultLayout :: Responsive a -> TextWidget a
applyResultLayout = (^. Responsive.rWide)

makeWidget ::
    (Monad i, Monad o) =>
    Widget.Id -> ExprGui.Expr Sugar.Binder i o -> GuiM env i o (TextWidget o)
makeWidget resultId holeResultConverted =
    do
        remUnwanted <- removeUnwanted
        theme <- Lens.view (has . Theme.hole)
        stdSpacing <- Spacer.getSpaceSize
        let padding = theme ^. Theme.holeResultPadding & (* stdSpacing)
        GuiM.makeBinder holeResultConverted
            <&> Widget.enterResultCursor .~ resultId
            <&> Widget.widget . Widget.eventMapMaker . Lens.mapped %~ remUnwanted
            <&> applyResultLayout
            <&> setFocalAreaToFullSize
            <&> Element.padAround padding
            & GuiM.withLocalIsHoleResult

make ::
    (Monad i, Monad o, Has (MomentuTexts.Texts Text) env) =>
    Widget.Id -> o () -> ExprGui.Expr Sugar.Binder i o ->
    GuiM env i o (Menu.RenderedOption o)
make resultId pick holeResultConverted =
    (,) <$> Lens.view (has . MomentuTexts.choose) <*>
    makeWidget resultId holeResultConverted
    & GuiState.assignCursor resultId (pickResult ^. Menu.pickDest)
    <&>
    \(pickText, widget) ->
    Menu.RenderedOption
    { Menu._rPick =
        Widget.PreEvent
        { Widget._pDesc = pickText
        , Widget._pAction = pickResult <$ pick
        , Widget._pTextRemainder = ""
        }
    , Menu._rWidget = widget
    }
    where
        holeResultId =
            holeResultConverted ^. SugarLens.binderResultExpr . _1 . Sugar.plEntityId
            & WidgetIds.fromEntityId
        mFirstHoleInside =
            holeResultConverted ^?
            unfinishedPayloads . _1 . Sugar.plEntityId
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
    SugarLens.SugarExpr t =>
    Traversal' (Annotated a # t) a
unfinishedPayloads f (Ann (Const a) x) =
    withDict (SugarLens.sugarExprRecursive (Proxy @t)) $
    flip Ann
    <$> htraverse (Proxy @SugarLens.SugarExpr #> unfinishedPayloads f) x
    <*> ((if SugarLens.isUnfinished x then f a else pure a) <&> Const)
