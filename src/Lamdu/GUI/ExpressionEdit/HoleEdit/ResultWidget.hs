{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Lamdu.GUI.ExpressionEdit.HoleEdit.ResultWidget
    ( make
    ) where

import qualified Control.Lens as Lens
import           Data.Store.Transaction (Transaction)
import qualified Data.Text as Text
import           GUI.Momentu (Widget, WithTextPos(..))
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
import           Lamdu.Config (HasConfig(..))
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.GUI.ExpressionGui (ExpressionN)
import qualified Lamdu.GUI.ExpressionGui as ExprGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.NearestHoles as NearestHoles
import qualified Lamdu.Sugar.Parens as AddParens
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction

getSearchStringRemainder :: SearchMenu.ResultsContext -> Sugar.Expression name m a -> Text
getSearchStringRemainder ctx holeResultConverted
    | any (`Lens.has` holeResultConverted) [literalNum, wrappedExpr . literalNum]
        && Text.isSuffixOf "." (ctx ^. SearchMenu.rSearchTerm)
        = "."
    | otherwise = ""
    where
        literalNum = Sugar.rBody . Sugar._BodyLiteral . Sugar._LiteralNum
        wrappedExpr = Sugar.rBody . Sugar._BodyWrapper . Sugar.wExpr

setFocalAreaToFullSize :: WithTextPos (Widget a) -> WithTextPos (Widget a)
setFocalAreaToFullSize =
    Align.tValue . Widget.sizedState <. Widget._StateFocused . Lens.mapped . Widget.fFocalAreas .@~
    (:[]) . Rect 0

postProcessSugar :: Int -> ExpressionN m () -> ExpressionN m ExprGui.Payload
postProcessSugar minOpPrec expr =
    expr
    & AddParens.addWith minOpPrec
    <&> pl
    & SugarLens.wrappedExprs . Sugar.plData . ExprGui.plShowAnnotation
    .~ ExprGui.alwaysShowAnnotations
    where
        pl (x, needParens, ()) =
            ExprGui.Payload
            { ExprGui._plStoredEntityIds = []
            , ExprGui._plNearestHoles = NearestHoles.none
            , ExprGui._plShowAnnotation = ExprGui.neverShowAnnotations
            , ExprGui._plNeedParens = needParens == AddParens.NeedsParens
            , ExprGui._plMinOpPrec = x
            }

-- | Remove unwanted event handlers from a hole result
removeUnwanted :: (MonadReader env m, HasConfig env) => m (EventMap a -> EventMap a)
removeUnwanted =
    Lens.view config
    <&>
    \c ->
    concat
    [ Config.delKeys c
    , Config.enterSubexpressionKeys c
    , Config.leaveSubexpressionKeys c
    , Grid.stdKeys ^.. Lens.folded
    , Config.letAddItemKeys c
    ]
    <&> MetaKey.toModKey
    <&> E.KeyEvent MetaKey.KeyState'Pressed
    & E.deleteKeys

applyResultLayout :: Responsive a -> WithTextPos (Widget a)
applyResultLayout fGui =
    (fGui ^. Responsive.render)
    Responsive.LayoutParams
    { Responsive._layoutMode = Responsive.LayoutWide
    , Responsive._layoutContext = Responsive.LayoutClear
    }

makeWidget ::
    Monad m =>
    Sugar.Payload f ExprGui.Payload ->
    Widget.Id ->
    Sugar.Expression (Name (T m)) (T m) () ->
    ExprGuiM m (WithTextPos (Widget (T m GuiState.Update)))
makeWidget pl resultId holeResultConverted =
    do
        remUnwanted <- removeUnwanted
        theme <- Theme.hole <$> Lens.view Theme.theme
        stdSpacing <- Spacer.getSpaceSize
        let padding = Theme.holeResultPadding theme <&> realToFrac & (* stdSpacing)
        postProcessSugar (pl ^. Sugar.plData . ExprGui.plMinOpPrec) holeResultConverted
            & ExprGuiM.makeSubexpression
            <&> Widget.enterResultCursor .~ resultId
            <&> Widget.widget . Widget.eventMapMaker . Lens.mapped %~ remUnwanted
            <&> applyResultLayout
            <&> setFocalAreaToFullSize
            <&> Element.pad padding

make ::
    Monad m =>
    Sugar.Payload f ExprGui.Payload ->
    SearchMenu.ResultsContext ->
    Widget.Id ->
    Sugar.HoleResult (T m) (Sugar.Expression (Name (T m)) (T m) ()) ->
    ExprGuiM m (Menu.RenderedOption (T m))
make pl ctx resultId holeResult =
    makeWidget pl resultId holeResultConverted
    & GuiState.assignCursor resultId (pickResult ^. Menu.pickDest)
    <&>
    \widget ->
    Menu.RenderedOption
    { Menu._rPick =
        Widget.PreEvent
        { Widget._pDesc = "Pick"
        , Widget._pAction = pickResult <$ holeResult ^. Sugar.holeResultPick
        , Widget._pTextRemainder = getSearchStringRemainder ctx holeResultConverted
        }
    , Menu._rWidget = widget
    }
    where
        holeResultId =
            holeResultConverted ^. Sugar.rPayload . Sugar.plEntityId
            & WidgetIds.fromEntityId
        mFirstHoleInside =
            holeResult ^?
            Sugar.holeResultConverted . SugarLens.holeAndWrapperPayloads . Sugar.plEntityId
            <&> WidgetIds.fromEntityId
        pickResult =
            case mFirstHoleInside of
            Nothing ->
                Menu.PickResult
                { Menu._pickDest = holeResultId
                , Menu._pickDestIsEntryPoint = False
                }
            Just innerEntryPoint ->
                Menu.PickResult
                { Menu._pickDest = innerEntryPoint
                , Menu._pickDestIsEntryPoint = True
                }
        holeResultConverted = holeResult ^. Sugar.holeResultConverted
