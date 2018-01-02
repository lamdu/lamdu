{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Lamdu.GUI.ExpressionEdit.HoleEdit.LiteralText
    ( makeLiteralTextEventMap
    ) where

import qualified Control.Lens as Lens
import           Data.Functor.Identity (Identity(..))
import qualified Data.Store.Transaction as Transaction
import qualified Data.Text as Text
import           GUI.Momentu (MetaKey(..))
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.MetaKey as MetaKey
import qualified GUI.Momentu.State as GuiState
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.State as HoleState
import           Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds (WidgetIds(..))
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction.Transaction

toLiteralTextKeys :: [MetaKey]
toLiteralTextKeys =
    [ MetaKey.shift MetaKey.Key'Apostrophe
    , MetaKey MetaKey.noMods MetaKey.Key'Apostrophe
    ]

toLiteralTextEventMap ::
    Monad m =>
    Sugar.LeafHoleActions (T m) (Sugar.Expression name n a) ->
    EventMap (T m GuiState.Update)
toLiteralTextEventMap actions =
    E.keysEventMapMovesCursor toLiteralTextKeys
    (E.Doc ["Edit", "Create Text Literal"]) $
    do
        (_score, mkResult) <- Sugar.LiteralText (Identity "") & actions ^. Sugar.holeOptionLiteral
        result <- mkResult
        result ^. Sugar.holeResultPick
        let argExpr =
                Sugar.holeResultConverted . Sugar.rBody . Sugar._BodyHole .
                Sugar.holeKind . Sugar._WrapperHole . Sugar.haExpr
        case result ^? argExpr of
            Just arg -> arg
            _ -> result ^. Sugar.holeResultConverted
            ^. Sugar.rPayload . Sugar.plEntityId
            & WidgetIds.fromEntityId
            & WidgetIds.literalTextEditOf
            & pure

makeLiteralTextEventMap ::
    Monad m =>
    Sugar.HoleKind (T m) (Sugar.Expression n p a) e -> WidgetIds ->
    ExprGuiM m (EventMap (T m GuiState.Update))
makeLiteralTextEventMap holeKind widgetIds =
    HoleState.readSearchTerm widgetIds <&> f
    where
        f searchTerm
            | Text.null searchTerm = holeKind ^. Sugar._LeafHole . Lens.to toLiteralTextEventMap
            | otherwise = mempty
