{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Lamdu.GUI.ExpressionEdit.HoleEdit.LiteralText
    ( makeLiteralTextEventMap
    ) where

import           Data.Functor.Identity (Identity(..))
import           Data.Store.Transaction (Transaction)
import           GUI.Momentu (MetaKey(..))
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.MetaKey as MetaKey
import qualified GUI.Momentu.State as GuiState
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction

toLiteralTextKeys :: [MetaKey]
toLiteralTextKeys =
    [ MetaKey.shift MetaKey.Key'Apostrophe
    , MetaKey MetaKey.noMods MetaKey.Key'Apostrophe
    ]

makeLiteralTextEventMap ::
    Monad m =>
    Sugar.Hole (T m) (Sugar.Expression name (T m) a) ->
    EventMap (T m GuiState.Update)
makeLiteralTextEventMap hole =
    do
        (_score, mkResult) <- Sugar.LiteralText (Identity "") & hole ^. Sugar.holeOptionLiteral
        result <- mkResult
        result ^. Sugar.holeResultPick
        case result ^? Sugar.holeResultConverted . Sugar.rBody . Sugar._BodyFragment . Sugar.fExpr of
            Just arg -> arg
            _ -> result ^. Sugar.holeResultConverted
            ^. Sugar.rPayload . Sugar.plEntityId
            & WidgetIds.fromEntityId
            & WidgetIds.literalTextEditOf
            & pure
    & E.keysEventMapMovesCursor toLiteralTextKeys (E.Doc ["Edit", "Create Text Literal"])
