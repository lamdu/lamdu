{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Lamdu.GUI.ExpressionEdit.HoleEdit.Literal
    ( makeLiteralEventMap
    ) where

import           Data.Functor.Identity (Identity(..))
import           GUI.Momentu (MetaKey(..), WidgetId)
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.MetaKey as MetaKey
import qualified GUI.Momentu.State as GuiState
import qualified Lamdu.CharClassification as Chars
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

toLiteralTextKeys :: [MetaKey]
toLiteralTextKeys =
    [ MetaKey.shift MetaKey.Key'Apostrophe
    , MetaKey MetaKey.noMods MetaKey.Key'Apostrophe
    ]

makeLiteral ::
    Monad f =>
    Sugar.OptionLiteral f (Sugar.Expression name f a) ->
    Sugar.Literal Identity ->
    f WidgetId
makeLiteral optionLiteral lit =
    do
        (_score, mkResult) <- optionLiteral lit
        result <- mkResult
        result ^. Sugar.holeResultPick
        case result ^? Sugar.holeResultConverted . Sugar.rBody . Sugar._BodyFragment . Sugar.fExpr of
            Just arg -> arg
            _ -> result ^. Sugar.holeResultConverted
            ^. Sugar.rPayload . Sugar.plEntityId
            & WidgetIds.fromEntityId
            & WidgetIds.literalEditOf
            & pure

makeLiteralEventMap ::
    Monad f =>
    Sugar.OptionLiteral f (Sugar.Expression name f a) ->
    EventMap (f GuiState.Update)
makeLiteralEventMap optionLiteral =
    E.keysEventMapMovesCursor toLiteralTextKeys (E.Doc ["Edit", "Literal Text"])
    (makeLiteral optionLiteral (Sugar.LiteralText (Identity "")))
    <>
    E.charGroup (Just "Digit") (E.Doc ["Edit", "Literal Number"]) Chars.digit
    (fmap GuiState.updateCursor . makeLiteral optionLiteral . Sugar.LiteralNum . Identity . read . (: []))
