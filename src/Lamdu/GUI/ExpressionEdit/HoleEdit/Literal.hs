{-# LANGUAGE RankNTypes #-}

module Lamdu.GUI.ExpressionEdit.HoleEdit.Literal
    ( makeLiteralEventMap
    ) where

import           Data.Functor.Identity (Identity(..))
import           Data.Tree.Diverse (ann, val)
import           GUI.Momentu (WidgetId)
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.State (Gui)
import qualified GUI.Momentu.State as GuiState
import qualified Lamdu.CharClassification as Chars
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

makeLiteral ::
    (Monad i, Monad o) =>
    (forall x. i x -> o x) ->
    Sugar.OptionLiteral name i o ->
    Sugar.Literal Identity ->
    o WidgetId
makeLiteral io optionLiteral lit =
    do
        result <-
            do
                (_score, mkResult) <- optionLiteral lit
                mkResult
            & io
        result ^. Sugar.holeResultPick
        case result ^? Sugar.holeResultConverted . val . Sugar._BinderExpr . Sugar._BodyFragment . Sugar.fExpr of
            Just arg -> arg ^. ann
            _ -> result ^. Sugar.holeResultConverted . SugarLens.binderResultExpr
            ^. Sugar.plEntityId
            & WidgetIds.fromEntityId
            & WidgetIds.literalEditOf
            & pure

makeLiteralEventMap ::
    (Monad i, Monad o) =>
    (forall x. i x -> o x) ->
    Sugar.OptionLiteral name i o ->
    Gui EventMap o
makeLiteralEventMap io optionLiteral =
    E.charGroup Nothing (E.Doc ["Edit", "Literal Text"]) "'\""
    (const (makeLiteral io optionLiteral (Sugar.LiteralText (Identity "")) <&> GuiState.updateCursor))
    <>
    E.charGroup (Just "Digit") (E.Doc ["Edit", "Literal Number"]) Chars.digit
    (fmap GuiState.updateCursor . makeLiteral io optionLiteral . Sugar.LiteralNum . Identity . read . (: []))
