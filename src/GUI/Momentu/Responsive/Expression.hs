-- | Responsive layout for expressions express the hierarchy using parentheses and indentation,
-- as is customary in many programming languages and in mathematics.

{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, OverloadedStrings #-}

module GUI.Momentu.Responsive.Expression
    ( Style(..), HasStyle(..)
    , disambiguators, boxSpacedDisambiguated, boxSpacedMDisamb, indent
    ) where

import qualified Control.Lens as Lens
import           Data.Aeson.TH (deriveJSON)
import           Data.Aeson.Types (defaultOptions)
import           Data.Text.Encoding (encodeUtf8)
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Align (WithTextPos)
import           GUI.Momentu.Animation (AnimId)
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.Glue ((/|/))
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Options as Options
import           GUI.Momentu.View (View)
import           GUI.Momentu.Widget (Widget, EventResult)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextView as TextView

import           Lamdu.Prelude

data Style = Style
    { indentBarWidth :: Double
    , indentBarGap :: Double
    , indentBarColor :: Draw.Color
    } deriving (Eq, Show)
deriveJSON defaultOptions ''Style

class HasStyle env where style :: Lens' env Style
instance HasStyle Style where style = id

disambiguators ::
    (MonadReader env m, HasStyle env, Spacer.HasStdSpacing env, Functor f) =>
    m (AnimId -> Options.Disambiguators (f EventResult))
disambiguators =
    do
        h <- addParens
        v <- indent
        Options.Disambiguators <$> h <*> v & return

addParens ::
    (MonadReader env m, TextView.HasStyle env, Functor f) =>
    m (AnimId -> WithTextPos (Widget (f Widget.EventResult)) -> WithTextPos (Widget (f Widget.EventResult)))
addParens =
    do
        textStyle <- Lens.view TextView.style
        let f ::
                Functor f =>
                AnimId -> WithTextPos (Widget (f Widget.EventResult)) -> WithTextPos (Widget (f Widget.EventResult))
            f myId w =
                paren "(" /|/ w /|/ paren ")"
                where
                    paren t = TextView.make textStyle t (myId ++ [encodeUtf8 t])
        return f

indent ::
    (MonadReader env m, HasStyle env, Spacer.HasStdSpacing env, Functor f) =>
    m (AnimId -> Responsive (f EventResult) -> Responsive (f EventResult))
indent =
    do
        bWidth <- totalBarWidth
        let reduceWidth =
                Responsive.render . Lens.argument .
                Responsive.layoutMode . Responsive.modeWidths
                -~ bWidth
        makeBar <- indentBar
        let f :: Functor f => AnimId -> WithTextPos (Widget (f EventResult)) -> WithTextPos (Widget (f EventResult))
            f myId w = makeBar (w ^. Element.height) myId /|/ w
        return (\myId -> (Responsive.alignedWidget %~ f myId) . reduceWidth)

totalBarWidth :: (MonadReader env m, HasStyle env, Spacer.HasStdSpacing env) => m Double
totalBarWidth =
    do
        s <- Lens.view style
        stdSpace <- Spacer.getSpaceSize <&> (^. _1)
        stdSpace * (indentBarWidth s + indentBarGap s) & return

indentBar ::
    (MonadReader env m, HasStyle env, Spacer.HasStdSpacing env) =>
    m (Widget.R -> AnimId -> View)
indentBar =
    do
        s <- Lens.view style
        stdSpace <- Spacer.getSpaceSize <&> (^. _1)
        let f :: Widget.R -> AnimId -> View
            f height myId =
                bar /|/ Spacer.make (Vector2 gapWidth 0)
                where
                    bar =
                        Spacer.make (Vector2 barWidth height)
                        & Draw.backgroundColor bgAnimId (indentBarColor s)
                    barWidth = stdSpace * indentBarWidth s
                    gapWidth = stdSpace * indentBarGap s
                    bgAnimId = myId ++ ["("]
        return f

boxSpacedDisambiguated ::
    (MonadReader env m, HasStyle env, Spacer.HasStdSpacing env, Functor f) =>
    m (AnimId -> [Responsive (f EventResult)] -> Responsive (f EventResult))
boxSpacedDisambiguated = boxSpacedMDisamb <&> Lens.argument %~ Just

boxSpacedMDisamb ::
    (MonadReader env m, HasStyle env, Spacer.HasStdSpacing env, Functor f) =>
    m (Maybe AnimId -> [Responsive (f Widget.EventResult)] -> Responsive (f Widget.EventResult))
boxSpacedMDisamb =
    do
        disamb <- disambiguators
        b <- Options.boxSpaced
        return (b . maybe Options.disambiguationNone disamb)
