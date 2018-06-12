-- | Responsive layout for expressions express the hierarchy using parentheses and indentation,
-- as is customary in many programming languages and in mathematics.

{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module GUI.Momentu.Responsive.Expression
    ( Style(..), indentBarWidth, indentBarGap, indentBarColor
    , HasStyle(..)
    , disambiguators, boxSpacedDisambiguated, boxSpacedMDisamb, indent
    , addParens
    ) where

import qualified Control.Lens as Lens
import           Data.Aeson.TH (deriveJSON)
import qualified Data.Aeson.Types as Aeson
import           Data.List.Lens (prefixed)
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
import qualified GUI.Momentu.State as State
import           GUI.Momentu.View (View)
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextView as TextView

import           Lamdu.Prelude

data Style = Style
    { _indentBarWidth :: Double
    , _indentBarGap :: Double
    , _indentBarColor :: Draw.Color
    } deriving (Eq, Show)
deriveJSON Aeson.defaultOptions
    {Aeson.fieldLabelModifier = (^?! prefixed "_")}
    ''Style
Lens.makeLenses ''Style

class HasStyle env where style :: Lens' env Style
instance HasStyle Style where style = id

disambiguators ::
    (MonadReader env m, HasStyle env, Spacer.HasStdSpacing env, Functor f) =>
    m (AnimId -> Options.Disambiguators (f State.Update))
disambiguators =
    do
        h <- addParens
        v <- indent
        Options.Disambiguators <$> h <*> v & pure

addParens ::
    (MonadReader env m, TextView.HasStyle env, Functor f) =>
    m (AnimId -> WithTextPos (Widget (f State.Update)) -> WithTextPos (Widget (f State.Update)))
addParens =
    Lens.view TextView.style
    <&> \textStyle myId w ->
    let paren t = TextView.make textStyle t (myId ++ [encodeUtf8 t])
    in  paren "(" /|/ w /|/ paren ")"

indent ::
    (MonadReader env m, HasStyle env, Spacer.HasStdSpacing env, Functor f) =>
    m (AnimId -> Responsive (f State.Update) -> Responsive (f State.Update))
indent =
    do
        bWidth <- totalBarWidth
        let reduceWidth =
                Responsive.rNarrow . Lens.argument .
                Responsive.layoutWidth
                -~ bWidth
        makeBar <- indentBar
        let f myId w = makeBar (w ^. Element.height) myId /|/ w
        pure $ \myId -> (Responsive.alignedWidget %~ f myId) . reduceWidth

totalBarWidth :: (MonadReader env m, HasStyle env, Spacer.HasStdSpacing env) => m Double
totalBarWidth =
    do
        s <- Lens.view style
        stdSpace <- Spacer.getSpaceSize <&> (^. _1)
        stdSpace * (s ^. indentBarWidth + s ^. indentBarGap) & pure

indentBar ::
    (MonadReader env m, HasStyle env, Spacer.HasStdSpacing env) =>
    m (Widget.R -> AnimId -> View)
indentBar =
    do
        s <- Lens.view style
        stdSpace <- Spacer.getSpaceSize <&> (^. _1)
        pure $ \height myId ->
            let bar =
                    Spacer.make (Vector2 barWidth height)
                    & Draw.backgroundColor bgAnimId (s ^. indentBarColor)
                barWidth = stdSpace * s ^. indentBarWidth
                gapWidth = stdSpace * s ^. indentBarGap
                bgAnimId = myId ++ ["("]
            in  bar /|/ Spacer.make (Vector2 gapWidth 0)

boxSpacedDisambiguated ::
    (MonadReader env m, HasStyle env, Spacer.HasStdSpacing env, Functor f) =>
    m (AnimId -> [Responsive (f State.Update)] -> Responsive (f State.Update))
boxSpacedDisambiguated = boxSpacedMDisamb <&> Lens.argument %~ Just

boxSpacedMDisamb ::
    (MonadReader env m, HasStyle env, Spacer.HasStdSpacing env, Functor f) =>
    m (Maybe AnimId -> [Responsive (f State.Update)] -> Responsive (f State.Update))
boxSpacedMDisamb =
    do
        disamb <- disambiguators
        b <- Options.boxSpaced
        pure (b . maybe Options.disambiguationNone disamb)
