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
import qualified Data.Aeson.TH.Extended as JsonTH
import           Data.Text.Encoding (encodeUtf8)
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Align (TextWidget)
import           GUI.Momentu.Animation (AnimId)
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.Glue as Glue
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Options as Options
import           GUI.Momentu.State (Gui)
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextView as TextView

import           Lamdu.Prelude

data Style = Style
    { _indentBarWidth :: Double
    , _indentBarGap :: Double
    , _indentBarColor :: Draw.Color
    } deriving (Eq, Show)
JsonTH.derivePrefixed "_" ''Style
Lens.makeLenses ''Style

class HasStyle env where style :: Lens' env Style
instance HasStyle Style where style = id

disambiguators ::
    ( MonadReader env m, Functor f, HasStyle env, Spacer.HasStdSpacing env
    , Glue.HasTexts env
    ) =>
    m (AnimId -> Gui Options.Disambiguators f)
disambiguators =
    do
        h <- addParens
        v <- indent
        Options.Disambiguators <$> h <*> v & pure

addParens ::
    (MonadReader env m, TextView.HasStyle env, Glue.HasTexts env, Functor f) =>
    m (AnimId -> TextWidget f -> TextWidget f)
addParens =
    do
        (Glue.Poly (|||)) <- Glue.mkPoly ?? Glue.Horizontal
        textStyle <- Lens.view TextView.style
        (preLabel, postLabel) <-
            Lens.view Dir.layoutDir <&>
            \case
            Dir.LeftToRight -> ("(", ")")
            Dir.RightToLeft -> (")", "(")
        pure $ \myId w ->
            let paren t = TextView.make textStyle t (myId ++ [encodeUtf8 t])
            in  paren preLabel ||| w ||| paren postLabel

indent ::
    ( MonadReader env m, Functor f, HasStyle env, Spacer.HasStdSpacing env
    , Glue.HasTexts env
    ) =>
    m (AnimId -> Gui Responsive f -> Gui Responsive f)
indent =
    do
        bWidth <- totalBarWidth
        let reduceWidth =
                Responsive.rNarrow . Lens.argument .
                Responsive.layoutWidth
                -~ bWidth
        makeBar <- indentBar
        (|||) <- Glue.mkGlue ?? Glue.Horizontal
        let f myId w = makeBar (w ^. Element.height) myId ||| w
        pure $ \myId -> (Responsive.alignedWidget %~ f myId) . reduceWidth

totalBarWidth :: (MonadReader env m, HasStyle env, Spacer.HasStdSpacing env) => m Double
totalBarWidth =
    do
        s <- Lens.view style
        stdSpace <- Spacer.getSpaceSize <&> (^. _1)
        stdSpace * (s ^. indentBarWidth + s ^. indentBarGap) & pure

indentBar ::
    ( MonadReader env m, HasStyle env, Spacer.HasStdSpacing env
    , Glue.HasTexts env
    ) =>
    m (Widget.R -> AnimId -> View)
indentBar =
    do
        s <- Lens.view style
        stdSpace <- Spacer.getSpaceSize <&> (^. _1)
        (|||) <- Glue.mkGlue ?? Glue.Horizontal
        pure $ \height myId ->
            let bar =
                    Spacer.make (Vector2 barWidth height)
                    & Draw.backgroundColor bgAnimId (s ^. indentBarColor)
                barWidth = stdSpace * s ^. indentBarWidth
                gapWidth = stdSpace * s ^. indentBarGap
                bgAnimId = myId ++ ["("]
            in  bar ||| Spacer.make (Vector2 gapWidth 0)

boxSpacedDisambiguated ::
    ( MonadReader env m, Applicative f, HasStyle env, Spacer.HasStdSpacing env
    , Glue.HasTexts env
    ) =>
    m (AnimId -> [Gui Responsive f] -> Gui Responsive f)
boxSpacedDisambiguated = boxSpacedMDisamb <&> Lens.argument %~ Just

boxSpacedMDisamb ::
    ( MonadReader env m, Applicative f, HasStyle env, Spacer.HasStdSpacing env
    , Glue.HasTexts env
    ) =>
    m (Maybe AnimId -> [Gui Responsive f] -> Gui Responsive f)
boxSpacedMDisamb =
    do
        disamb <- disambiguators
        b <- Options.boxSpaced
        pure (b . maybe Options.disambiguationNone disamb)
