-- | Responsive layout for expressions express the hierarchy using parentheses and indentation,
-- as is customary in many programming languages and in mathematics.

{-# LANGUAGE TemplateHaskell #-}
module GUI.Momentu.Responsive.Expression
    ( Style(..), indentBarWidth, indentBarGap, indentBarColor
    , disambiguators, mDisambiguators
    , boxSpacedMDisamb, indent
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
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextView as TextView

import           GUI.Momentu.Prelude

data Style = Style
    { _indentBarWidth :: Double
    , _indentBarGap :: Double
    , _indentBarColor :: Draw.Color
    } deriving (Eq, Show)
JsonTH.derivePrefixed "_" ''Style
Lens.makeLenses ''Style

disambiguators ::
    ( MonadReader env m, Functor f, Has Style env, Spacer.HasStdSpacing env
    , Has Dir.Layout env
    ) =>
    m (AnimId -> Options.Disambiguators f)
disambiguators =
    do
        h <- addParens
        v <- indent
        Options.Disambiguators <$> h <*> v & pure

mDisambiguators ::
    ( MonadReader env m, Functor f, Has Style env, Spacer.HasStdSpacing env
    , Has Dir.Layout env
    ) =>
    m (Maybe AnimId -> Options.Disambiguators f)
mDisambiguators = disambiguators <&> maybe Options.disambiguationNone

addParens ::
    ( MonadReader env m, Has TextView.Style env, Functor f, Has Dir.Layout env
    ) =>
    m (AnimId -> TextWidget f -> TextWidget f)
addParens =
    Lens.view id <&>
    \env myId w ->
    let paren t = TextView.make env t (myId ++ [encodeUtf8 t])
    in  paren "(" ||| w ||| paren ")"
    where
        Glue.Poly (|||) = Glue.mkPoly Dir.LeftToRight Glue.Horizontal

indent ::
    ( MonadReader env m, Functor f, Has Style env, Spacer.HasStdSpacing env
    , Has Dir.Layout env
    ) =>
    m (AnimId -> Responsive f -> Responsive f)
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

totalBarWidth :: (MonadReader env m, Has Style env, Spacer.HasStdSpacing env) => m Double
totalBarWidth =
    do
        s <- Lens.view has
        stdSpace <- Spacer.getSpaceSize <&> (^. _1)
        stdSpace * (s ^. indentBarWidth + s ^. indentBarGap) & pure

indentBar ::
    ( MonadReader env m, Has Style env, Spacer.HasStdSpacing env
    , Has Dir.Layout env
    ) =>
    m (Widget.R -> AnimId -> View)
indentBar =
    do
        s <- Lens.view has
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

boxSpacedMDisamb ::
    ( MonadReader env m, Applicative f, Has Style env, Spacer.HasStdSpacing env
    , Glue.HasTexts env
    ) =>
    m (Maybe AnimId -> [Responsive f] -> Responsive f)
boxSpacedMDisamb = (.) <$> Options.boxSpaced <*> mDisambiguators
