{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, StandaloneDeriving, DeriveTraversable, DeriveGeneric, OverloadedStrings #-}
module Graphics.DrawingCombinators.Utils
    ( Image
    , square
    , TextSize(..), bounding, advance
    , Draw.fontHeight, textSize
    , RenderedText(..), renderedTextSize, renderedText
    , renderText
    , scale, translate
    , Draw.clearRender
    ) where

import           Control.Applicative (liftA2)
import qualified Control.Lens as Lens
import           Data.Aeson.TH (deriveJSON)
import           Data.Aeson.Types (defaultOptions)
import qualified Data.Text as Text
import           Data.Vector.Vector2 (Vector2(..))
import           Foreign.C.Types.Instances ()
import           GHC.Generics (Generic)
import           Graphics.DrawingCombinators ((%%))
import qualified Graphics.DrawingCombinators as Draw

import           Lamdu.Prelude

type Image = Draw.Image ()

deriving instance Read Draw.Color
deriving instance Generic Draw.Color
deriveJSON defaultOptions ''Draw.Color

scale :: Vector2 Draw.R -> Draw.Affine
scale (Vector2 x y) = Draw.scale x y

roundR :: Draw.R -> Draw.R
roundR x = fromIntegral (round x :: Int)

translate :: Vector2 Draw.R -> Draw.Affine
-- Translating to fractional positions causes drawing artefacts for
-- text, so we round all positions to nearest pixel boundary
translate (Vector2 x y) = Draw.translate (roundR x, roundR y)

square :: Image
square = void $ Draw.convexPoly [ (0, 0), (1, 0), (1, 1), (0, 1) ]

data TextSize a = TextSize
    { _bounding :: a
    , _advance :: a
    } deriving (Functor, Foldable, Traversable)
instance Applicative TextSize where
    pure x = TextSize x x
    TextSize f1 f2 <*> TextSize x1 x2 = TextSize (f1 x1) (f2 x2)
instance Num a => Num (TextSize a) where
    fromInteger = pure . fromInteger
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    abs = fmap abs
    signum = fmap signum
    negate = fmap negate

Lens.makeLenses ''TextSize

textWidth :: Draw.Font -> Text -> TextSize Draw.R
textWidth font str =
    TextSize
    { _bounding =
      Draw.textBoundingWidth font str
      -- max with advance because spaces are counted only in advance
      -- but not the bounding width
      & max adv
    , _advance = adv
    }
    where
        adv = Draw.textAdvance font str

data RenderedText a = RenderedText
    { _renderedTextSize :: TextSize (Vector2 Draw.R)
    , _renderedText :: a
    }
Lens.makeLenses ''RenderedText

textSize :: Draw.Font -> Text -> TextSize (Vector2 Draw.R)
textSize font str =
    (`Vector2` height) <$> textWidth font str
    where
        height = Draw.fontHeight font * fromIntegral numLines
        numLines = 1 + Text.count "\n" str

renderText :: Draw.Font -> Draw.TextAttrs -> Text -> RenderedText Image
renderText font textAttrs str =
    RenderedText
    { _renderedTextSize = textSize font str
    , _renderedText =
        Draw.text font str textAttrs
        & void
        -- Text is normally at height -0.5..1.5.  We move it to be -textHeight..0
        & (translate (Vector2 0 (-Draw.fontHeight font - Draw.fontDescender font)) %%)
        -- We want to reverse it so that higher y is down, and it is also
        -- moved to 0..2
        & (scale (Vector2 1 (-1)) %%)
    }
