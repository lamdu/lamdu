{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude, StandaloneDeriving, DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
module Graphics.DrawingCombinators.Utils
    ( Image
    , square
    , TextSize(..)
    , Draw.fontHeight, textSize
    , drawText
    , scale, translate
    , Draw.clearRender
    ) where

import           Control.Applicative (liftA2)
import           Control.Lens.Operators
import           Control.Monad (void)
import qualified Data.Aeson.Types as Aeson
import           Data.Vector.Vector2 (Vector2(..))
import           Foreign.C.Types.Instances ()
import           GHC.Generics (Generic)
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.DrawingCombinators ((%%))

import           Prelude.Compat

type Image = Draw.Image ()

deriving instance Read Draw.Color
deriving instance Generic Draw.Color

instance Aeson.ToJSON Draw.Color where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions
instance Aeson.FromJSON Draw.Color

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
    { bounding :: a
    , advance :: a
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

textWidth :: Draw.Font -> String -> TextSize Draw.R
textWidth font str =
    TextSize
    { bounding =
      Draw.textBoundingWidth font str
      -- max with advance because spaces are counted only in advance
      -- but not the bounding width
      & max adv
    , advance = adv
    }
    where
        adv = Draw.textAdvance font str

textSize :: Draw.Font -> String -> TextSize (Vector2 Draw.R)
textSize font str = (`Vector2` Draw.fontHeight font) <$> textWidth font str

{-# NOINLINE drawText #-}
drawText :: Draw.Font -> Draw.TextAttrs -> String -> Image
drawText font textAttrs str =
    Draw.text font str textAttrs
    & void
    -- Text is normally at height -0.5..1.5.  We move it to be -textHeight..0
    & (translate (Vector2 0 (-Draw.fontHeight font - Draw.fontDescender font)) %%)
    -- We want to reverse it so that higher y is down, and it is also
    -- moved to 0..2
    & (scale (Vector2 1 (-1)) %%)
