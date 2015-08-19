{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude, StandaloneDeriving, DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
module Graphics.DrawingCombinators.Utils
    ( Image
    , square
    , TextSize(..)
    , textHeight, textSize
    , textLinesSize
    , drawText, drawTextLines
    , backgroundColor
    , scale, translate
    , Draw.clearRenderSized
    ) where

import           Control.Applicative (liftA2)
import           Control.Lens.Operators
import           Control.Monad (void)
import           Data.Aeson (ToJSON(..), FromJSON(..))
import           Data.List (genericLength)
import           Data.Vector.Vector2 (Vector2(..))
import           Foreign.C.Types.Instances ()
import           GHC.Generics (Generic)
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.DrawingCombinators ((%%))

import           Prelude.Compat

type Image = Draw.Image ()

deriving instance Read Draw.Color
deriving instance Generic Draw.Color

instance ToJSON Draw.Color
instance FromJSON Draw.Color

scale :: Vector2 Draw.R -> Draw.Affine
scale (Vector2 x y) = Draw.scale x y

translate :: Vector2 Draw.R -> Draw.Affine
translate (Vector2 x y) = Draw.translate (x, y)

square :: Image
square = void $ Draw.convexPoly [ (0, 0), (1, 0), (1, 1), (0, 1) ]

textHeight :: Draw.R
textHeight = 2

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
textSize font str = (`Vector2` textHeight) <$> textWidth font str

descender :: Draw.Font -> Draw.R
descender = Draw.fontDescender

drawText :: Draw.Font -> String -> Image
drawText font str =
    str
    & Draw.text font
    & void
    -- Text is normally at height -0.5..1.5.  We move it to be -textHeight..0
    & (translate (Vector2 0 (-textHeight - descender font)) %%)
    -- We want to reverse it so that higher y is down, and it is also
    -- moved to 0..2
    & (scale (Vector2 1 (-1)) %%)

textLinesHeight :: [String] -> Draw.R
textLinesHeight = (textHeight *) . genericLength

textLinesWidth :: Draw.Font -> [String] -> TextSize Draw.R
textLinesWidth font = fmap maximum . traverse (textWidth font)

textLinesSize :: Draw.Font -> [String] -> TextSize (Vector2 Draw.R)
textLinesSize font textLines =
    (`Vector2` textLinesHeight textLines) <$>
    textLinesWidth font textLines

drawTextLines :: Draw.Font -> [String] -> Image
drawTextLines font =
    foldr (step . drawText font) mempty
    where
        step lineImage restImage =
            mappend lineImage $
            translate (Vector2 0 textHeight) %% restImage

backgroundColor :: Draw.Color -> Vector2 Draw.R -> Image -> Image
backgroundColor color size image =
    mappend image $
    Draw.tint color $ scale size %% square
