{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell, StandaloneDeriving, OverloadedStrings #-}
module Graphics.DrawingCombinators.Utils
    ( Image
    , square
    , scale, translate
    , Draw.clearRender
    ) where

import           Control.Monad (void)
import           Data.Aeson.TH (deriveJSON)
import           Data.Aeson.Types (defaultOptions)
import           Data.Vector.Vector2 (Vector2(..))
import           Foreign.C.Types.Instances ()
import           GHC.Generics (Generic)
import qualified Graphics.DrawingCombinators as Draw

import           Prelude

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
