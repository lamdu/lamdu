{-# OPTIONS -Wall #-}
module Graphics.DrawingCombinators.Utils (Image, square) where

import Control.Monad(void)
import qualified Graphics.DrawingCombinators as Draw

type Image = Draw.Image ()

square :: Image
square = void $ Draw.convexPoly [ (0, 0), (1, 0), (1, 1), (0, 1) ]
