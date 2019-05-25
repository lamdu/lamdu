{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell, StandaloneDeriving #-}
module Graphics.DrawingCombinators.Extended
    ( module Graphics.DrawingCombinators
    , square
    , scaleV, translateV
    , openSprite
    ) where

import           Control.Monad (void)
import           Control.Exception (Exception, SomeException(..), catch, throwIO)
import           Data.Aeson.TH (deriveJSON)
import           Data.Aeson.Types (defaultOptions)
import           Data.Vector.Vector2 (Vector2(..))
import           Foreign.C.Types.Instances ()
import           GHC.Generics (Generic)
import qualified Graphics.DrawingCombinators as DrawingCombinators
import           Graphics.DrawingCombinators hiding (openSprite)

import           Prelude

data OpenSpriteFailed = OpenSpriteFailed FilePath SomeException
    deriving (Show, Exception)

deriving instance Read Color
deriving instance Generic Color
deriveJSON defaultOptions ''Color

scaleV :: Vector2 R -> Affine
scaleV (Vector2 x y) = scale x y

roundR :: R -> R
roundR x = fromIntegral (round x :: Int)

translateV :: Vector2 R -> Affine
-- Translating to fractional positions causes drawing artefacts for
-- text, so we round all positions to nearest pixel boundary
translateV (Vector2 x y) = translate (roundR x, roundR y)

square :: Image ()
square = void $ convexPoly [ (0, 0), (1, 0), (1, 1), (0, 1) ]

openSprite :: FilePath -> IO Sprite
openSprite path =
    DrawingCombinators.openSprite path
    `catch` \e@SomeException{} -> throwIO (OpenSpriteFailed path e)
