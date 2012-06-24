{-# LANGUAGE TemplateHaskell, DeriveFunctor #-}

module Graphics.UI.Bottle.Sized (Sized, mkSized, fromSize, requestedSize, atRequestedSize, atFromSize, align) where

import Control.Applicative ((<$>), (<*>))
import Data.MRUMemo (memo)
import Data.Vector.Vector2 (Vector2)
import Graphics.UI.Bottle.Animation (R)
import Graphics.UI.Bottle.SizeRange (Size, SizeRange)
import qualified Data.AtFieldTH as AtFieldTH
import qualified Graphics.UI.Bottle.SizeRange as SizeRange

data Sized a = Sized
    { sRequestedSize :: SizeRange
    , sFromSize :: Size -> a
    }
    deriving (Functor)
AtFieldTH.make ''Sized

mkSized :: SizeRange -> (Size -> a) -> Sized a
mkSized sr = Sized sr . memo

requestedSize :: Sized a -> SizeRange
requestedSize = sRequestedSize

fromSize :: Sized a -> Size -> a
fromSize = sFromSize

atRequestedSize :: (SizeRange -> SizeRange) -> Sized a -> Sized a
atRequestedSize = atSRequestedSize

atFromSize :: ((Size -> a) -> (Size -> b)) -> Sized a -> Sized b
atFromSize f = atSFromSize (memo . f)

align :: (Vector2 R -> a -> a) -> Vector2 R -> Sized a -> Sized a
align translate ratio sized =
  atFromSize ((g . SizeRange.srMaxSize . requestedSize) sized) sized
  where
    g maxSize mkSize size =
      translate pos . mkSize $
      cap <$> maxSize <*> size
      where
        pos = mkPos <$> maxSize <*> ratio <*> size
        mkPos Nothing _ _ = 0
        mkPos (Just maxSz) r sz = r * (sz - min maxSz sz)
        cap Nothing sz = sz
        cap (Just maxSz) sz = min maxSz sz
