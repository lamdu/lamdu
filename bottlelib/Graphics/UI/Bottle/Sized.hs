{-# LANGUAGE TemplateHaskell, DeriveFunctor #-}

module Graphics.UI.Bottle.Sized (Sized(..), atRequestedSize, atFromSize, align) where

import Control.Applicative ((<$>), (<*>))
import Data.Vector.Vector2 (Vector2)
import Graphics.UI.Bottle.SizeRange (Size, SizeRange)
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Data.AtFieldTH as AtFieldTH
import qualified Graphics.UI.Bottle.SizeRange as SizeRange

data Sized a = Sized
    { requestedSize :: SizeRange
    , fromSize :: Size -> a
    }
    deriving (Functor)

AtFieldTH.make ''Sized

align ::
  (Vector2 Anim.R -> a -> a) -> Vector2 Anim.R -> Sized a -> Sized a
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
