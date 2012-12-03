{-# OPTIONS -fno-warn-orphans #-}
module Data.Monoid.Instances () where

import Control.Applicative ((<$>))
import Data.Binary(Binary(..))
import qualified Data.Monoid as Monoid

instance Binary Monoid.Any where
  get = Monoid.Any <$> get
  put (Monoid.Any x) = put x
