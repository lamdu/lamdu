-- | Has-by-type
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Data.Has (Has(..)) where

import Control.Lens (Lens')

import Prelude.Compat (id)

class Has t env where has :: Lens' env t

instance Has a a where has = id
