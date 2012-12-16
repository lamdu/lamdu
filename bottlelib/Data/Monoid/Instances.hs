{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fno-warn-orphans #-}
module Data.Monoid.Instances () where

import Data.Binary(Binary(..))
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Monoid (Any(..))

derive makeBinary ''Any
