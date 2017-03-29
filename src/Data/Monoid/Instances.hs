{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# OPTIONS -fno-warn-orphans #-}
module Data.Monoid.Instances () where

import Data.Binary (Binary)
import Data.Monoid (Any(..))

deriving instance Binary Any
