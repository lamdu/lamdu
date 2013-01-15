{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fno-warn-orphans #-}
module Data.Monoid.Instances () where

import Control.DeepSeq (NFData(..))
import Data.Binary (Binary(..))
import Data.Derive.Binary (makeBinary)
import Data.Derive.NFData (makeNFData)
import Data.DeriveTH (derive)
import Data.Monoid (Any(..))

derive makeBinary ''Any
derive makeNFData ''Any
