{-# LANGUAGE CPP, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# OPTIONS -fno-warn-orphans #-}
module Data.Monoid.Instances () where

import Data.Binary (Binary)
import Data.Monoid (Any(..))

#if !MIN_VERSION_deepseq(1,4,0)
import Control.DeepSeq (NFData)

deriving instance NFData Any
#endif

deriving instance Binary Any
