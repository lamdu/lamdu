module Lamdu.CodeEdit.Sugar.Config
  ( SugarConfig(..)
  ) where

import Control.Applicative ((<$>), (<*>))
import Data.Binary (Binary(..))
import Data.Foldable (sequenceA_)
import qualified Lamdu.Data.IRef as DataIRef

data SugarConfig t = SugarConfig
  { cons :: DataIRef.DefI t
  , nil :: DataIRef.DefI t
  }
instance Binary (SugarConfig t) where
  get = SugarConfig <$> get <*> get
  put (SugarConfig x y) = sequenceA_ [put x, put y]
