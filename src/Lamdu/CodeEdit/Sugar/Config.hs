{-# LANGUAGE TemplateHaskell #-}
module Lamdu.CodeEdit.Sugar.Config
  ( SugarConfig(..)
  ) where

import Data.Binary (Binary(..))
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import qualified Lamdu.Data.IRef as DataIRef

data SugarConfig t = SugarConfig
  { cons :: DataIRef.DefI t
  , nil :: DataIRef.DefI t
  }
derive makeBinary ''SugarConfig
