{-# LANGUAGE TemplateHaskell #-}

module Editor.CodeEdit.Sugar.Config
  ( SugarConfig(..)
  ) where

import Data.Binary (Binary(..))
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import qualified Editor.Data.IRef as DataIRef

data SugarConfig = SugarConfig
  { cons :: DataIRef.DefinitionIRef
  , nil :: DataIRef.DefinitionIRef
  }
derive makeBinary ''SugarConfig
