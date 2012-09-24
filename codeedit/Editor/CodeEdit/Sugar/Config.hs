{-# LANGUAGE TemplateHaskell #-}

module Editor.CodeEdit.Sugar.Config
  ( SugarConfig(..)
  ) where

import Data.Binary (Binary(..))
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Editor.Data (DefinitionIRef)

data SugarConfig = SugarConfig
  { cons :: DefinitionIRef
  , nil :: DefinitionIRef
  }

derive makeBinary ''SugarConfig
