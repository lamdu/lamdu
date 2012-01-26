{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
module Editor.Data (Definition(..), Parameter(..), Expression(..), atDefParameters, atDefBody)
where

import Data.Binary (Binary(..))
import Data.Derive.Binary(makeBinary)
import Data.DeriveTH(derive)
import Data.Store.IRef (IRef)
import qualified Data.AtFieldTH as AtFieldTH

data Parameter = Parameter
  deriving (Eq, Ord, Read, Show)
derive makeBinary ''Parameter

data Expression = Expression
  deriving (Eq, Ord, Read, Show)
derive makeBinary ''Expression

data Definition = Definition {
  defParameters :: [IRef Parameter],
  defBody :: IRef Expression
  }
  deriving (Eq, Ord, Read, Show)
derive makeBinary ''Definition
AtFieldTH.make ''Definition
