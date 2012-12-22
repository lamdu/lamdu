{-# LANGUAGE TemplateHaskell, DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveDataTypeable #-}
module Lamdu.Data.Definition
  ( Definition(..), defBody, defType
  , Body(..)
  , FFIName(..)
  , Builtin(..)
  ) where

import Data.Binary (Binary(..))
import Data.Binary.Get (getWord8)
import Data.Binary.Put (putWord8)
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Foldable (Foldable(..))
import Data.Traversable (Traversable(..))
import Data.Typeable (Typeable)
import qualified Control.Lens.TH as LensTH

data FFIName = FFIName
  { fModule :: [String]
  , fName :: String
  } deriving (Eq, Ord)
derive makeBinary ''FFIName

instance Show FFIName where
  show (FFIName path name) = concatMap (++".") path ++ name

data Builtin = Builtin
  { bName :: FFIName
  } deriving (Eq, Ord, Show)
derive makeBinary ''Builtin

data Body expr
  = BodyExpression expr
  | BodyBuiltin Builtin
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Definition expr = Definition
  { _defBody :: Body expr
  , _defType :: expr
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Typeable)

LensTH.makeLenses ''Definition
derive makeBinary ''Body
derive makeBinary ''Definition
