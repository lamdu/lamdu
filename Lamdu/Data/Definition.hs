{-# LANGUAGE TemplateHaskell, DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveDataTypeable #-}
module Lamdu.Data.Definition
  ( FFIName(..)
  , Builtin(..)
  , Content(..)
  , Body(..), bodyContent, bodyType
  , Definition(..), defBody, defPayload
  ) where

import Data.Binary (Binary(..))
import Data.Binary.Get (getWord8)
import Data.Binary.Put (putWord8)
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Foldable (Foldable(..))
import Data.Traversable (Traversable(..))
import Data.Typeable (Typeable)
import qualified Control.Lens as Lens

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

data Content expr
  = ContentExpr expr
  | ContentBuiltin Builtin
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Body expr = Body
  { _bodyContent :: Content expr
  , _bodyType :: expr
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Typeable)

data Definition expr a = Definition
  { _defBody :: Body expr
  , _defPayload :: a
  }

Lens.makeLenses ''Body
Lens.makeLenses ''Definition
derive makeBinary ''Body
derive makeBinary ''Content
derive makeBinary ''Definition
