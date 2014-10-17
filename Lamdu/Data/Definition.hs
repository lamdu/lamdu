{-# LANGUAGE DeriveGeneric, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Lamdu.Data.Definition
  ( FFIName(..)
  , Builtin(..)
  , Content(..)
  , ExportedType(..)
  , Body(..), bodyContent, bodyType
  , Definition(..), defBody, defPayload
  ) where

import Control.Applicative ((<$>))
import Control.Lens (Lens, Lens')
import Data.Binary (Binary(..))
import Data.Foldable (Foldable(..))
import Data.Traversable (Traversable(..))
import GHC.Generics (Generic)
import Lamdu.Expr.Scheme (Scheme)

data FFIName = FFIName
  { fModule :: [String]
  , fName :: String
  } deriving (Generic, Eq, Ord)

instance Show FFIName where
  show (FFIName path name) = concatMap (++".") path ++ name

data Builtin = Builtin
  { bName :: FFIName
  } deriving (Generic, Eq, Ord, Show)

-- TODO: Perhaps parameterize over Expr payload instead?
data Content valExpr
  = ContentExpr valExpr
  | ContentBuiltin Builtin
  deriving (Generic, Eq, Ord, Show, Functor, Foldable, Traversable)

data ExportedType = NoExportedType | ExportedType Scheme
  deriving (Generic, Show)

data Body valExpr = Body
  { _bodyContent :: Content valExpr
  , _bodyType :: ExportedType
  } deriving (Generic, Show, Functor, Foldable, Traversable)

data Definition valExpr a = Definition
  { _defBody :: Body valExpr
  , _defPayload :: a
  } deriving (Generic, Functor, Foldable, Traversable)

instance Binary FFIName
instance Binary Builtin
instance Binary valExpr => Binary (Content valExpr)
instance Binary ExportedType
instance Binary valExpr => Binary (Body valExpr)
instance (Binary valExpr, Binary a) => Binary (Definition valExpr a)

bodyContent :: Lens (Body a) (Body b) (Content a) (Content b)
bodyContent f (Body c t) = (`Body` t) <$> f c

bodyType :: Lens' (Body a) ExportedType
bodyType f (Body c t) = Body c <$> f t

defBody :: Lens (Definition s a) (Definition t a) (Body s) (Body t)
defBody f (Definition b p) = (`Definition` p) <$> f b

defPayload :: Lens (Definition expr a) (Definition expr b) a b
defPayload f (Definition b p) = Definition b <$> f p
