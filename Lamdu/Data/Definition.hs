{-# LANGUAGE NoImplicitPrelude, DeriveGeneric, DeriveFunctor, DeriveFoldable, DeriveTraversable, TemplateHaskell #-}
module Lamdu.Data.Definition
    ( FFIName(..)
    , Builtin(..)
      , builtinTags
    , ExportedType(..), _NoExportedType, _ExportedType
      , exportedTypeTags
    , Expr(..), expr, exprType, exprUsedDefinitions
      , exprTags
    , Body(..), _BodyExpr, _BodyBuiltin
      , bodyTags
    , Definition(..), defBody, defPayload
      , defTags
    ) where

import qualified Control.Lens as Lens
import           Data.Binary (Binary(..))
import           Data.Map (Map)
import           GHC.Generics (Generic)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Calc.Type.Scheme (Scheme)
import           Lamdu.Calc.Val (Var)
import           Lamdu.Expr.Lens (schemeTags)

import           Prelude.Compat

data FFIName = FFIName
    { fModule :: [String]
    , fName :: String
    } deriving (Generic, Eq, Ord)

instance Show FFIName where
    show (FFIName path name) = concatMap (++".") path ++ name

data Builtin = Builtin
    { bName :: FFIName
    , bType :: Scheme
    } deriving (Generic, Show)

data ExportedType = NoExportedType | ExportedType Scheme
    deriving (Generic, Show)

data Expr valExpr = Expr
    { _expr :: valExpr
    , _exprType :: ExportedType
    , _exprUsedDefinitions :: Map Var Scheme
    } deriving (Generic, Show, Functor, Foldable, Traversable)

data Body valExpr
    = BodyExpr (Expr valExpr)
    | BodyBuiltin Builtin
    deriving (Generic, Show, Functor, Foldable, Traversable)

data Definition valExpr a = Definition
    { _defBody :: Body valExpr
    , _defPayload :: a
    } deriving (Generic, Functor, Foldable, Traversable)

instance Binary FFIName
instance Binary Builtin
instance Binary ExportedType
instance Binary valExpr => Binary (Expr valExpr)
instance Binary valExpr => Binary (Body valExpr)
instance (Binary valExpr, Binary a) => Binary (Definition valExpr a)

Lens.makePrisms ''ExportedType
Lens.makePrisms ''Body
Lens.makeLenses ''Definition
Lens.makeLenses ''Expr

builtinTags :: Lens.Setter' Builtin T.Tag
builtinTags f (Builtin ffiName typ) = Builtin ffiName <$> schemeTags f typ

exportedTypeTags :: Lens.Setter' ExportedType T.Tag
exportedTypeTags = _ExportedType . schemeTags

exprTags :: Lens.Setter' (Expr valExpr) T.Tag
exprTags f (Expr val typ usedDefs) =
    Expr val
    <$> exportedTypeTags f typ
    <*> (traverse . schemeTags) f usedDefs

bodyTags :: Lens.Setter' (Body valExpr) T.Tag
bodyTags f (BodyExpr x) = BodyExpr <$> exprTags f x
bodyTags f (BodyBuiltin x) = BodyBuiltin <$> builtinTags f x

defTags :: Lens.Setter' (Definition valExpr a) T.Tag
defTags = defBody . bodyTags
