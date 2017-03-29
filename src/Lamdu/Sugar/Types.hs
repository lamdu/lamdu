{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Lamdu.Sugar.Types
    ( module Lamdu.Sugar.Types.Binder
    , module Lamdu.Sugar.Types.Expression
    , EntityId
    , Pane(..), paneDefinition, paneClose
    , WorkArea(..), waPanes, waRepl
    , Definition(..), drDefinitionState, drEntityId, drName, drBody, drDefI
    , DefinitionBody(..), _DefinitionBodyExpression, _DefinitionBodyBuiltin
    , DefinitionExpression(..), deContent, deTypeInfo
    , AcceptNewType(..)
    , DefinitionTypeInfo(..)
        , _DefinitionExportedTypeInfo
        , _DefinitionNewType
    , Anchors.PresentationMode(..)
    , Anchors.DefinitionState(..)
    , DefinitionBuiltin(..), biType, biName, biSetName
    , DefinitionU
    ) where

import qualified Control.Lens as Lens
import           Data.Store.Transaction (Transaction, MkProperty)
import           Data.UUID.Types (UUID)
import           Lamdu.Calc.Type.Scheme (Scheme)
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import           Lamdu.Sugar.Internal.EntityId (EntityId)
import           Lamdu.Sugar.Types.Binder
import           Lamdu.Sugar.Types.Expression
import           Lamdu.Expr.IRef (DefI)

import           Lamdu.Prelude

type T = Transaction

data AcceptNewType m = AcceptNewType
    { antOldExportedType :: Scheme
    , antNewInferredType :: Scheme
    , antAccept :: T m ()
    }

data DefinitionTypeInfo m
    = DefinitionExportedTypeInfo Scheme
    | DefinitionNewType (AcceptNewType m)

data DefinitionExpression name m expr = DefinitionExpression
    { _deTypeInfo :: DefinitionTypeInfo m
    , _deContent :: Binder name m expr
    } deriving (Functor, Foldable, Traversable)

data DefinitionBuiltin m = DefinitionBuiltin
    { _biName :: Definition.FFIName
    , _biSetName :: Definition.FFIName -> T m ()
    , _biType :: Scheme
    }

data DefinitionBody name m expr
    = DefinitionBodyExpression (DefinitionExpression name m expr)
    | DefinitionBodyBuiltin (DefinitionBuiltin m)
    deriving (Functor, Foldable, Traversable)

data Definition name m expr = Definition
    { _drName :: name
    , _drDefI :: DefI m
    , _drDefinitionState :: MkProperty m Anchors.DefinitionState
    , _drEntityId :: EntityId
    , _drBody :: DefinitionBody name m expr
    } deriving (Functor, Foldable, Traversable)

type DefinitionU m a = Definition UUID m (Expression UUID m a)

data Pane name m a = Pane
    { _paneDefinition :: Definition name m (Expression name m a)
    , _paneClose :: T m EntityId
    } deriving (Functor, Foldable, Traversable)

data WorkArea name m a = WorkArea
    { _waPanes :: [Pane name m a]
    , _waRepl :: Expression name m a
    } deriving (Functor, Foldable, Traversable)

Lens.makeLenses ''Definition
Lens.makeLenses ''DefinitionBuiltin
Lens.makeLenses ''DefinitionExpression
Lens.makeLenses ''Pane
Lens.makeLenses ''WorkArea
Lens.makePrisms ''DefinitionBody
Lens.makePrisms ''DefinitionTypeInfo
