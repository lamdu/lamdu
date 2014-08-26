{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TemplateHaskell #-}
module Lamdu.Sugar.Types.Internal
  ( StorePoint(..), T
  , NoInferred(..), Inferred
  , NoStored(..), Stored
  ) where

import Data.Binary (Binary)
import Data.Store.Transaction (Transaction)
import Data.Typeable (Typeable)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Infer as Infer

type T = Transaction

data NoInferred = NoInferred
type Inferred = Infer.Payload

data NoStored = NoStored
type Stored m = ExprIRef.ValIProperty m

newtype StorePoint t = StorePoint { unStorePoint :: ExprIRef.ValI t }
  deriving (Eq, Ord, Binary, Typeable)
