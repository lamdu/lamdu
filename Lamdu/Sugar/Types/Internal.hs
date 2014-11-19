{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lamdu.Sugar.Types.Internal
  ( StorePoint(..), T
  , NoStored(..), Stored
  ) where

import Data.Binary (Binary)
import Data.Store.Transaction (Transaction)
import qualified Lamdu.Expr.IRef as ExprIRef

type T = Transaction

data NoStored = NoStored
type Stored m = ExprIRef.ValIProperty m

newtype StorePoint t = StorePoint { unStorePoint :: ExprIRef.ValI t }
  deriving (Eq, Ord, Binary)
