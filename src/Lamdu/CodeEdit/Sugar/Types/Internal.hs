{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Lamdu.CodeEdit.Sugar.Types.Internal
  ( StorePoint(..), T, CT
  ) where

import Control.Monad.Trans.State (StateT)
import Data.Binary (Binary)
import Data.Cache (Cache)
import Data.Store.Transaction (Transaction)
import Data.Typeable (Typeable)
import qualified Lamdu.Data.Expression.IRef as ExprIRef

type T = Transaction
type CT m = StateT Cache (T m)

newtype StorePoint t = StorePoint { unStorePoint :: ExprIRef.ExpressionI t }
  deriving (Eq, Ord, Binary, Typeable)
