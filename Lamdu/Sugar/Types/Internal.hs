{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lamdu.Sugar.Types.Internal
  ( StorePoint(..)
  , NoStored(..), Stored
  ) where

import Data.Binary (Binary)
import qualified Lamdu.Expr.IRef as ExprIRef

data NoStored = NoStored
type Stored m = ExprIRef.ValIProperty m

newtype StorePoint t = StorePoint { unStorePoint :: ExprIRef.ValI t }
  deriving (Eq, Ord, Binary)
