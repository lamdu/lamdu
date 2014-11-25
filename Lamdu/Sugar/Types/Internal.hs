{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lamdu.Sugar.Types.Internal
  ( StorePoint(..)
  ) where

import Data.Binary (Binary)
import qualified Lamdu.Expr.IRef as ExprIRef

newtype StorePoint t = StorePoint { unStorePoint :: ExprIRef.ValI t }
  deriving (Eq, Ord, Binary)
