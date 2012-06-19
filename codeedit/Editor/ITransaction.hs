{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Editor.ITransaction
  ( ITransaction, runITransaction
  , transaction, setP, pureModifyP
  ) where

{- Inner transaction -}

import Control.Applicative (Applicative)
import Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.Store.Property as Property

newtype ITransaction t m a = ITransaction {
  unITransaction :: Transaction t m a
  }
  deriving (Functor, Applicative, Monad)
AtFieldTH.make ''ITransaction

runITransaction :: ITransaction t m a -> Transaction t m a
runITransaction = unITransaction

transaction :: Transaction t m a -> ITransaction t m a
transaction = ITransaction

setP :: Transaction.Property t m a -> a -> ITransaction t m ()
setP prop value = transaction $ Property.set prop value

pureModifyP :: Monad m => Transaction.Property t m a -> (a -> a) -> ITransaction t m ()
pureModifyP prop f = transaction $ Property.pureModify prop f
