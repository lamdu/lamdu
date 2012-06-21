{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Editor.ITransaction
  ( ITransaction, runITransaction
  , transaction
  ) where

{- Inner transaction -}

import Control.Applicative (Applicative)
import Data.Store.Transaction (Transaction)
import qualified Data.AtFieldTH as AtFieldTH

newtype ITransaction t m a = ITransaction {
  unITransaction :: Transaction t m a
  }
  deriving (Functor, Applicative, Monad)
AtFieldTH.make ''ITransaction

runITransaction :: ITransaction t m a -> Transaction t m a
runITransaction = unITransaction

transaction :: Transaction t m a -> ITransaction t m a
transaction = ITransaction
