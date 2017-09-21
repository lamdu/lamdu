-- | IOTrans is an applicative that allows 3 phases of execution:
-- A. IO action (e.g: to read a JSON file)
-- B. A transaction (allowed to depend on A)
-- C. A final *main-thread* IO action (allowed to depend on B) (e.g:
--    to write a JSON file)

{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, DeriveFunctor #-}

module Lamdu.GUI.IOTrans
    ( IOTrans(..), ioTrans, liftTrans
    ) where

import           Control.Applicative (liftA2)
import qualified Control.Lens as Lens
import           Data.Orphans () -- Imported for Monoid (IO ()) instance
import           Data.Store.Transaction (Transaction)
import qualified GUI.Momentu.Main as Main
import           Lamdu.Prelude

type T = Transaction

newtype IOTrans m a = IOTrans { _ioTrans :: IO (T m (Main.EventResult a)) }
    deriving (Functor)
Lens.makeLenses ''IOTrans

instance Monad m => Applicative (IOTrans m) where
    pure = IOTrans . pure . pure . pure
    IOTrans f <*> IOTrans x = (liftA2 . liftA2 . liftA2) ($) f x & IOTrans

liftTrans :: Functor m => T m a -> IOTrans m a
liftTrans = IOTrans . pure . fmap pure
