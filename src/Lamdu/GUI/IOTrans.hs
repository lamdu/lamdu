{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, KindSignatures, TupleSections #-}

module Lamdu.GUI.IOTrans
    ( IOTrans(..), ioTrans, trans, liftTrans, liftIO, liftIOT, liftTExecInMain
    ) where

import qualified Control.Lens as Lens
import           Data.Functor.Compose (Compose(..))
import           Data.Orphans () -- Imported for Monoid (IO ()) instance
import qualified GUI.Momentu.Main as Main
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

-- | IOTrans is an applicative that allows 3 phases of execution:
-- A. IO action (e.g: to read a JSON file)
-- B. A transaction (allowed to depend on A)
-- C. A final *main-thread* IO action (allowed to depend on B) (e.g:
--    to write a JSON file)
newtype IOTrans m a = IOTrans
    { _ioTrans :: Compose IO (Compose (T m) ((,) (Main.ExecuteInMainThread IO))) a
    } deriving (Functor, Applicative)
Lens.makeLenses ''IOTrans

trans ::
    Lens.Setter
    (IOTrans m a)
    (IOTrans n b)
    (T m (Main.ExecuteInMainThread IO, a))
    (T n (Main.ExecuteInMainThread IO, b))
trans f (IOTrans (Compose act)) =
    (Lens.mapped . Lens._Wrapped) f act
    <&> Compose
    <&> IOTrans

liftTrans :: Functor m => T m a -> IOTrans m a
liftTrans = IOTrans . Compose . pure . Compose . fmap pure

-- | IOTrans is not a Monad, so it isn't a MonadTrans. But it can lift IO actions
liftIO :: Monad m => IO a -> IOTrans m a
liftIO act = act <&> pure & Compose & IOTrans

liftIOT :: Functor m => IO (T m a) -> IOTrans m a
liftIOT = IOTrans . Compose . fmap (Compose . fmap pure)

-- | Run T / IO (IO in main thread)
liftTExecInMain :: Functor m => T m (Main.ExecuteInMainThread IO) -> IOTrans m ()
liftTExecInMain act =
    act
    <&> (, ())
    & Compose
    & pure
    & Compose
    & IOTrans
