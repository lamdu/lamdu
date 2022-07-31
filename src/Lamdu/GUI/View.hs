{-# LANGUAGE DerivingVia, GeneralisedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances #-}

module Lamdu.GUI.View
    ( ViewM, liftView
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Reader (ReaderT(..))
import qualified Data.Monoid as Monoid
import qualified Lamdu.GUI.Classes as C

import           Lamdu.Prelude

newtype ViewM env i a = ViewM (ReaderT env i a)
    deriving newtype (Functor, Applicative, Monad, MonadReader env)
    deriving (Semigroup, Monoid) via (Monoid.Ap (ViewM env i) a)

instance Monad i => C.InfoMonad (ViewM env i) i where liftInfo = ViewM . lift

instance Applicative i => C.SetTagName (ViewM env i) Proxy where setTagName = pure (const (const Proxy))

liftView :: (C.InfoMonad m i, MonadReader env m) => ViewM env i a -> m a
liftView (ViewM action) = Lens.view id >>= C.liftInfo . runReaderT action
