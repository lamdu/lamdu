{-# LANGUAGE RankNTypes #-}
module Lamdu.Sugar.Names.CPS
    ( CPS(..), runcps
    , liftCPS
    ) where

import Lamdu.Prelude

newtype CPS m a = CPS { unCPS :: forall r. m r -> m (a, r) }
    deriving (Functor)

runcps :: Applicative f => CPS f b -> f b
runcps act = unCPS act (pure ()) <&> fst

liftCPS :: Monad m => m a -> CPS m a
liftCPS action = CPS $ \inner -> (,) <$> action <*> inner

instance Functor m => Applicative (CPS m) where
    pure x = CPS $ fmap ((,) x)
    CPS cpsf <*> CPS cpsx =
        CPS (fmap foo . cpsf . cpsx)
        where
            foo (f, (x, r)) = (f x, r)
