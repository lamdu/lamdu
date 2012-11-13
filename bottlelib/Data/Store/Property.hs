{-# LANGUAGE TemplateHaskell, Rank2Types #-}
module Data.Store.Property
  ( Property(..), pVal, pSet, value, set
  , compose, pureCompose, composeLens
  , modify, modify_, pureModify
  , list
  ) where

import Control.Monad ((<=<))
import qualified Control.Lens as Lens
import qualified Control.Lens.TH as LensTH

data Property m a = Property {
  _pVal :: a,
  _pSet :: a -> m ()
  }
LensTH.makeLenses ''Property

value :: Property m a -> a
value = Lens.view pVal

set :: Property m a -> a -> m ()
set = Lens.view pSet

modify :: Monad m => Property m a -> (a -> m (a, b)) -> m b
modify (Property val setter) f = do
  (newValue, res) <- f val
  setter newValue
  return res

modify_ :: Monad m => Property m a -> (a -> m a) -> m ()
modify_ (Property val setter) f = setter =<< f val

pureModify :: Monad m => Property m a -> (a -> a) -> m ()
pureModify prop = modify_ prop . (return .)

compose ::
  Monad m => (a -> b) -> (b -> m a) ->
  Property m a -> Property m b
compose aToB bToA (Property val setter) =
  Property (aToB val) (setter <=< bToA)

pureCompose ::
  Monad m => (a -> b) -> (b -> a) -> Property m a -> Property m b
pureCompose ab ba = compose ab (return . ba)

composeLens :: Lens.SimpleLens a b -> Property m a -> Property m b
composeLens lens (Property val setter) =
  Property (Lens.view lens val) (setter . flip (Lens.set lens) val)

list :: Property m [a] -> [Property m a]
list (Property vals setter) =
  zipWith mkProp vals [0..]
  where
    mkProp val i =
      Property val (setter . (pre ++) . (: post))
      where
        (pre, _: post) = splitAt i vals
