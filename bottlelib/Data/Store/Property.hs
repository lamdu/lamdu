{-# LANGUAGE TemplateHaskell, Rank2Types #-}
module Data.Store.Property
  ( Property(..), pVal, pSet, value, set
  , compose, pureCompose, composeLens
  , modify, modify_, pureModify
  , list
  ) where

import Control.Lens (Lens')
import Control.Lens.Operators
import Control.Monad ((<=<))
import Control.MonadA (MonadA)
import qualified Control.Lens as Lens

data Property m a = Property {
  _pVal :: a,
  _pSet :: a -> m ()
  }
Lens.makeLenses ''Property

value :: Property m a -> a
value = (^. pVal)

set :: Property m a -> a -> m ()
set = (^. pSet)

modify :: MonadA m => Property m a -> (a -> m (a, b)) -> m b
modify (Property val setter) f = do
  (newValue, res) <- f val
  setter newValue
  return res

modify_ :: MonadA m => Property m a -> (a -> m a) -> m ()
modify_ (Property val setter) f = setter =<< f val

pureModify :: MonadA m => Property m a -> (a -> a) -> m ()
pureModify prop = modify_ prop . (return .)

compose ::
  MonadA m => (a -> b) -> (b -> m a) ->
  Property m a -> Property m b
compose aToB bToA (Property val setter) =
  Property (aToB val) (setter <=< bToA)

pureCompose ::
  MonadA m => (a -> b) -> (b -> a) -> Property m a -> Property m b
pureCompose ab ba = compose ab (return . ba)

composeLens :: Lens' a b -> Property m a -> Property m b
composeLens lens (Property val setter) =
  Property (val ^. lens) (setter . flip (lens .~) val)

list :: Property m [a] -> [Property m a]
list (Property vals setter) =
  zipWith mkProp vals [0..]
  where
    mkProp val i =
      Property val (setter . (pre ++) . (: post))
      where
        (pre, _: post) = splitAt i vals
