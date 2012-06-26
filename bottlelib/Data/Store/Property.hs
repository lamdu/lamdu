module Data.Store.Property
  ( Property(..)
  , composeLabel, compose, pureCompose
  , modify, modify_, pureModify
  , list
  ) where

import Control.Monad ((<=<))

data Property m a = Property {
  value :: a,
  set :: a -> m ()
  }

modify :: Monad m => Property m a -> (a -> m (a, b)) -> m b
modify prop f = do
  (newValue, res) <- f $ value prop
  set prop newValue
  return res

modify_ :: Monad m => Property m a -> (a -> m a) -> m ()
modify_ prop f = set prop <=< f $ value prop

pureModify :: Monad m => Property m a -> (a -> a) -> m ()
pureModify prop = modify_ prop . (return .)

inFields ::
  (a -> b) -> ((a -> m ()) -> b -> m ()) ->
  Property m a -> Property m b
inFields onGet onSet (Property getter setter) =
  Property (onGet getter) (onSet setter)

compose ::
  Monad m => (a -> b) -> (b -> m a) ->
  Property m a -> Property m b
compose aToB bToA = inFields aToB (<=< bToA)

pureCompose ::
  Monad m => (a -> b) -> (b -> a) -> Property m a -> Property m b
pureCompose ab ba = compose ab (return . ba)

composeLabel ::
  Monad m => (rec -> field) -> (field -> rec -> rec) ->
  Property m rec -> Property m field
composeLabel getField setField (Property val setter) =
  Property getter' setter'
  where
    getter' = getField val
    setter' = setter . flip setField val

list :: Property m [a] -> [Property m a]
list (Property vals setter) =
  zipWith mkProp vals [0..]
  where
    mkProp val i =
      Property val (setter . (pre ++) . (: post))
      where
        (pre, _: post) = splitAt i vals
