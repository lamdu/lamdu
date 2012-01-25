{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TypeOperators #-}

module Data.Store.Property(
    Property(..), composeLabel, compose, pureCompose,
    modify, modify_, pureModify)
where

import           Control.Monad     (liftM, (>=>))
import           Data.Record.Label ((:->))
import qualified Data.Record.Label as Label

data Property m a = Property {
  get :: m a,
  set :: a -> m ()
  }

modify :: Monad m => Property m a -> (a -> m (a, b)) -> m b
modify prop f = do
  (newValue, res) <- f =<< get prop
  set prop newValue
  return res

modify_ :: Monad m => Property m a -> (a -> m a) -> m ()
modify_ prop f = set prop =<< f =<< get prop

pureModify :: Monad m => Property m a -> (a -> a) -> m ()
pureModify prop = modify_ prop . (return .)

inFields ::
  (m a -> m b) -> ((a -> m ()) -> b -> m ()) ->
  Property m a -> Property m b
inFields onGet onSet (Property getter setter) =
  Property (onGet getter) (onSet setter)

compose ::
  Monad m => (a -> m b) -> (b -> m a) ->
  Property m a -> Property m b
compose aToB bToA = inFields (>>= aToB) (bToA >=>)

pureCompose ::
  Monad m => (a -> b) -> (b -> a) -> Property m a -> Property m b
pureCompose ab ba = compose (return . ab) (return . ba)

infixl 5 `composeLabel`
composeLabel :: Monad m => (a :-> b) -> Property m a -> Property m b
composeLabel label (Property getter setter) =
  Property getter' setter'
  where
    getter' = Label.getL label `liftM` getter
    setter' x = setter . Label.setL label x =<< getter
