{-# LANGUAGE TemplateHaskell, Rank2Types #-}
module Data.Property
    ( Property(..), pVal, pSet, value, set
    , compose, pureCompose, composeLens
    , modify_, pureModify, hoist

    , MkProperty(..), MkProperty', mkProperty, prop
    , getP, setP, modP, mkHoist, mkHoist'

    , fromIORef
    ) where

import qualified Control.Lens as Lens
import           Control.Monad ((<=<))
import           Data.IORef (IORef, readIORef, writeIORef)

import           Lamdu.Prelude

data Property m a = Property
    { _pVal :: a
    , _pSet :: a -> m ()
    } deriving Generic
Lens.makeLenses ''Property

value :: Property m a -> a
value = (^. pVal)

set :: Functor m => Property m a -> a -> m (Property m a)
set (Property _ write) x = Property x write <$ write x

modify_ :: Monad m => Property m a -> (a -> m a) -> m ()
modify_ (Property val setter) f = setter =<< f val

pureModify :: Monad m => Property m a -> (a -> a) -> m ()
pureModify prop = modify_ prop . (pure .)

compose ::
    Monad m => (a -> b) -> (b -> m a) ->
    Property m a -> Property m b
compose aToB bToA (Property val setter) =
    Property (aToB val) (setter <=< bToA)

pureCompose ::
    Monad m => (a -> b) -> (b -> a) -> Property m a -> Property m b
pureCompose ab ba = compose ab (pure . ba)

composeLens :: Lens' a b -> Property m a -> Property m b
composeLens lens (Property val setter) =
    Property (val ^. lens) (setter . flip (lens .~) val)

hoist :: (m () -> m' ()) -> Property m a -> Property m' a
hoist f = pSet . Lens.mapped %~ f

-- MkProperty:

newtype MkProperty i o a = MkProperty { _mkProperty :: i (Property o a) }
Lens.makeLenses ''MkProperty

prop ::
    Functor i =>
    Lens.Setter
    (MkProperty i o a)
    (MkProperty i p b)
    (Property o a)
    (Property p b)
prop = mkProperty . Lens.mapped

type MkProperty' m = MkProperty m m

getP :: Functor i => MkProperty i o a -> i a
getP = fmap value . (^. mkProperty)

setP :: Monad m => MkProperty m m a -> a -> m ()
setP (MkProperty mkProp) val = do
    p <- mkProp
    set p val & void

modP :: Monad m => MkProperty m m a -> (a -> a) -> m ()
modP (MkProperty mkProp) f = do
    p <- mkProp
    pureModify p f

fromIORef :: IORef a -> MkProperty' IO a
fromIORef ref =
    MkProperty $
    readIORef ref
    <&> \val ->
        Property
        { _pVal = val
        , _pSet = writeIORef ref
        }

mkHoist ::
    Functor i' =>
    (forall x. i x -> i' x) -> (o () -> o' ()) ->
    MkProperty i o a -> MkProperty i' o' a
mkHoist i o x =
    x
    & mkProperty %~ i
    & mkProperty . Lens.mapped %~ hoist o

mkHoist' ::
    Functor m' =>
    (forall x. m x -> m' x) -> MkProperty m m a -> MkProperty m' m' a
mkHoist' f = mkHoist f f
