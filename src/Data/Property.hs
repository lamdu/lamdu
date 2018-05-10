{-# LANGUAGE TemplateHaskell, Rank2Types #-}
module Data.Property
    ( Property(..), pVal, pSet, value, set
    , compose, pureCompose, composeLens
    , modify_, pureModify

    , MkProperty(..), MkProperty', mkProperty
    , getP, setP, modP
    ) where

import qualified Control.Lens as Lens
import           Control.Monad ((<=<))

import           Lamdu.Prelude

data Property m a = Property
    { _pVal :: a
    , _pSet :: a -> m ()
    } deriving Generic
Lens.makeLenses ''Property

value :: Property m a -> a
value = (^. pVal)

set :: Property m a -> a -> m ()
set = (^. pSet)

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

-- MkProperty:

newtype MkProperty i o a = MkProperty { _mkProperty :: i (Property o a) }
Lens.makeLenses ''MkProperty

type MkProperty' m = MkProperty m m

getP :: Functor i => MkProperty i o a -> i a
getP = fmap value . (^. mkProperty)

setP :: Monad m => MkProperty m m a -> a -> m ()
setP (MkProperty mkProp) val = do
    prop <- mkProp
    set prop val

modP :: Monad m => MkProperty m m a -> (a -> a) -> m ()
modP (MkProperty mkProp) f = do
    prop <- mkProp
    pureModify prop f

