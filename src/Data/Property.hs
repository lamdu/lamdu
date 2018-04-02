{-# LANGUAGE TemplateHaskell, Rank2Types #-}
module Data.Property
    ( Property(..), pVal, pSet, value, set
    , compose, pureCompose, composeLens
    , modify_, pureModify

    , MkProperty(..), MkProperty', mkProperty
    , getP, setP, modP
    ) where

import           Control.Lens (Lens')
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Monad ((<=<))

import           Prelude

data Property m a = Property
    { _pVal :: a
    , _pSet :: a -> m ()
    }
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

newtype MkProperty im am a = MkProperty { _mkProperty :: im (Property am a) }
Lens.makeLenses ''MkProperty

type MkProperty' m = MkProperty m m

getP :: Functor im => MkProperty im am a -> im a
getP = fmap value . (^. mkProperty)

setP :: Monad m => MkProperty m m a -> a -> m ()
setP (MkProperty mkProp) val = do
    prop <- mkProp
    set prop val

modP :: Monad m => MkProperty m m a -> (a -> a) -> m ()
modP (MkProperty mkProp) f = do
    prop <- mkProp
    pureModify prop f

