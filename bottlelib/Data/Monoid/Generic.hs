{-# LANGUAGE TypeOperators, FlexibleContexts #-}
module Data.Monoid.Generic (GMonoid, def_mempty, def_mappend) where

-- Use GHC 7.4's Generic class for creating Monoid instances
import GHC.Generics (Generic, M1(..), U1(..), K1(..), (:*:)(..), Rep, from, to)
import Data.Monoid (Monoid(..))

-- Generic version of Monoid. We'll need to create an instance for each of the
-- Generic types.
class GMonoid f where
    gmempty :: f a
    gmappend :: f a -> f a -> f a

-- Nullary constructors. The instance is simple: mempty is the constructor,
-- mappend is the constructor.
instance GMonoid U1 where
    gmempty = U1
    gmappend U1 U1 = U1

-- Products
instance (GMonoid a, GMonoid b) => GMonoid (a :*: b) where
    -- Product of two gmempty values
    gmempty = gmempty :*: gmempty

    -- Apply gmappend recursively to the left and right and create a new
    -- product.
    gmappend (a :*: x) (b :*: y) = gmappend a b :*: gmappend x y

-- Metadata: just a passthrough
instance GMonoid a => GMonoid (M1 i c a) where
    gmempty = M1 gmempty
    gmappend (M1 x) (M1 y) = M1 $ gmappend x y

-- Arguments: now use the real Monoid methods. We're essentially just
-- wrapping/unwrapping here.
--
-- Note that this forces all of the fields in our datatype to be instances of
-- Monoid, which is what we should expect.
instance Monoid a => GMonoid (K1 i a) where
    gmempty = K1 mempty
    gmappend (K1 x) (K1 y) = K1 $ mappend x y

{-# ANN module "HLint: ignore Use camelCase" #-}

-- Default implementations of mempty and mappend using gmempty and gmappend.
-- All we do is use @to@ and @from@ to wrap and unwrap.
def_mempty :: (Generic a, GMonoid (Rep a)) => a
def_mempty = to gmempty

def_mappend :: (Generic a, GMonoid (Rep a)) => a -> a -> a
def_mappend x y = to $ from x `gmappend` from y
