{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}
{-# LANGUAGE DerivingVia #-}
module Data.Vector.Vector2
    ( Vector2(Vector2)
    , curry, uncurry, sqrNorm
    )
where

import           Control.Applicative (liftA2)
import           Control.DeepSeq (NFData(..))
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import qualified Data.Aeson.Types as Aeson
import           Data.Binary (Binary(..))
import qualified Data.Tuple as Tuple
import           GHC.Generics (Generic, Generic1)
import           Generic.Data (Generically(..), Generically1(..))

import           Prelude hiding (curry, uncurry)

data Vector2 a = Vector2 !a !a
    deriving ( Generic, Generic1, Eq, Ord
             , Show, Read, Functor, Foldable, Traversable
             , NFData, Binary
             )
    deriving (Semigroup, Monoid) via Generically (Vector2 a)
    deriving Applicative via Generically1 Vector2
    -- Note the Ord instance is obviously not a mathematical one
    -- (Vectors aren't ordinals!). Useful to have in a binary search
    -- tree though.

instance Aeson.ToJSON a => Aeson.ToJSON (Vector2 a) where
    toJSON (Vector2 x y) = Aeson.toJSON (x, y)
instance Aeson.FromJSON a => Aeson.FromJSON (Vector2 a) where
    parseJSON json =
        Aeson.parseJSON json
        <&> Tuple.uncurry Vector2

instance a ~ b => Lens.Field1 (Vector2 a) (Vector2 b) a b where
    _1 f (Vector2 x y) = (`Vector2` y) <$> Lens.indexed f (0 :: Int) x
instance a ~ b => Lens.Field2 (Vector2 a) (Vector2 b) a b where
    _2 f (Vector2 x y) = Vector2 x <$> Lens.indexed f (1 :: Int) y

-- Taken almost verbatim from QuickCheck's instance for (a, b)
-- instance Arbitrary a => Arbitrary (Vector2 a) where
--   arbitrary = liftA2 Vector2 arbitrary arbitrary
--   shrink (Vector2 x y) = [ Vector2 x' y | x' <- shrink x ] ++
--                          [ Vector2 x y' | y' <- shrink y ]

-- instance Ix a => Ix (Vector2 a) where
--   range (start, stop) = uncurry (liftA2 Vector2) $ liftA2 (Prelude.curry range) start stop
--   inRange (start, stop) = uncurry (&&) . liftA3 (Prelude.curry inRange) start stop
--   index (Vector2 l t, Vector2 r b) (Vector2 x y) = (x' - l') * (b' + 1 - t') + y' - t'
--     where
--       (x', y', l', t', b') = (indexw x, indexh y, indexw l, indexh t, indexh b)
--       indexw = index (l, r)
--       indexh = index (t, b)

-- TODO: QuickCheck this:
-- prop_range :: Ix a => (Vector2 a, Vector2 a) -> Bool
-- prop_range r = map (index r) vectors == [0..length vectors-1]
--   where
--     vectors = range r

curry :: (Vector2 a -> b) -> a -> a -> b
curry f x y = f (Vector2 x y)

uncurry :: (a -> a -> b) -> Vector2 a -> b
uncurry f (Vector2 x y) = f x y

sqrNorm :: Num a => Vector2 a -> a
sqrNorm = uncurry (+) . (^ (2::Int))

-- An improper Num instance, for convenience
instance Num a => Num (Vector2 a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs = fmap abs
    negate = fmap negate
    signum = fmap signum
    fromInteger = pure . fromInteger

instance Fractional a => Fractional (Vector2 a) where
    (/) = liftA2 (/)
    fromRational = pure . fromRational
