{-# LANGUAGE TemplateHaskell, TypeFamilies, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Data.OpaqueRef
  ( Ref
    , unsafeAsInt
      -- ^ Don't use unless you absolutely have to
    , unsafeFromInt
      -- ^ Don't use unless you absolutely have to
  , RefMap, refMapEmpty
    , refMapFromList, refMapFromListWith
    , refMapIntersectionWith
    , refMapUnionWith, refMapSingleton
    , refMapFilter, refMapMinViewWithKey
    , refMapKeysSet, refMapDifference
    , refMapNull
    , refMapUnmaintainedLookup
    , unsafeRefMapItems
  , RefSet, refSetEmpty
    , refSetToList, refSetFromList
    , refSetSingleton
    , refSetNull
    , unsafeRefSetKeys
  , Fresh, initialFresh
    , freshRef
  ) where

import Prelude hiding (lookup)

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.State (StateT(..))
import Control.MonadA (MonadA)
import Data.Binary (Binary(..))
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Foldable (Foldable)
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Monoid (Monoid(..))
import Data.Traversable (Traversable(..))
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

newtype Ref p = MkRef { unsafeAsInt :: Int }
  deriving (Eq, Ord, Binary)

unsafeFromInt :: Int -> Ref p
unsafeFromInt = MkRef

newtype Fresh p = MkFresh (Ref p)
  deriving (Binary)

Lens.makeIso ''Ref
Lens.makeIso ''Fresh

initialFresh :: Fresh p
initialFresh = MkFresh (MkRef 0)

freshRef :: MonadA m => StateT (Fresh p) m (Ref p)
freshRef = Lens.use (Lens.from mkFresh) <* (Lens.from (mkRef . mkFresh) += 1)

newtype RefSet p = RefSet IntSet
  deriving (Monoid, Show)
derive makeBinary ''RefSet

newtype RefMap p v = RefMap (IntMap v)
  deriving (Functor, Foldable, Traversable, Monoid, Show)
derive makeBinary ''RefMap

instance Show (Ref p) where
  show (MkRef x) = 'R':show x

refMapEmpty :: RefMap p v
refMapEmpty = RefMap IntMap.empty

unsafeRefMapItems :: Lens.Traversal (RefMap pa a) (RefMap pb b) (Ref pa, a) (Ref pb, b)
unsafeRefMapItems f = fmap refMapFromList . traverse f . refMapToList

refMapIntersectionWith :: (a -> b -> c) -> RefMap p a -> RefMap p b -> RefMap p c
refMapIntersectionWith f (RefMap x) (RefMap y) = RefMap $ IntMap.intersectionWith f x y

refMapUnionWith :: (a -> a -> a) -> RefMap p a -> RefMap p a -> RefMap p a
refMapUnionWith f (RefMap x) (RefMap y) = RefMap $ IntMap.unionWith f x y

refMapSingleton :: Ref p -> a -> RefMap p a
refMapSingleton (MkRef x) = RefMap . IntMap.singleton x

refMapFilter :: (a -> Bool) -> RefMap p a -> RefMap p a
refMapFilter p (RefMap x) = RefMap $ IntMap.filter p x

refMapKeysSet :: RefMap p a -> RefSet p
refMapKeysSet (RefMap x) = RefSet $ IntMap.keysSet x

refMapDifference :: RefMap p a -> RefMap p b -> RefMap p a
refMapDifference (RefMap x) (RefMap y) = RefMap $ IntMap.difference x y

refMapNull :: RefMap p a -> Bool
refMapNull (RefMap x) = IntMap.null x

refMapMinViewWithKey :: RefMap p a -> Maybe ((Ref p, a), RefMap p a)
refMapMinViewWithKey (RefMap x) =
  IntMap.minViewWithKey x
  <&> Lens._1 . Lens._1 %~ MkRef
  <&> Lens._2 %~ RefMap

refMapToList :: RefMap p a -> [(Ref p, a)]
refMapToList (RefMap x) = x & IntMap.toList <&> Lens._1 %~ MkRef

refMapFromList :: [(Ref p, a)] -> RefMap p a
refMapFromList = RefMap . IntMap.fromList . (Lens.mapped . Lens._1 %~ (^. Lens.from mkRef))

refMapFromListWith :: (a -> a -> a) -> [(Ref p, a)] -> RefMap p a
refMapFromListWith combine = RefMap . IntMap.fromListWith combine . (Lens.mapped . Lens._1 %~ (^. Lens.from mkRef))

refMapUnmaintainedLookup ::
  MonadA m => (Ref p -> m (Ref p)) -> Ref p -> StateT (RefMap p a) m (Ref p, Maybe a)
refMapUnmaintainedLookup doLookup ref =
  tryNow ref $ do
    rep <- lift $ doLookup ref
    tryNow rep $ do
      oldMap <- State.get
      newMap <- lift $ oldMap & unsafeRefMapItems . Lens._1 %%~ doLookup
      State.put newMap
      tryNow rep $ return (rep, Nothing)
  where
    tryNow r notFound = do
      mFound <- State.gets (^. Lens.at r)
      case mFound of
        Just found -> return $ (r, Just found)
        Nothing -> notFound

refSetEmpty :: RefSet p
refSetEmpty = RefSet IntSet.empty

refSetFromList :: [Ref p] -> RefSet p
refSetFromList = RefSet . IntSet.fromList . map (^. Lens.from mkRef)

refSetToList :: RefSet p -> [Ref p]
refSetToList (RefSet x) = x & IntSet.toList <&> MkRef

unsafeRefSetKeys :: Lens.Traversal' (RefSet p) (Ref p)
unsafeRefSetKeys f = fmap refSetFromList . traverse f . refSetToList

refSetSingleton :: Ref p -> RefSet p
refSetSingleton (MkRef x) = RefSet $ IntSet.singleton x

refSetNull :: RefSet p -> Bool
refSetNull (RefSet x) = IntSet.null x

type instance Lens.Index (RefSet p) = Ref p

type instance Lens.Index (RefMap p a) = Ref p
type instance Lens.IxValue (RefMap p a) = a

convertIndex :: Lens.Indexable i p => (i1 -> i) -> p a b -> Lens.Indexed i1 a b
convertIndex onIndex f = Lens.Indexed (Lens.indexed f . onIndex)

instance Functor f => Lens.Contains f (RefSet p) where
  contains mk@(MkRef k) f (RefSet x) =
    RefSet <$> Lens.contains k (convertIndex ((`asTypeOf` mk) . MkRef) f) x

instance Lens.At (RefMap p a) where
  at mk@(MkRef k) f (RefMap x) =
    RefMap <$> Lens.at k (convertIndex ((`asTypeOf` mk) . MkRef) f) x

instance Applicative f => Lens.Ixed f (RefMap p a) where
  ix mk@(MkRef k) f (RefMap x) =
    RefMap <$> Lens.ix k (convertIndex ((`asTypeOf` mk) . MkRef) f) x

instance Lens.FunctorWithIndex (Ref p) (RefMap p) where
  imap f (RefMap x) = RefMap $ Lens.imap (f . MkRef) x

instance Lens.FoldableWithIndex (Ref p) (RefMap p) where
  ifoldMap f (RefMap x) = Lens.ifoldMap (f . MkRef) x

instance Lens.TraversableWithIndex (Ref p) (RefMap p) where
  itraverse f (RefMap x) = RefMap <$> Lens.itraverse (f . MkRef) x
