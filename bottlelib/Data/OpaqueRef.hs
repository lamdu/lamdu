{-# LANGUAGE TemplateHaskell, TypeFamilies, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Data.OpaqueRef
  ( Ref
    , unsafeAsInt
      -- ^ Don't use unless you absolutely have to
    , unsafeFromInt
      -- ^ Don't use unless you absolutely have to
  , RefMap, emptyRefMap
    , refMapToList, refMapFromList
    , refMapSingleton
    , unsafeRefMapItems
  , RefSet, emptyRefSet
    , refSetToList, refSetFromList
    , refSetSingleton
    , unsafeRefSetKeys
  , Fresh, initialFresh
    , freshRef
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Monad.Trans.State (StateT(..))
import Control.MonadA (MonadA)
import Data.Foldable (Foldable)
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Monoid (Monoid(..))
import Data.Traversable (Traversable(..))
import Prelude hiding (lookup)
import qualified Control.Lens as Lens
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

newtype Ref = MkRef { unsafeAsInt :: Int }
  deriving (Eq, Ord)

unsafeFromInt :: Int -> Ref
unsafeFromInt = MkRef

newtype Fresh = MkFresh Ref

Lens.makeIso ''Ref
Lens.makeIso ''Fresh

initialFresh :: Fresh
initialFresh = MkFresh (MkRef 0)

freshRef :: MonadA m => StateT Fresh m Ref
freshRef = Lens.use (Lens.from mkFresh) <* (Lens.from (mkRef . mkFresh) += 1)

newtype RefSet = RefSet IntSet
  deriving (Monoid)
newtype RefMap v = RefMap (IntMap v)
  deriving (Functor, Foldable, Traversable, Monoid)

instance Show Ref where
  show (MkRef x) = 'R':show x

emptyRefMap :: RefMap v
emptyRefMap = RefMap IntMap.empty

unsafeRefMapItems :: Lens.Traversal (RefMap a) (RefMap b) (Ref, a) (Ref, b)
unsafeRefMapItems f = fmap refMapFromList . traverse f . refMapToList

refMapSingleton :: Ref -> a -> RefMap a
refMapSingleton (MkRef x) = RefMap . IntMap.singleton x

refMapToList :: RefMap a -> [(Ref, a)]
refMapToList (RefMap x) = x & IntMap.toList <&> Lens._1 %~ MkRef

refMapFromList :: [(Ref, a)] -> RefMap a
refMapFromList = RefMap . IntMap.fromList . (Lens.mapped . Lens._1 %~ (^. Lens.from mkRef))

emptyRefSet :: RefSet
emptyRefSet = RefSet IntSet.empty

refSetFromList :: [Ref] -> RefSet
refSetFromList = RefSet . IntSet.fromList . map (^. Lens.from mkRef)

refSetToList :: RefSet -> [Ref]
refSetToList (RefSet x) = x & IntSet.toList <&> MkRef

unsafeRefSetKeys :: Lens.Traversal' RefSet Ref
unsafeRefSetKeys f = fmap refSetFromList . traverse f . refSetToList

refSetSingleton :: Ref -> RefSet
refSetSingleton (MkRef x) = RefSet $ IntSet.singleton x

type instance Lens.Index (RefMap a) = Ref
type instance Lens.IxValue (RefMap a) = a

convertIndex :: Lens.Indexable i p => (i1 -> i) -> p a b -> Lens.Indexed i1 a b
convertIndex onIndex f = Lens.Indexed (Lens.indexed f . onIndex)

instance Lens.At (RefMap a) where
  at (MkRef k) f (RefMap x) = RefMap <$> Lens.at k (convertIndex MkRef f) x

instance Applicative f => Lens.Ixed f (RefMap a) where
  ix (MkRef k) f (RefMap x) = RefMap <$> Lens.ix k (convertIndex MkRef f) x

instance Lens.FunctorWithIndex Ref RefMap where
  imap f (RefMap x) = RefMap $ Lens.imap (f . MkRef) x

instance Lens.FoldableWithIndex Ref RefMap where
  ifoldMap f (RefMap x) = Lens.ifoldMap (f . MkRef) x

instance Lens.TraversableWithIndex Ref RefMap where
  itraverse f (RefMap x) = RefMap <$> Lens.itraverse (f . MkRef) x
