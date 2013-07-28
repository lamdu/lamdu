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

newtype Ref p = MkRef { unsafeAsInt :: Int }
  deriving (Eq, Ord)

unsafeFromInt :: Int -> Ref p
unsafeFromInt = MkRef

newtype Fresh p = MkFresh (Ref p)

Lens.makeIso ''Ref
Lens.makeIso ''Fresh

initialFresh :: Fresh p
initialFresh = MkFresh (MkRef 0)

freshRef :: MonadA m => StateT (Fresh p) m (Ref p)
freshRef = Lens.use (Lens.from mkFresh) <* (Lens.from (mkRef . mkFresh) += 1)

newtype RefSet p = RefSet IntSet
  deriving (Monoid)
newtype RefMap p v = RefMap (IntMap v)
  deriving (Functor, Foldable, Traversable, Monoid)

instance Show (Ref p) where
  show (MkRef x) = 'R':show x

emptyRefMap :: RefMap p v
emptyRefMap = RefMap IntMap.empty

unsafeRefMapItems :: Lens.Traversal (RefMap pa a) (RefMap pb b) (Ref pa, a) (Ref pb, b)
unsafeRefMapItems f = fmap refMapFromList . traverse f . refMapToList

refMapSingleton :: Ref p -> a -> RefMap p a
refMapSingleton (MkRef x) = RefMap . IntMap.singleton x

refMapToList :: RefMap p a -> [(Ref p, a)]
refMapToList (RefMap x) = x & IntMap.toList <&> Lens._1 %~ MkRef

refMapFromList :: [(Ref p, a)] -> RefMap p a
refMapFromList = RefMap . IntMap.fromList . (Lens.mapped . Lens._1 %~ (^. Lens.from mkRef))

emptyRefSet :: RefSet p
emptyRefSet = RefSet IntSet.empty

refSetFromList :: [Ref p] -> RefSet p
refSetFromList = RefSet . IntSet.fromList . map (^. Lens.from mkRef)

refSetToList :: RefSet p -> [Ref p]
refSetToList (RefSet x) = x & IntSet.toList <&> MkRef

unsafeRefSetKeys :: Lens.Traversal' (RefSet p) (Ref p)
unsafeRefSetKeys f = fmap refSetFromList . traverse f . refSetToList

refSetSingleton :: Ref p -> RefSet p
refSetSingleton (MkRef x) = RefSet $ IntSet.singleton x

type instance Lens.Index (RefMap p a) = Ref p
type instance Lens.IxValue (RefMap p a) = a

convertIndex :: Lens.Indexable i p => (i1 -> i) -> p a b -> Lens.Indexed i1 a b
convertIndex onIndex f = Lens.Indexed (Lens.indexed f . onIndex)

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
