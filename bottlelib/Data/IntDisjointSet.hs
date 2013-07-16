{-# LANGUAGE BangPatterns #-}

-- The fork at https://github.com/Peaker/disjoint-set was not pulled
-- into mainline hackage, so we store the fork here:
{- |
Module      : Data.IntDisjointSet
Description : Persistent Disjoint-Sets (a.k.a. Union-Find)
Copyright   : (c) 2012 Maxwell Sayles.
License     : BSD3

Maintainer  : maxwellsayles@gmail.com
Stability   : stable
Portability : non-portable (only tested with GHC 6.12.3)

Persistent Disjoint-Sets (a.k.a. Union-Find).  This implements
disjoint-sets according to the description given in
\"/Introduction to Algorithms/\" by Cormen et al
(<http://mitpress.mit.edu/algorithms>).
Most functions incur an additional O(logn) overhead due to the use
of persistent maps.

Disjoint-sets are a set of elements with equivalence relations defined
between elements, i.e. two elements may be members of the same equivalence
set. Each element has a set representative. The implementation works by
maintaining a map from an element to its parent. When an element is its
own parent, it is the set representative. Two elements are part of the
same equivalence set when their set representatives are the same.

In order to find the set representative efficiently, after each traversal
from an element to its representative, we compress the path so that
each element on the path points directly to the set representative.  For
this to be persistent, lookup is stateful and so returns the result
of the lookup and a new disjoint set.

Additionally, to make sure that path lengths grow logarithmically, we
maintain the rank of a set. This is a logarithmic upper bound on the
number of elements in each set. When we compute the union of two sets,
we make the set with the smaller rank a child of the set with the larger
rank. When two sets have equal rank, the first set is a child of the second
and the rank of the second is increased by 1.

Below \alpha(n) refers to the extremely slowly growing inverse Ackermann
function.
-}

module Data.IntDisjointSet (IntDisjointSet,
                            empty,
                            singleton,
                            insert,
                            unsafeMerge,
                            union, unionRep,
                            lookup,
                            elems,
                            toList,
                            fromList,
                            equivalent,
                            disjointSetSize,
                            size,
                            map) where

import Control.Arrow
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Maybe
import qualified Data.IntMap as IntMap
import qualified Data.List as List
import Data.Maybe
import Prelude hiding (lookup, map)

type Rank = Int

data Node
  = NodeRepresentative {-# UNPACK #-}!Rank
  | NodeLink {-# UNPACK #-}!Int -- link to parent Node

{-| Represents a disjoint set of integers. -}
data IntDisjointSet = IntDisjointSet
  { -- numSets is maintained in the data structure so we can implement
    -- O(1) disjointSetSize:
    numSets :: {-# UNPACK #-}!Int
  , nodes :: !(IntMap.IntMap Node)
  }

instance Show IntDisjointSet where
    show = ("fromList " ++) . show . fst . toList

{-| Create a disjoint set with no members. O(1). -}
empty :: IntDisjointSet
empty = IntDisjointSet 0 IntMap.empty

{-| Create a disjoint set with one member. O(1). -}
singleton :: Int -> IntDisjointSet
singleton !x = IntDisjointSet 1 $ IntMap.singleton x $! NodeRepresentative 0

{-|
Insert x into the disjoint set.
If it is already a member, then do nothing,
otherwise x has no equivalence relations.
O(logn).
-}
insert :: Int -> IntDisjointSet -> IntDisjointSet
insert !x set@(IntDisjointSet count p) =
    let v = NodeRepresentative 0
        (l, p') = IntMap.insertLookupWithKey (\_ _ old -> old) x v p
    in  case l of
          Just _  -> set
          Nothing -> IntDisjointSet (count+1) p'

{-|
Given two instances of disjoint sets that share no members in common,
computes a third disjoint set that is the combination of the two.

This method is unsafe in that is does not verify that the two input
sets share no members in common and in the event that a member
overlaps, the resulting set may have incorrect equivalence relations.
-}
unsafeMerge :: IntDisjointSet -> IntDisjointSet -> IntDisjointSet
unsafeMerge (IntDisjointSet c1 p1) (IntDisjointSet c2 p2) =
    IntDisjointSet (c1+c2) (IntMap.union p1 p2)

{-|
Create an equivalence relation between x and y.
Amortized O(logn * \alpha(n)).

This function works by looking up the set representatives
for both x and y.  If they are the same, it does nothing.
Then it looks up the rank for both representatives and
makes the tree of the smaller ranked representative a
child of the tree of the larger ranked representative.
If both representatives have the same rank, x is made a
child of y and the rank of y is increase by 1.

If either x or y is not present in the input set, nothing is done.
-}
union :: Int -> Int -> IntDisjointSet -> IntDisjointSet
union x y set = snd $ unionRep x y set

unionRep :: Int -> Int -> IntDisjointSet -> (Maybe Int, IntDisjointSet)
unionRep !x !y set = flip runState set $ runMaybeT $ do
  (repx, rankx) <- MaybeT $ state $ compressedFind x
  (repy, ranky) <- MaybeT $ state $ compressedFind y
  if repx == repy
    then return repx
    else do
      IntDisjointSet count newSet <- lift get
      let unify low high updateRank = do
            lift $ put $! IntDisjointSet (count-1) $ updateRank $
              IntMap.insert low (NodeLink high) newSet
            return high
      case compare rankx ranky of
        LT -> unify repx repy id
        GT -> unify repy repx id
        EQ -> unify repx repy $ IntMap.insert repy (NodeRepresentative (ranky + 1))

compressedFind :: Int -> IntDisjointSet -> (Maybe (Int, Rank), IntDisjointSet)
compressedFind !x set =
  case find x set of
    Nothing          -> (Nothing, set)
    Just (rep, rank) -> let !compressedSet = compress rep x set
                        in  (Just (rep, rank), compressedSet)

{-|
Find the set representative for this input.
This performs path compression and so is stateful.
Amortized O(logn * \alpha(n)).
-}
lookup :: Int -> IntDisjointSet -> (Maybe Int, IntDisjointSet)
lookup !x set = (fmap fst mPair, newSet)
  where
    (mPair, newSet) = compressedFind x set

{-| Return a list of all the elements. -}
-- This is stateful for consistency and possible future revisions.
elems :: IntDisjointSet -> ([Int], IntDisjointSet)
elems = IntMap.keys . nodes &&& id

{-|
Generate an association list of each element and its representative,
in arbitrary order.
-}
toList :: IntDisjointSet -> ([(Int, Int)], IntDisjointSet)
toList set = flip runState set $ do
               xs <- state elems
               forM xs $ \x -> do
                 Just rep <- state $ lookup x
                 return (x, rep)

{-|
Given an association list representing equivalences between elements,
generate the corresponding disjoint-set.
-}
fromList :: [(Int, Int)] -> IntDisjointSet
fromList = foldr (\(x, y) -> union x y . insert y . insert x) empty

{-| True if both elements belong to the same set. -}
equivalent :: Int -> Int -> IntDisjointSet -> (Bool, IntDisjointSet)
equivalent !x !y set = first (fromMaybe False) $
                       flip runState set $
                       runMaybeT $ do
                         repx <- MaybeT $ state $ lookup x
                         repy <- MaybeT $ state $ lookup y
                         return $! repx == repy

{-| Return the number of disjoint sets. O(N). -}
disjointSetSize :: IntDisjointSet -> Int
disjointSetSize = numSets

{-| Return the number of elements in all disjoint sets. O(1). -}
size :: IntDisjointSet -> Int
size = IntMap.size . nodes

{-|
Map each member to another Int.
The map function must be a bijection, i.e. 1-to-1 mapping.
-}
map :: (Int -> Int) -> IntDisjointSet -> IntDisjointSet
map f (IntDisjointSet count p) =
  let mapNode old@(NodeRepresentative _) = old
      mapNode (NodeLink parent) = NodeLink (f parent)
      p' = IntMap.fromList $ List.map (f *** mapNode) $ IntMap.toList p
  in  IntDisjointSet count p'

-- Find the set representative.
-- This traverses parents until the parent of y == y and returns y.
find :: Int -> IntDisjointSet -> Maybe (Int, Rank)
find !x (IntDisjointSet _ set) =
  do node <- IntMap.lookup x set
     Just $ case node of
       NodeRepresentative rank -> (x, rank)
       NodeLink x' -> find' x'
  where find' y = case set IntMap.! y of
                    NodeRepresentative rank -> (y, rank)
                    NodeLink y' -> find' y'

-- Given a start node and its representative, compress
-- the path to the root.
compress :: Int -> Int -> IntDisjointSet -> IntDisjointSet
compress !rep !leaf (IntDisjointSet count orig) = IntDisjointSet count $ helper leaf orig
    where helper !x p
              | x == rep  = p
              | otherwise = helper x' p'
              where NodeLink x' = p IntMap.! x
                    p' = IntMap.insert x (NodeLink rep) p
