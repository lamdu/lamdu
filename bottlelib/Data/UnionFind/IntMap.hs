{- Copied from the union-find package, until the new github version is
 - uploaded.
 -}

{- union-find LICENSE file:

Copyright 2009, Thomas Schilling
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice,
this list of conditions and the following disclaimer.
 
- Redistributions in binary form must reproduce the above copyright notice,
this list of conditions and the following disclaimer in the documentation
and/or other materials provided with the distribution.
 
- Neither name of the author nor the names of its contributors may be
used to endorse or promote products derived from this software without
specific prior written permission. 

THIS SOFTWARE IS PROVIDED BY THE AUTHOR(S) AND THE CONTRIBUTORS "AS
IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR THE
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-}

{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Data.UnionFind.IntMap 
    ( newPointSupply, fresh, repr, descr, setDescr, union, equivalent,
      PointSupply, Point ) where

import qualified Data.IntMap as IM

data PointSupply a = PointSupply !Int (IM.IntMap (Link a))
  deriving Show

data Link a 
    = Info {-# UNPACK #-} !Int a
      -- ^ This is the descriptive element of the equivalence class
      -- and its rank.
    | Link {-# UNPACK #-} !Int
      -- ^ Pointer to some other element of the equivalence class.
     deriving Show

newtype Point a = Point Int
  deriving (Show)

newPointSupply :: PointSupply a
newPointSupply = PointSupply 0 IM.empty

fresh :: PointSupply a -> a -> (PointSupply a, Point a)
fresh (PointSupply next eqs) a =
  (PointSupply (next + 1) (IM.insert next (Info 0 a) eqs), Point next)

-- freshList :: PointSupply a -> [a] -> (PointSupply a, [Point a])
-- freshList 

repr :: PointSupply a -> Point a -> Point a
repr ps p = reprInfo ps p (\n _rank _a -> Point n)

reprInfo :: PointSupply a -> Point a -> (Int -> Int -> a -> r) -> r
reprInfo (PointSupply _next eqs) (Point n) k = go n
  where
    go !i =
      case eqs IM.! i of
        Link i' -> go i'
        Info r a -> k i r a
  
union :: PointSupply a -> Point a -> Point a -> PointSupply a
union ps@(PointSupply next eqs) p1 p2 =
  reprInfo ps p1 $ \i1 r1 _a1 -> 
  reprInfo ps p2 $ \i2 r2 a2 ->
  if i1 == i2 then ps else
    case r1 `compare` r2 of
      LT ->
        -- No rank or descriptor update necessary
        let !eqs1 = IM.insert i1 (Link i2) eqs in
        PointSupply next eqs1
      EQ ->
        let !eqs1 = IM.insert i1 (Link i2) eqs
            !eqs2 = IM.insert i2 (Info (r2 + 1) a2) eqs1 in
        PointSupply next eqs2
      GT ->
        let !eqs1 = IM.insert i1 (Info r2 a2) eqs
            !eqs2 = IM.insert i2 (Link i1) eqs1 in
        PointSupply next eqs2

descr :: PointSupply a -> Point a -> a
descr ps p = reprInfo ps p (\_ _ a -> a)

setDescr :: PointSupply a -> Point a -> a -> PointSupply a
setDescr ps@(PointSupply next eqs) p val =
  reprInfo ps p $ \i r _oldVal ->
  PointSupply next $ IM.insert i (Info r val) eqs

equivalent :: PointSupply a -> Point a -> Point a -> Bool
equivalent ps p1 p2 =
  reprInfo ps p1 $ \i1 _ _ ->
  reprInfo ps p2 $ \i2 _ _ ->
  i1 == i2

{-
tst1 :: IO ()
tst1 = do
  let ps0 = newPointSupply
      (ps1, p1) = fresh ps0 "hello"
      (ps2, p2) = fresh ps1 "world"
      (ps3, p3) = fresh ps2 "you"
      (ps, p4) = fresh ps3 "there"
  let ps' = union ps p1 p2
  print (descr ps p1, descr ps p2, equivalent ps p1 p2)
  print (descr ps' p1, descr ps' p2, equivalent ps' p1 p2)
  let ps'' = union ps' p3 p1
  print (descr ps'' p1, descr ps'' p3, equivalent ps'' p1 p3)
  print ps''
-}

-- TODO: should fail
{-
tst2 :: IO ()
tst2 = do
  let as0 = newPointSupply
      (as, a1) = fresh as0 "foo"
      bs0 = newPointSupply
      (bs, b1) = fresh bs0 "bar"
  print $ union as a1 b1
  -}    
