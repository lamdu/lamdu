-- | A wrapper for the fuzzyset library that makes it a Fuzzy map
{-# LANGUAGE OverloadedStrings #-}
module Lamdu.Fuzzy
    ( Fuzzy, make, matches
    , memoableMake
    ) where

import qualified Control.Lens as Lens
import           Data.Char (toLower)
import           Data.List (sortOn)
import           Data.MMap (MMap)
import           Data.Map.Utils (singleton)
import           Data.Semigroup (Semigroup)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Vector ((!))
import qualified Data.Vector as Vector
import qualified Text.EditDistance as EditDistance
import           Text.PrettyPrint (($+$), (<+>))
import qualified Text.PrettyPrint as Pretty
import           Text.PrettyPrint.HughesPJClass (Pretty(..))

import           Lamdu.Prelude

data Fuzzy a = Fuzzy (MMap Text a) (MMap Char (Fuzzy a))

pPrintMMap :: (k -> Pretty.Doc) -> (v -> Pretty.Doc) -> MMap k v -> Pretty.Doc
pPrintMMap pk pv m =
    m ^@.. Lens.ifolded
    <&> (\(k, v) -> pk k <+> " => " <+> pv v)
    & Pretty.vcat

instance Semigroup a => Semigroup (Fuzzy a) where
    Fuzzy xs xmore <> Fuzzy ys ymore =
        Fuzzy (xs <> ys) (xmore <> ymore)

instance Monoid a => Monoid (Fuzzy a) where
    mempty = Fuzzy mempty mempty
    Fuzzy xs xmore `mappend` Fuzzy ys ymore =
        Fuzzy (xs `mappend` ys) (xmore `mappend` ymore)

instance Pretty a => Pretty (Fuzzy a) where
    pPrint (Fuzzy xs m) =
        payload $+$ Pretty.nest 4 (pPrintMMap pPrint pPrint m)
        where
            payload = pPrintMMap (Pretty.text . Text.unpack) pPrint xs

trieOf :: Monoid a => Text -> String -> a -> Fuzzy a
trieOf s [] x = Fuzzy (singleton s x) mempty
trieOf s (c:cs) x =
    Fuzzy mempty (singleton (toLower c) cont) `mappend` cont
    where
        cont = trieOf s cs x

make :: Ord a => [(Text, a)] -> Fuzzy (Set a)
make items =
    items
    <&> (\(text, x) -> trieOf text (Text.unpack text) (Set.singleton x))
    & mconcat

-- | Takes the number of skips in given string that are allowed
trieMatch :: Monoid a => Int -> String -> Fuzzy a -> MMap Text a
trieMatch _ [] (Fuzzy xs _) = xs
trieMatch n (c:cs) t@(Fuzzy _ m) =
    (m ^.. Lens.ix (toLower c) <&> trieMatch n cs)
    ++ [trieMatch (n-1) cs t | n > 0] -- <-- try skipping a char
    & mconcat

distance :: Text -> Text -> [Int]
distance rawX rawY =
    concat
    [ check (==)
    , check Text.isPrefixOf
    , check Text.isInfixOf
    ] ++
    [ EditDistance.restrictedDamerauLevenshteinDistance
      EditDistance.defaultEditCosts (Text.unpack x) (Text.unpack y)
    ]
    where
        score True = 0
        score False = 1
        check f = map score [f rawX rawY, f x y]
        x = Text.toLower rawX
        y = Text.toLower rawY

allowedSkips :: Int
allowedSkips = 0

matches :: Monoid a => Text -> Fuzzy a -> MMap Text a
matches = trieMatch allowedSkips . Text.unpack

memoableMake ::
    ([(Text, Int)] -> Fuzzy (Set Int)) -> [(Text, a)] -> Text -> [a]
memoableMake memoMake pairs =
    -- Keep as explicit lambda syntax (not in LHS) for
    -- performance/sharing reasons (where clauses do not depend on
    -- last Text param)
    \text ->
    matches text f ^@.. Lens.ifolded
    & sortOn (distance text . fst)
    <&> snd
    <&> Set.toList
    <&> map (v !)
    & concat
    where
        f =
            pairs ^@.. Lens.ifolded
            <&> (\(idx, (text, _)) -> (text, idx))
            & memoMake
        v = pairs <&> snd & Vector.fromList
