-- | A wrapper for the fuzzyset library that makes it a Fuzzy map
{-# LANGUAGE TemplateHaskell, DerivingVia #-}
module Lamdu.Fuzzy
    ( Fuzzy, make, matches
    , Distance(..), isFuzzy, distanceInts
    , memoableMake
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Extended ((==>))
import           Data.Char (toLower)
import           Data.List (sortOn)
import           Data.MMap (MMap)
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Data.Vector ((!))
import qualified Data.Vector as Vector
import qualified Text.EditDistance as EditDistance
import           Text.PrettyPrint (($+$), (<+>))
import qualified Text.PrettyPrint as Pretty
import           Text.PrettyPrint.HughesPJClass (Pretty(..))

import           Lamdu.Prelude

data Fuzzy a =
    Fuzzy (MMap Text a) (MMap Char (Fuzzy a))
    deriving stock Generic
    deriving (Semigroup, Monoid) via Generically (Fuzzy a)

pPrintMMap :: (k -> Pretty.Doc) -> (v -> Pretty.Doc) -> MMap k v -> Pretty.Doc
pPrintMMap pk pv m =
    m ^@.. Lens.ifolded
    <&> (\(k, v) -> pk k <+> " => " <+> pv v)
    & Pretty.vcat

instance Pretty a => Pretty (Fuzzy a) where
    pPrint (Fuzzy xs m) =
        payload $+$ Pretty.nest 4 (pPrintMMap pPrint pPrint m)
        where
            payload = pPrintMMap (Pretty.text . Text.unpack) pPrint xs

trieOf :: Semigroup a => Text -> String -> a -> Fuzzy a
trieOf s [] x = Fuzzy (s ==> x) mempty
trieOf s (c:cs) x =
    Fuzzy mempty (toLower c ==> cont) <> cont
    where
        cont = trieOf s cs x

make :: Ord a => [(Text, a)] -> Fuzzy (Set a)
make items =
    items
    <&> (\(text, x) -> trieOf text (Text.unpack text) (Set.singleton x))
    & mconcat

-- | Takes the number of skips in given string that are allowed
trieMatch :: Semigroup a => Int -> String -> Fuzzy a -> MMap Text a
trieMatch _ [] (Fuzzy xs _) = xs
trieMatch n (c:cs) t@(Fuzzy _ m) =
    (m ^.. Lens.ix (toLower c) <&> trieMatch n cs)
    ++ [trieMatch (n-1) cs t | n > 0] -- <-- try skipping a char
    & mconcat

data Distance = Distance
    { _isFuzzy :: Bool
    , _distanceInts :: [Int]
    } deriving (Eq, Ord)

Lens.makeLenses ''Distance

distance :: Text -> Text -> Distance
distance rawX rawY =
    Distance
    { _isFuzzy = all (== 1) nonFuzzyScores
    , _distanceInts =
        nonFuzzyScores ++
        [ EditDistance.restrictedDamerauLevenshteinDistance
          EditDistance.defaultEditCosts (Text.unpack x) (Text.unpack y)
        ]
    }
    where
        nonFuzzyScores =
            concat
            [ check (==)
            , check Text.isPrefixOf
            , check Text.isInfixOf
            ]
        score True = 0
        score False = 1
        check f = map score [f rawX rawY, f x y]
        x = Text.toLower rawX
        y = Text.toLower rawY

allowedSkips :: Int
allowedSkips = 0

matches :: Semigroup a => Text -> Fuzzy a -> MMap Text a
matches = trieMatch allowedSkips . Text.unpack

memoableMake ::
    ([(Text, Int)] -> Fuzzy (Set Int)) -> [(Text, a)] -> Text -> [(Distance, a)]
memoableMake memoMake pairs =
    -- Keep as explicit lambda syntax (not in LHS) for
    -- performance/sharing reasons (where clauses do not depend on
    -- last Text param)
    \text ->
    matches text f ^@.. Lens.ifolded
    <&> _1 %~ distance text
    & sortOn fst
    >>= flatten
    <&> _2 %~ (v !)
    where
        -- Use const cause lower scores are better, use those for dups
        flatten (dist, indices) = Set.toList indices <&> (,) dist
        f =
            pairs ^@.. Lens.ifolded
            <&> (\(idx, (text, _)) -> (text, idx))
            & memoMake
        v = pairs <&> snd & Vector.fromList
