module Lamdu.Sugar.Names.Add.Abbreviations (abbreviations) where

import qualified Control.Lens as Lens
import           Data.Foldable (fold)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Trie.Text (Trie)
import qualified Data.Trie.Text as Trie
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Tag as Tag

import           Lamdu.Prelude

abbreviations :: Map T.Tag Tag.TextsInLang -> Map T.Tag Text
abbreviations p1texts =
    filter (useAbbrev trie . (^. _2)) (p1texts ^@.. Lens.itraversed)
    ^@.. Lens.traverse . Lens.filteredBy _1 <. _2 . Tag.abbreviation . Lens._Just
    & Map.fromList
    where
        trie = toTrie p1texts

useAbbrev :: Trie (Set T.Tag) -> Tag.TextsInLang -> Bool
useAbbrev textTags tag =
    case tag ^. Tag.abbreviation of
    Nothing -> False
    Just abbrev -> Set.size (fold (Trie.submap abbrev textTags)) == 1

toTrie :: Map T.Tag Tag.TextsInLang -> Trie (Set T.Tag)
toTrie p1Texts =
    p1Texts ^@.. Lens.itraversed <. (Tag.name <> Tag.abbreviation . Lens._Just)
    <&> (\(tag, text) -> Trie.singleton text (Set.singleton tag))
    & mconcat
