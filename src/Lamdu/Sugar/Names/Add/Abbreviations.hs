module Lamdu.Sugar.Names.Add.Abbreviations (abbreviations) where

import qualified Control.Lens as Lens
import qualified Data.MMap as MMap
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Tuple as Tuple
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Tag as Tag

import           Lamdu.Prelude

abbreviations :: Map T.Tag Tag.TextsInLang -> Map T.Tag Text
abbreviations p1texts =
    abbreviationTags
    ^@.. Lens.itraversed <. Lens.filtered ((== 1) . Set.size) . Lens.folded
    <&> Tuple.swap
    & Map.fromList
    where
        fullTexts = p1texts ^.. traverse . Tag.name & Set.fromList
        abbreviationTags =
            p1texts
            ^@.. Lens.itraversed <. Tag.abbreviation . Lens._Just
            <&> (\(tag, abr) -> (abr, Set.singleton tag))
            & filter (not . (`Set.member` fullTexts) . fst)
            & MMap.fromList

