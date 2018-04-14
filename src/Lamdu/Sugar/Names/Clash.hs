-- | Name clash logic
module Lamdu.Sugar.Names.Clash
    ( Info, infoOf, isClash
    ) where

import           Control.Monad (foldM)
import qualified Data.Map as Map
import           Data.Map.Utils (unionWithM)
import           Lamdu.Sugar.Internal (InternalName(..), internalNameMatch)
import qualified Lamdu.Sugar.Names.Annotated as Annotated
import           Lamdu.Sugar.Names.Walk (Disambiguator)
import qualified Lamdu.Sugar.Names.Walk as Walk

import           Lamdu.Prelude

-- TODO: Simplify CollisionGroup
type CollisionGroup = [Walk.NameType]

collisionGroups :: [CollisionGroup]
collisionGroups =
    [ [ Walk.GlobalDef, Walk.TaggedVar ]
    , [ Walk.TaggedNominal ]
    ]

data Info = Clash | NoClash NameContext

data GroupNameContext = Ambiguous InternalName | Disambiguated (Map Disambiguator InternalName)

-- A valid (non-clashing) context for a single name where multiple
-- InternalNames may coexist
type NameContext = Map CollisionGroup GroupNameContext

isClash :: Info -> Bool
isClash Clash = True
isClash NoClash {} = False

infoOf :: Annotated.Name -> Info
infoOf = NoClash . nameContextOf

-- Returns (Maybe NameContext) isomorphic to Info because of the
-- useful Applicative instance for Maybe (used in nameContextMatch)
-- i.e: Nothing indicates a clash
--      Just nameContext indicates a disambiguated name context
groupNameContextMatch :: GroupNameContext -> GroupNameContext -> Maybe GroupNameContext
groupNameContextMatch a b =
    case (a, b) of
    (Ambiguous internalName, Disambiguated m) -> matchAD internalName m
    (Disambiguated m, Ambiguous internalName) -> matchAD internalName m
    (Ambiguous x, Ambiguous y) -> internalNameMatch x y <&> Ambiguous
    (Disambiguated x, Disambiguated y) ->
        unionWithM internalNameMatch x y <&> Disambiguated
    where
        matchAD internalName m =
            foldM internalNameMatch internalName m <&> Ambiguous

nameContextMatch :: NameContext -> NameContext -> Info
nameContextMatch x y = unionWithM groupNameContextMatch x y & maybe Clash NoClash

groupNameContextOf :: Annotated.Name -> GroupNameContext
groupNameContextOf (Annotated.Name internalName Nothing _) = Ambiguous internalName
groupNameContextOf (Annotated.Name internalName (Just d) _) = Map.singleton d internalName & Disambiguated

nameContextOf :: Annotated.Name -> NameContext
nameContextOf inst =
    filter (inst ^. Annotated.nameType `elem`) collisionGroups
    <&> ((,) ?? ctx)
    & Map.fromList
    where
        ctx = groupNameContextOf inst

instance Semigroup Info where
    NoClash x <> NoClash y = nameContextMatch x y
    _ <> _ = Clash

instance Monoid Info where
    mempty = NoClash mempty
    mappend = (<>)
