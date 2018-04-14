-- | Name clash logic
module Lamdu.Sugar.Names.Clash
    ( Info, infoOf, isClash
    ) where

import           Control.Monad (foldM)
import qualified Data.Map as Map
import           Data.Map.Utils (unionWithM)
import qualified Data.Set as Set
import           Data.UUID.Types (UUID)
import           Lamdu.Sugar.Internal (InternalName(..))
import qualified Lamdu.Sugar.Names.Annotated as Annotated
import           Lamdu.Sugar.Names.Walk (Disambiguator)
import qualified Lamdu.Sugar.Names.Walk as Walk

import           Lamdu.Prelude

type CollisionGroup = Set Walk.NameType

collisionGroups :: [CollisionGroup]
collisionGroups =
    [ [ Walk.GlobalDef, Walk.TaggedVar ]
    , [ Walk.TaggedNominal ]
    ] <&> Set.fromList

data Info = Clash | NoClash NameContext

data GroupNameContext = Ambiguous UUID | Disambiguated (Map Disambiguator UUID)

-- A valid (non-clashing) context for a single name where multiple
-- InternalNames may coexist
type NameContext = Map CollisionGroup GroupNameContext

isClash :: Info -> Bool
isClash Clash = True
isClash NoClash {} = False

infoOf :: Annotated.Name -> Info
infoOf = NoClash . nameContextOf

ctxMatch :: UUID -> UUID -> Maybe UUID
ctxMatch x y
    | x == y = Just x
    | otherwise = Nothing

-- Returns (Maybe NameContext) isomorphic to Info because of the
-- useful Applicative instance for Maybe (used in nameContextMatch)
-- i.e: Nothing indicates a clash
--      Just nameContext indicates a disambiguated name context
groupNameContextMatch :: GroupNameContext -> GroupNameContext -> Maybe GroupNameContext
groupNameContextMatch a b =
    case (a, b) of
    (Ambiguous ctx, Disambiguated m) -> matchAD ctx m
    (Disambiguated m, Ambiguous ctx) -> matchAD ctx m
    (Ambiguous x, Ambiguous y) -> ctxMatch x y <&> Ambiguous
    (Disambiguated x, Disambiguated y) ->
        unionWithM ctxMatch x y <&> Disambiguated
    where
        matchAD ctx m =
            foldM ctxMatch ctx m <&> Ambiguous

nameContextMatch :: NameContext -> NameContext -> Info
nameContextMatch x y = unionWithM groupNameContextMatch x y & maybe Clash NoClash

groupNameContextOf :: UUID -> Maybe Disambiguator -> GroupNameContext
groupNameContextOf ctx Nothing = Ambiguous ctx
groupNameContextOf ctx (Just d) = Map.singleton d ctx & Disambiguated

nameContextOf :: Annotated.Name -> NameContext
nameContextOf (Annotated.Name (InternalName (Just nameCtx) _tag) disamb nameType) =
    filter (nameType `elem`) collisionGroups
    <&> ((,) ?? ctx)
    & Map.fromList
    where
        ctx = groupNameContextOf nameCtx disamb
nameContextOf (Annotated.Name (InternalName Nothing _tag) _disamb _nameType) =
    mempty

instance Semigroup Info where
    NoClash x <> NoClash y = nameContextMatch x y
    _ <> _ = Clash

instance Monoid Info where
    mempty = NoClash mempty
    mappend = (<>)
