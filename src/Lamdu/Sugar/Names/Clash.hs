-- | Name clash logic
module Lamdu.Sugar.Names.Clash
    ( Info, infoOf, isClash, collide
    ) where

import           Control.Lens.Extended ((~~>))
import           Control.Monad (foldM)
import           Data.MMap (MMap)
import qualified Data.MMap as MMap
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
    deriving Show

data UUIDInfo = Single UUID | Multiple -- no need to store the UUIDs, they clash with any UUID
    deriving Show

instance Semigroup UUIDInfo where
    Single x <> Single y | x == y = Single x
    _ <> _ = Multiple

data GroupNameContext = Ambiguous UUIDInfo | Disambiguated (MMap Disambiguator UUIDInfo)
    deriving Show

instance Semigroup GroupNameContext where
    Ambiguous x <> Ambiguous y = Ambiguous (x <> y)
    Disambiguated x <> Disambiguated y = Disambiguated (x <> y)
    Ambiguous x <> Disambiguated y = Ambiguous (foldr (<>) x y)
    Disambiguated x <> Ambiguous y = Ambiguous (foldr (<>) y x)

-- A valid (non-clashing) context for a single name where multiple
-- InternalNames may coexist
type NameContext = MMap CollisionGroup GroupNameContext

isClash :: Info -> Bool
isClash Clash = True
isClash NoClash {} = False

infoOf :: Annotated.Name -> Info
infoOf = NoClash . nameContextOf

ctxMatch :: UUIDInfo -> UUIDInfo -> Maybe UUIDInfo
ctxMatch x y =
    case x <> y of
    Single z -> Just (Single z)
    Multiple -> Nothing

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
        MMap.unionWithM ctxMatch x y <&> Disambiguated
    where
        matchAD ctx m =
            foldM ctxMatch ctx m <&> Ambiguous

nameContextMatch :: NameContext -> NameContext -> Info
nameContextMatch x y = MMap.unionWithM groupNameContextMatch x y & maybe Clash NoClash

groupNameContextOf :: UUID -> Maybe Disambiguator -> GroupNameContext
groupNameContextOf uuid Nothing = Ambiguous (Single uuid)
groupNameContextOf uuid (Just d) = d ~~> Single uuid & Disambiguated

nameContextOf :: Annotated.Name -> NameContext
nameContextOf (Annotated.Name (InternalName (Just nameCtx) _tag _) disamb nameType) =
    filter (nameType `elem`) collisionGroups
    <&> ((,) ?? ctx)
    & MMap.fromList
    where
        ctx = groupNameContextOf nameCtx disamb
nameContextOf (Annotated.Name (InternalName Nothing _tag _) _disamb _nameType) =
    mempty

instance Semigroup Info where
    NoClash x <> NoClash y = NoClash (x <> y)
    _ <> _ = Clash

instance Monoid Info where
    mempty = NoClash mempty

-- | Collide two Clash Infos with one another. The Infos come from
-- scopes that are above/below one another, and so directly collide,
-- rather than from sibling scopes as in the Semigroup instance
collide :: Info -> Info -> Info
collide (NoClash x) (NoClash y) = nameContextMatch x y
collide _ _ = Clash
