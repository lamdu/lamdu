-- | Name clash logic
module Lamdu.Sugar.Names.Clash
    ( Info, infoOf, isClash, collide
    , Collider(..), uncolliders, colliders
    ) where

import           Control.Lens.Extended ((~~>))
import           Control.Monad (foldM)
import           Data.Coerce (coerce)
import           Data.MMap (MMap)
import qualified Data.MMap as MMap
import qualified Data.Set as Set
import           Data.UUID.Types (UUID)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Sugar.Internal (InternalName(..))
import qualified Lamdu.Sugar.Names.Annotated as Annotated
import           Lamdu.Sugar.Names.Walk (Disambiguator)
import qualified Lamdu.Sugar.Names.Walk as Walk

import           Lamdu.Prelude

data Info = Clash | NoClash NameContext
    deriving Show

-- A valid (non-clashing) context for a single name where multiple
-- InternalNames may coexist
type NameContext = MMap CollisionGroup GroupNameContext

type CollisionGroup = Set Walk.NameType

data GroupNameContext = Ambiguous UUIDInfo | Disambiguated (MMap Disambiguator UUIDInfo)
    deriving Show

data UUIDInfo = Single UUID | Multiple -- no need to store the UUIDs, they clash with any UUID
    deriving Show

instance Semigroup UUIDInfo where
    Single x <> Single y | x == y = Single x
    _ <> _ = Multiple

instance Semigroup GroupNameContext where
    Ambiguous x <> Ambiguous y = Ambiguous (x <> y)
    Disambiguated x <> Disambiguated y = Disambiguated (x <> y)
    Ambiguous x <> Disambiguated y = Ambiguous (foldr (<>) x y)
    Disambiguated x <> Ambiguous y = Ambiguous (foldr (<>) y x)

instance Monoid GroupNameContext where
    mempty = Disambiguated mempty

collisionGroups :: [CollisionGroup]
collisionGroups =
    [ [ Walk.GlobalDef, Walk.TaggedVar ]
    , [ Walk.TaggedNominal ]
    ] <&> Set.fromList

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

-- | Newtype for a Semigroup that collides two Clash Infos with one
-- another. The Infos come from scopes that are above/below one
-- another, and so directly collide, rather than from sibling scopes
-- as in the Semigroup instance
newtype Collider = Collider Info deriving stock Show
instance Semigroup Collider where
    Collider x <> Collider y = Collider (x `collide` y)

-- 2 wrappers for coerce for readability/safety
uncolliders :: MMap T.Tag Collider -> MMap T.Tag Info
uncolliders = coerce

colliders :: MMap T.Tag Info -> MMap T.Tag Collider
colliders = coerce

collide :: Info -> Info -> Info
collide (NoClash x) (NoClash y) = nameContextMatch x y
collide _ _ = Clash
