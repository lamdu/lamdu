{-# LANGUAGE TemplateHaskell, DerivingVia #-}
-- | Name clash logic
module Lamdu.Sugar.Names.Clash
    ( Info, _Clash, _NoClash
    , infoOf
    , Collider(..), _Collider, colliders
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Extended ((~~>))
import           Control.Monad (foldM)
import           Data.MMap (MMap)
import qualified Data.MMap as MMap
import           Data.UUID.Types (UUID)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Sugar.Internal (inContext)
import qualified Lamdu.Sugar.Names.Annotated as Annotated
import           Lamdu.Sugar.Names.Walk (Disambiguator)
import qualified Lamdu.Sugar.Names.Walk as Walk

import           Lamdu.Prelude

data Info = Clash | NoClash NameContext
    deriving Show

-- A valid (non-clashing) context for a single name where multiple
-- InternalNames may coexist
type NameContext = NameSpaces GroupNameContext

data NameSpaces a = NameSpaces
    { _nsLower :: a
    , _nsUpper :: a
    } deriving stock (Functor, Foldable, Traversable, Show, Generic, Generic1)
    deriving Applicative via Generically1 NameSpaces
    deriving (Monoid, Semigroup) via Generically (NameSpaces a)

data GroupNameContext = Ambiguous UUIDInfo | Disambiguated (MMap Disambiguator UUIDInfo)
    deriving Show

data UUIDInfo = Single UUID | Multiple -- no need to store the UUIDs, they clash with any UUID
    deriving Show

Lens.makeLenses ''NameSpaces
Lens.makePrisms ''Info

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
nameContextMatch x y = groupNameContextMatch <$> x <*> y & sequenceA & maybe Clash NoClash

groupNameContextOf :: UUID -> Maybe Disambiguator -> GroupNameContext
groupNameContextOf uuid Nothing = Ambiguous (Single uuid)
groupNameContextOf uuid (Just d) = d ~~> Single uuid & Disambiguated

nameTypeSpace :: Applicative f => Walk.NameType -> Lens.LensLike' f (NameSpaces a) a
nameTypeSpace Walk.TaggedNominal = nsUpper
nameTypeSpace Walk.GlobalDef = nsLower
nameTypeSpace Walk.TaggedVar = nsLower
nameTypeSpace _ = const pure -- Empty traversal

nameContextOf :: Annotated.Name -> NameContext
nameContextOf (Annotated.Name name disamb nameType) =
    foldMap
    (\c -> mempty & nameTypeSpace nameType .~ groupNameContextOf c disamb)
    (name ^. inContext)

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
    Collider (NoClash x) <> Collider (NoClash y) = Collider (nameContextMatch x y)
    _ <> _ = Collider Clash
instance Monoid Collider where mempty = Collider mempty

Lens.makePrisms ''Collider

colliders :: Lens.Iso' (MMap T.Tag Info) (MMap T.Tag Collider)
colliders = Lens.coerced
