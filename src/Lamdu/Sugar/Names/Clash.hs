-- | Name clash logic
{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Sugar.Names.Clash
    ( AnnotatedName(..), anInternal, anDisambiguator, anNameType
      , anTag
    , NameContext
    , IsClash(..), isClash, isClashOf
    ) where

import qualified Control.Lens as Lens
import           Control.Monad (foldM)
import qualified Data.Map as Map
import           Data.Map.Utils (unionWithM)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Sugar.Internal (InternalName(..), internalNameMatch, inTag)
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

-- | Info about a single instance of use of a name:
data AnnotatedName = AnnotatedName
    { _anInternal :: !InternalName
    , -- | Is the name used in a function application context? We consider
      -- the application as a disambiguator
      _anDisambiguator :: !(Maybe Disambiguator)
    , _anNameType :: !Walk.NameType
    } deriving (Eq, Ord, Show)
Lens.makeLenses ''AnnotatedName

anTag :: Lens' AnnotatedName T.Tag
anTag = anInternal . inTag

data IsClash = Clash | NoClash NameContext

data GroupNameContext = Ambiguous InternalName | Disambiguated (Map Disambiguator InternalName)

-- A valid (non-clashing) context for a single name where multiple
-- InternalNames may coexist
type NameContext = Map CollisionGroup GroupNameContext

isClash :: IsClash -> Bool
isClash Clash = True
isClash NoClash {} = False

isClashOf :: AnnotatedName -> IsClash
isClashOf = NoClash . nameContextOf

-- Returns (Maybe NameContext) isomorphic to IsClash because of the
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

nameContextMatch :: NameContext -> NameContext -> IsClash
nameContextMatch x y = unionWithM groupNameContextMatch x y & maybe Clash NoClash

groupNameContextOf :: AnnotatedName -> GroupNameContext
groupNameContextOf (AnnotatedName internalName Nothing _) = Ambiguous internalName
groupNameContextOf (AnnotatedName internalName (Just d) _) = Map.singleton d internalName & Disambiguated

nameContextOf :: AnnotatedName -> NameContext
nameContextOf inst =
    filter (inst ^. anNameType `elem`) collisionGroups
    <&> ((,) ?? ctx)
    & Map.fromList
    where
        ctx = groupNameContextOf inst

instance Semigroup IsClash where
    NoClash x <> NoClash y = nameContextMatch x y
    _ <> _ = Clash

instance Monoid IsClash where
    mempty = NoClash mempty
    mappend = (<>)
