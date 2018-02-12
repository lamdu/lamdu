-- | Name clash logic
{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}
module Lamdu.Sugar.Names.Clash
    ( AnnotatedName(..), niUUID, niDisambiguator, niNameType
    , NameContext
    , check
    , IsClash(..), isClash, isClashOf
    ) where

import qualified Control.Lens as Lens
import qualified Data.Map as Map
import           Data.Map.Utils (unionWithM)
import           Data.UUID.Types (UUID)
import           Lamdu.Sugar.Names.Walk (Disambiguator)
import qualified Lamdu.Sugar.Names.Walk as Walk

import           Lamdu.Prelude

type CollisionGroup = [Walk.NameType]

collisionGroups :: [CollisionGroup]
collisionGroups =
    [ [ Walk.DefName, Walk.ParamName, Walk.FieldParamName ]
    , [ Walk.TagName, Walk.FieldParamName ]
    , [ Walk.NominalName ]
    ]

-- | Info about a single instance of use of a name:
data AnnotatedName = AnnotatedName
    { _niUUID :: !UUID
    , -- | Is the name used in a function application context? We consider
      -- the application as a disambiguator
      _niDisambiguator :: !(Maybe Disambiguator)
    , _niNameType :: !Walk.NameType
    } deriving (Eq, Ord, Show)
Lens.makeLenses ''AnnotatedName

data IsClash = Clash | NoClash NameContext

data GroupNameContext = Ambiguous UUID | Disambiguated (Map Disambiguator UUID)

-- A valid (non-clashing) context for a single name where multiple
-- UUIDs may coexist
type NameContext = Map CollisionGroup GroupNameContext

isClash :: IsClash -> Bool
isClash Clash = True
isClash NoClash {} = False

isClashOf :: AnnotatedName -> IsClash
isClashOf = NoClash . nameContextOf

-- Returns (Maybe NameContext) isomorphic to IsClash because of the
-- useful Applicative instance for Maybe (used in nameContextCombine)
-- i.e: Nothing indicates a clash
--      Just nameContext indicates a disambiguated name context
groupNameContextCombine :: GroupNameContext -> GroupNameContext -> Maybe GroupNameContext
groupNameContextCombine a b =
    case (a, b) of
    (Ambiguous uuid, Disambiguated m) -> combineAD uuid m
    (Disambiguated m, Ambiguous uuid) -> combineAD uuid m
    (Ambiguous x, Ambiguous y)
        | x == y -> Just (Ambiguous x)
        | otherwise -> Nothing
    (Disambiguated x, Disambiguated y)
        | Map.intersectionWith (/=) x y & or -> Nothing
        | otherwise -> x <> y & Disambiguated & Just
    where
        combineAD uuid m
            | m ^.. Lens.folded & filter (/= uuid) & null = Just (Ambiguous uuid)
            | otherwise = Nothing

nameContextCombine :: NameContext -> NameContext -> IsClash
nameContextCombine x y = unionWithM groupNameContextCombine x y & maybe Clash NoClash

groupNameContextOf :: AnnotatedName -> GroupNameContext
groupNameContextOf (AnnotatedName uuid Nothing _) = Ambiguous uuid
groupNameContextOf (AnnotatedName uuid (Just d) _) = Map.singleton d uuid & Disambiguated

nameContextOf :: AnnotatedName -> NameContext
nameContextOf inst =
    filter (inst ^. niNameType `elem`) collisionGroups
    <&> ((,) ?? ctx)
    & Map.fromList
    where
        ctx = groupNameContextOf inst

instance Semigroup IsClash where
    NoClash x <> NoClash y = nameContextCombine x y
    _ <> _ = Clash

instance Monoid IsClash where
    mempty = NoClash mempty
    mappend = (<>)

check :: [AnnotatedName] -> IsClash
check ns = ns <&> isClashOf & mconcat
