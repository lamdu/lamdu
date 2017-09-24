-- | Name clash logic
{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}
module Lamdu.Sugar.Names.Clash
    ( NameInstance(..), niUUID, niDisambiguator, niNameType
    , NameContext
    , Disambiguator
    , check
    , IsClash(..), isClash, isClashOf
    ) where

import qualified Control.Lens as Lens
import qualified Data.Map as Map
import           Data.Map.Utils (unionWithM)
import           Data.UUID.Types (UUID)
import qualified Lamdu.Sugar.Names.Walk as Walk

import           Lamdu.Prelude

type CollisionGroup = [Walk.NameType]

collisionGroups :: [CollisionGroup]
collisionGroups =
    [ [ Walk.DefName, Walk.ParamName, Walk.FieldParamName ]
    , [ Walk.TagName, Walk.FieldParamName ]
    , [ Walk.NominalName ]
    ]

type Disambiguator = Walk.FunctionSignature

-- | Info about a single instance of use of a name:
data NameInstance = NameInstance
    { _niUUID :: !UUID
    , -- | Is the name used in a function application context? We consider
      -- the application as a disambiguator
      _niDisambiguator :: !(Maybe Disambiguator)
    , _niNameType :: !Walk.NameType
    } deriving (Eq, Ord, Show)
Lens.makeLenses ''NameInstance

data IsClash = Clash | NoClash NameContext
isClash :: IsClash -> Bool
isClash Clash = True
isClash NoClash {} = False

isClashOf :: NameInstance -> IsClash
isClashOf = NoClash . nameContextOf

data GroupNameContext = Ambiguous UUID | Disambiguated (Map Disambiguator UUID)

-- A valid (non-clashing) context for a single name where multiple
-- UUIDs may coexist
type NameContext = Map CollisionGroup GroupNameContext

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

nameContextCombine :: NameContext -> NameContext -> Maybe NameContext
nameContextCombine = unionWithM groupNameContextCombine

groupNameContextOf :: NameInstance -> GroupNameContext
groupNameContextOf (NameInstance uuid Nothing _) = Ambiguous uuid
groupNameContextOf (NameInstance uuid (Just d) _) = Map.singleton d uuid & Disambiguated

nameContextOf :: NameInstance -> NameContext
nameContextOf inst =
    filter (inst ^. niNameType `elem`) collisionGroups
    <&> ((,) ?? ctx)
    & Map.fromList
    where
        ctx = groupNameContextOf inst

instance Monoid IsClash where
    mempty = NoClash mempty
    mappend (NoClash x) (NoClash y) =
        case nameContextCombine x y of
        Nothing -> Clash
        Just ctx -> NoClash ctx
    mappend _ _ = Clash

check :: [NameInstance] -> IsClash
check ns = ns <&> isClashOf & mconcat
