{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, TypeFamilies, TemplateHaskell, NoMonomorphismRestriction, OverloadedStrings #-}
module Lamdu.Sugar.Names.Add
    ( addToWorkArea
    , -- re-export for tests
      InternalName(..), inTag, inContext
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Reader (ReaderT(..), Reader, runReader, MonadReader(..))
import qualified Control.Monad.Reader as Reader
import           Control.Monad.State (runState, evalState)
import           Control.Monad.Trans.FastWriter (Writer, runWriter, MonadWriter)
import qualified Control.Monad.Trans.FastWriter as Writer
import qualified Data.Char as Char
import           Data.Foldable (fold)
import           Data.MMap (MMap(..), _MMap)
import qualified Data.MMap as MMap
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Map.Utils (singleton, hasKey)
import           Data.Monoid.Generic (def_mempty, def_mappend)
import           Data.Property (Property(..), MkProperty)
import qualified Data.Property as Property
import qualified Data.Set as Set
import qualified Data.Tuple as Tuple
import           Data.UUID.Types (UUID)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Data.Anchors (anonTag)
import           Lamdu.Name
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Names.Annotated as Annotated
import           Lamdu.Sugar.Names.CPS (CPS(..), runcps, liftCPS)
import           Lamdu.Sugar.Names.Clash (IsClash)
import qualified Lamdu.Sugar.Names.Clash as Clash
import           Lamdu.Sugar.Names.NameGen (NameGen)
import qualified Lamdu.Sugar.Names.NameGen as NameGen
import           Lamdu.Sugar.Names.Walk (MonadNaming(..), Disambiguator)
import qualified Lamdu.Sugar.Names.Walk as Walk
import           Lamdu.Sugar.Types

import           Lamdu.Prelude hiding (Map)

type StoredText o = Property o Text

newtype DisplayText = DisplayText { unDisplayText :: Text }
    deriving (Eq, Ord)

------------------------------
---------- Pass 0 ------------
------------------------------
data P0Name o = P0Name
    { __p0TagName :: StoredText o
    , _p0InternalName :: InternalName
    }
Lens.makeLenses ''P0Name

newtype P0Env i o = P0Env
    { _p0GetNameProp :: T.Tag -> MkProperty i o Text
    }
Lens.makeLenses ''P0Env

newtype Pass0LoadNames i o a =
    Pass0LoadNames { unPass0LoadNames :: ReaderT (P0Env i o) i a }
    deriving (Functor, Applicative, Monad, MonadReader (P0Env i o))

runPass0LoadNames :: P0Env i o -> Pass0LoadNames i o a -> i a
runPass0LoadNames r = (`runReaderT` r) . unPass0LoadNames

instance Monad i => MonadNaming (Pass0LoadNames i o) where
    type OldName (Pass0LoadNames i o) = InternalName
    type NewName (Pass0LoadNames i o) = P0Name o
    type IM (Pass0LoadNames i o) = i
    opRun = Reader.ask <&> runPass0LoadNames
    opWithName _ _ n = CPS $ \inner -> (,) <$> getP0Name n <*> inner
    opGetName _ _ = getP0Name

p0lift :: Monad i => i a -> Pass0LoadNames i o a
p0lift = Pass0LoadNames . lift

getP0Name :: Monad i => InternalName -> Pass0LoadNames i o (P0Name o)
getP0Name internalName =
    do
        nameProp <- Lens.view p0GetNameProp
        nameProp (internalName ^. inTag) ^. Property.mkProperty & p0lift
    <&> (`P0Name` internalName)

------------------------------
---------- Pass 1 ------------
------------------------------

data TagVal = TagVal
    { tvAnyGlobal :: Bool
    , tvIsClash :: IsClash
    }

instance Semigroup TagVal where
    TagVal anyGlobal0 isClash0 <> TagVal anyGlobal1 isClash1 =
        TagVal (anyGlobal0 || anyGlobal1) (isClash0 <> isClash1)

instance Monoid TagVal where
    mempty = TagVal False mempty
    mappend = (<>)

tagMapSingleton :: T.Tag -> Annotated.Name -> MMap T.Tag TagVal
tagMapSingleton k v =
    singleton k val & MMap
    where
        val =
            TagVal
            { tvAnyGlobal = v ^. Annotated.nameType & Walk.isGlobal
            , tvIsClash = Clash.isClashOf v
            }

data P1Out = P1Out
    { _p1Tags :: MMap T.Tag TagVal
    , _p1Contexts :: MMap T.Tag (Set UUID)
        -- ^ Needed to generate suffixes
    , _p1LocalCollisions :: Set T.Tag
        -- ^ Local (vs [Local|Global]) collisions
    , _p1Texts :: MMap DisplayText (Set T.Tag)
    } deriving (Generic)
instance Semigroup P1Out where
    (<>) = def_mappend
instance Monoid P1Out where
    mempty = def_mempty
    mappend = (<>)
Lens.makeLenses ''P1Out

data P1KindedName o = P1StoredName Annotated.Name (StoredText o) | P1AnonName UUID

data P1Name o = P1Name
    { p1KindedName :: P1KindedName o
    , p1TagsBelow :: MMap T.Tag TagVal
        -- ^ Allow checking collisions for names hidden behind monadic
        -- actions:
    , p1TextsBelow :: MMap DisplayText (Set T.Tag)
        -- ^ We keep the texts below each node so we can check if an
        -- auto-generated name collides with any name in inner scopes
        -- We only use the keys in this map, but we do not strip the
        -- values so all p1TextsBelow can be shared
    }
newtype Pass1PropagateUp (im :: * -> *) (am :: * -> *) a =
    Pass1PropagateUp (Writer P1Out a)
    deriving (Functor, Applicative, Monad, MonadWriter P1Out)
runPass1PropagateUp :: Pass1PropagateUp i o a -> (a, P1Out)
runPass1PropagateUp (Pass1PropagateUp act) = runWriter act

tellSome :: MonadWriter w m => Lens.ASetter' w a -> a -> m ()
tellSome l v = mempty & l .~ v & Writer.tell

instance Monad i => MonadNaming (Pass1PropagateUp i o) where
    type OldName (Pass1PropagateUp i o) = P0Name o
    type NewName (Pass1PropagateUp i o) = P1Name o
    type IM (Pass1PropagateUp i o) = i
    opRun = pure (pure . fst . runPass1PropagateUp)
    opWithName _ = p1Name Nothing
    opGetName mDisambiguator nameType p0Name =
        p1Name mDisambiguator nameType p0Name & runcps

p1Anon :: Maybe UUID -> CPS (Pass1PropagateUp i o) (P1Name o)
p1Anon ctx =
    case ctx of
    Nothing -> error "Anon tag with no context"
    Just uuid ->
        CPS (Writer.listen <&> Lens.mapped %~ Tuple.swap . (_2 %~ f))
        where
            f innerOut =
                P1Name
                { p1KindedName = P1AnonName uuid
                , p1TagsBelow = innerOut ^. p1Tags
                , p1TextsBelow = innerOut ^. p1Texts
                }

displayOf :: StoredText o -> DisplayText
displayOf prop
    | Lens.has Lens._Empty text = DisplayText "(empty)"
    | otherwise = DisplayText text
    where
        text = Property.value prop

p1Tagged ::
    Maybe Disambiguator -> Walk.NameType -> P0Name o ->
    CPS (Pass1PropagateUp i o) (P1Name o)
p1Tagged mDisambiguator nameType (P0Name prop internalName) =
    CPS $ \inner ->
    do
        (r, innerOut) <-
            tellSome p1Tags (tagMapSingleton tag aName)
            *> tellSome p1Texts (singleton displayText (Set.singleton tag))
            *> inner
            & Writer.listen
        let tags = innerOut ^. p1Tags
        when
            ( Walk.isLocal nameType
              && (Clash.isClash . tvIsClash) (tags ^. _MMap . Lens.ix tag)
            ) (tellSome p1LocalCollisions (Set.singleton tag))
        pure
            ( P1Name
                { p1KindedName = P1StoredName aName prop
                , p1TagsBelow = tags
                , p1TextsBelow = innerOut ^. p1Texts
                }
            , r
            )
    where
        tag = internalName ^. inTag
        displayText = displayOf prop
        aName =
            Annotated.Name
            { Annotated._internal = internalName
            , Annotated._disambiguator = mDisambiguator
            , Annotated._nameType = nameType
            }

p1Name ::
    Maybe Disambiguator -> Walk.NameType -> P0Name o ->
    CPS (Pass1PropagateUp i o) (P1Name o)
p1Name mDisambiguator nameType p0Name =
    -- NOTE: We depend on the anonTag key in the map
    liftCPS (traverse_ (tellSome p1Contexts . singleton tag . Set.singleton) ctx)
    *> if tag == anonTag
        then p1Anon ctx
        else p1Tagged mDisambiguator nameType p0Name
    where
        InternalName ctx tag = p0Name ^. p0InternalName

-------------------------------------
---------- Pass1 -> Pass2 -----------
-------------------------------------

tagText :: DisplayText -> Collision -> TagText
tagText = TagText . unDisplayText

makeTagTexts :: MMap DisplayText (Set T.Tag) -> Map T.Tag TagText
makeTagTexts p1texts =
    p1texts
    & Lens.imapped %@~ mkTagTexts
    & fold
    where
        mkTagTexts text tags
            | isCollidingName text tags =
                zipWith (mkCollision text) [0..] (Set.toList tags) & Map.fromList
            | otherwise =
                Map.fromSet (const (tagText text NoCollision)) tags
        mkCollision text idx tag = (tag, tagText text (Collision idx))
        isCollidingName text tagsOfName =
            isReserved text || Set.size tagsOfName > 1

isReserved :: DisplayText -> Bool
isReserved (DisplayText name) =
    name `Set.member` reservedWords
    || (name ^? Lens.ix 0 <&> Char.isDigit & fromMaybe False)
    where
        reservedWords =
            Set.fromList
            [ "if", "elif", "else"
            , "case", "of"
            , "let"
            , "or"
            , "λ", "«", "»", "Ø", "|", ".", "→", "➾"
            , "Unnamed"
            ]

toSuffixMap :: MMap T.Tag (Set UUID) -> Map TaggedVarId CollisionSuffix
toSuffixMap tagContexts =
    tagContexts & Lens.imapped %@~ eachTag & (^.. Lens.folded) & mconcat
    where
        eachTag tag contexts = zipWith (item tag) [0..] (Set.toList contexts) & Map.fromList
        item tag idx uuid = (TaggedVarId uuid tag, idx)

initialP2Env :: P1Out -> P2Env
initialP2Env (P1Out p1tags p1contexts p1localCollisions p1texts) =
    P2Env
    { _p2NameGen = NameGen.initial
    , _p2AnonSuffixes =
        p1contexts ^.. Lens.ix anonTag . Lens.folded & (`zip` [0..])
        & Map.fromList
    , _p2TagTexts = tagTexts
    , _p2Texts = MMap.keysSet p1texts
    , _p2TagSuffixes =
        localCollisionContexts <> globalCollisionContexts
        & toSuffixMap
    , _p2TagsAbove = globalTags
    , _p2TextsAbove = globalTags ^@.. Lens.itraversed <&> fst <&> lookupText & Set.fromList
    , _p2Tags = p1tags <&> tvIsClash
    }
    where
        lookupText tag =
            tagTexts ^? Lens.ix tag . ttText
            & fromMaybe (error "Cannot find global tag in tagTexts")
            & DisplayText
        tagTexts = makeTagTexts p1texts
        -- TODO: Use OrderedSet for nice ordered suffixes
        localCollisionContexts =
            p1localCollisions & MMap.fromSet toContexts
        globalCollisionContexts =
            globalTags & MMap.filter Clash.isClash
            & Lens.imapped %@~ \tag _ -> toContexts tag
        toContexts k =
            p1contexts ^. Lens.at k
            & fromMaybe (error "No Contexts for clashing tag??")
        globalTags = MMap.mapMaybe justGlobals p1tags
        justGlobals (TagVal True isClash) = Just isClash
        justGlobals (TagVal False _) = Nothing


------------------------------
---------- Pass 2 ------------
------------------------------
----- Add tag suffixes -------
------------------------------

-- Like InternalName, but necessarily with a context and non-anonymous tag
data TaggedVarId = TaggedVarId
    { _tvCtx :: UUID -- InternalName's context
    , _tvTag :: T.Tag   -- InternalName's tag
    } deriving (Eq, Ord)

data P2Env = P2Env
    { _p2NameGen :: NameGen UUID -- Map anon name contexts to chosen auto-names
    , _p2AnonSuffixes :: Map UUID CollisionSuffix
        -- ^ Untagged global names (defs/nominals) are presented as
        -- "Unnamed" with a collision suffix. This maps the contexts
        -- (def/nominal ids) to the suffix
    , _p2TagTexts :: Map T.Tag TagText
    , _p2Texts :: Set DisplayText
        -- ^ The set of all texts seen in P1 traversal (we do not see hole results)
        -- This is used to identify textual collisions in hole result tags
    , _p2TagSuffixes :: Map TaggedVarId CollisionSuffix
        -- ^ When N (>1) different entities have the same tag in the
        -- same scope, the tag gets a different suffix for each of its
        -- entities
    , _p2TagsAbove :: MMap T.Tag IsClash
        -- ^ Tags used in containing scopes (above) -- used to
        -- generate "UnknownCollision" inside hole results
    , _p2TextsAbove :: Set DisplayText
        -- ^ Used to prevent auto-names from re-using texts from above
    , _p2Tags :: MMap T.Tag IsClash
        -- ^ All tags including locals from all inner scopes -- used to
        -- check collision of globals in hole results with everything.
    }
Lens.makeLenses ''P2Env

newtype Pass2MakeNames (im :: * -> *) (am :: * -> *) a = Pass2MakeNames { runPass2MakeNames :: Reader P2Env a }
    deriving (Functor, Applicative, Monad, MonadReader P2Env)

runPass2MakeNamesInitial :: P1Out -> Pass2MakeNames i o a -> a
runPass2MakeNamesInitial p1out act =
    initialP2Env p1out & (runReader . runPass2MakeNames) act

getCollision :: MMap T.Tag TagVal -> Annotated.Name -> Pass2MakeNames i o Collision
getCollision tagsBelow aName =
    case mCtx of
    Nothing -> pure NoCollision -- simple tag has no tag collisions
    Just ctx ->
        Lens.view id
        <&> \env ->
        case env ^. p2TagSuffixes . Lens.at taggedVarId of
        Just suffix -> Collision suffix
        Nothing ->
            -- In hole results, the collsions suffixes are not precomputed,
            -- but rather computed here:
            if tags ^. Lens.ix tag <> Clash.isClashOf aName & Clash.isClash
            then
                -- Once a collision, other non-colliding instances
                -- also get a suffix, so we have no idea what suffix
                -- we'll get:
                UnknownCollision
            else NoCollision
            where
                tags
                    | isGlobal = env ^. p2Tags
                    | otherwise =
                          (tagsBelow <&> tvIsClash)
                          <> (env ^. p2TagsAbove)
        where
            taggedVarId = TaggedVarId ctx tag
    where
        isGlobal = Lens.has (Annotated.nameType . Walk._GlobalDef) aName
        InternalName mCtx tag = aName ^. Annotated.internal

instance Monad i => MonadNaming (Pass2MakeNames i o) where
    type OldName (Pass2MakeNames i o) = P1Name o
    type NewName (Pass2MakeNames i o) = Name o
    type IM (Pass2MakeNames i o) = i
    opRun = Lens.view id <&> flip (runReader . runPass2MakeNames) <&> (pure .)
    opWithName varInfo _ = p2cpsNameConvertor varInfo
    opGetName _ = p2nameConvertor

getTagText :: T.Tag -> StoredText o -> Pass2MakeNames i o TagText
getTagText tag prop =
    Lens.view id
    <&> \env ->
    env ^. p2TagTexts . Lens.at tag
    & fromMaybe (TagText (unDisplayText displayText) (checkCollision env))
    where
        displayText = displayOf prop
        checkCollision env
            | Set.member displayText (env ^. p2Texts) = UnknownCollision
            | otherwise = NoCollision

storedName ::
    MMap T.Tag TagVal -> Annotated.Name -> StoredText o ->
    Pass2MakeNames i o (Name o)
storedName tagsBelow aName prop =
    StoredName prop
    <$> getTagText tag prop
    <*> getCollision tagsBelow aName
    <&> Stored
    where
        tag = aName ^. Annotated.tag

p2nameConvertor :: Walk.NameType -> P1Name o -> Pass2MakeNames i o (Name o)
p2nameConvertor nameType (P1Name (P1StoredName aName text) tagsBelow _) =
    storedName tagsBelow aName text
    <&>
    case nameType of
    Walk.TaggedNominal -> _Stored . snDisplayText . ttText . Lens.ix 0 %~ Char.toUpper
    _ -> id
p2nameConvertor nameType (P1Name (P1AnonName uuid) _ _) =
    case nameType of
    Walk.Tag -> error "TODO: Refactor types to rule this out"
    Walk.TaggedVar ->
        Lens.view p2NameGen
        <&> evalState (NameGen.existingName uuid)
        <&> AutoGenerated
    Walk.TaggedNominal -> globalAnon
    Walk.GlobalDef -> globalAnon
    where
        globalAnon =
            Lens.view (p2AnonSuffixes . Lens.at uuid)
            <&> fromMaybe bugCollision <&> Unnamed
        -- Holes results should never have unnamed subexprs, instead
        -- of crashing, show -1 as the suffix
        bugCollision = -1

p2cpsNameConvertor :: NameGen.VarInfo -> Walk.CPSNameConvertor (Pass2MakeNames i o)
p2cpsNameConvertor varInfo (P1Name kName tagsBelow textsBelow) =
    CPS $ \inner ->
    do
        env0 <- Lens.view id
        let accept autoText =
                not (hasKey autoText textsBelow)
                && Set.notMember autoText (env0 ^. p2TextsAbove)
        (newNameForm, env1) <-
            case kName of
            P1StoredName aName text ->
                storedName tagsBelow aName text
                <&> flip (,) (env0 & p2TagsAbove . Lens.at tag <>~ Just isClash)
                where
                    isClash = Clash.isClashOf aName
                    tag = aName ^. Annotated.tag
            P1AnonName ctx ->
                NameGen.newName (accept . DisplayText) varInfo ctx
                <&> AutoGenerated
                & Lens.zoom p2NameGen
                & (`runState` env0)
                & pure
        let text = visible newNameForm ^. _1 . ttText & DisplayText
        let env2 = env1 & p2TextsAbove %~ Set.insert text
        res <- Reader.local (const env2) inner
        pure (newNameForm, res)

runPasses ::
    Functor i =>
    (T.Tag -> MkProperty i o Text) ->
    (a -> Pass0LoadNames i o b) ->
    (b -> Pass1PropagateUp i o c) ->
    (c -> Pass2MakeNames i o d) ->
    a -> i d
runPasses getNameProp f0 f1 f2 =
    fmap (pass2 . pass1) . pass0
    where
        pass0 = runPass0LoadNames (P0Env getNameProp) . f0
        pass1 = runPass1PropagateUp . f1
        pass2 (x, p1out) = f2 x & runPass2MakeNamesInitial p1out

addToWorkArea ::
    Monad i =>
    (T.Tag -> MkProperty i o Text) ->
    WorkArea InternalName i o a ->
    i (WorkArea (Name o) i o a)
addToWorkArea getNameProp =
    runPasses getNameProp f f f
    where
        f = Walk.toWorkArea
