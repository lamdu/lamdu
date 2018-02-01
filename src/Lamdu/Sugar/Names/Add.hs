{-# LANGUAGE NoImplicitPrelude, LambdaCase, GeneralizedNewtypeDeriving, RecordWildCards, TypeFamilies, TemplateHaskell, DeriveGeneric, NoMonomorphismRestriction, OverloadedStrings #-}
module Lamdu.Sugar.Names.Add
    ( addToWorkArea
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Reader (Reader, runReader, MonadReader(..))
import qualified Control.Monad.Reader as Reader
import           Control.Monad.State (runState, evalState)
import           Control.Monad.Trans.FastWriter (Writer, runWriter, MonadWriter)
import qualified Control.Monad.Trans.FastWriter as Writer
import           Control.Monad.Transaction (getP, setP)
import qualified Data.Char as Char
import           Data.Foldable (fold)
import qualified Data.Map as Map
import           Data.Monoid.Generic (def_mempty, def_mappend)
import qualified Data.Set as Set
import           Data.UUID.Types (UUID)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Data.Anchors (assocTagNameRef, anonTag)
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Name
import           Lamdu.Sugar.Internal
import           Lamdu.Sugar.Names.CPS (CPS(..), runcps)
import           Lamdu.Sugar.Names.Clash (IsClash(..), AnnotatedName)
import qualified Lamdu.Sugar.Names.Clash as Clash
import           Lamdu.Sugar.Names.NameGen (NameGen)
import qualified Lamdu.Sugar.Names.NameGen as NameGen
import           Lamdu.Sugar.Names.Walk (MonadNaming, Disambiguator)
import qualified Lamdu.Sugar.Names.Walk as Walk
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

------------------------------
---------- Pass 0 ------------
------------------------------
data P0Name = P0Name
    { _p0TagName :: Text
    , _p0internalName :: InternalName
    }

newtype Pass0LoadNames tm a = Pass0LoadNames { runPass0LoadNames :: T tm a }
    deriving (Functor, Applicative, Monad)

instance Monad tm => MonadNaming (Pass0LoadNames tm) where
    type OldName (Pass0LoadNames tm) = InternalName
    type NewName (Pass0LoadNames tm) = P0Name
    type SM (Pass0LoadNames tm) = tm
    opRun = pure runPass0LoadNames
    opWithName _ _ n = CPS $ \inner -> (,) <$> getP0Name n <*> inner
    opGetName _ _ = getP0Name

getP0Name :: Monad tm => InternalName -> Pass0LoadNames tm P0Name
getP0Name internalName =
    assocTagNameRef (internalName ^. inTag) & getP & Pass0LoadNames
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

-- Wrap the Map for a more sensible (recursive) Monoid instance
newtype TagMap = TagMap { _tagMap :: Map T.Tag TagVal }
Lens.makeLenses ''TagMap

instance Semigroup TagMap where
    TagMap x <> TagMap y =
        TagMap $ Map.unionWith (flip (<>)) x y

instance Monoid TagMap where
    mempty = TagMap Map.empty
    mappend = (<>)

tagMapSingleton :: T.Tag -> AnnotatedName -> TagMap
tagMapSingleton k v =
    Map.singleton k val & TagMap
    where
        val =
            TagVal
            { tvAnyGlobal = v ^. Clash.anNameType & Walk.isGlobal
            , tvIsClash = Clash.isClashOf v
            }

data P1Out = P1Out
    { _p1Tags :: TagMap
    , _p1Contexts :: Map T.Tag (Set UUID)
        -- ^ Needed to generate suffixes
    , _p1LocalCollisions :: Set T.Tag
        -- ^ Local (vs [Local|Global]) collisions
    , _p1Texts :: Map Text (Set T.Tag)
    } deriving (Generic)
instance Semigroup P1Out where
    (<>) = def_mappend
instance Monoid P1Out where
    mempty = def_mempty
    mappend = (<>)
Lens.makeLenses ''P1Out

data P1KindedName = P1AnnotatedName AnnotatedName Text | P1AnonName UUID

data P1Name = P1Name
    { p1KindedName :: P1KindedName
    , p1TagsBelow :: TagMap
        -- ^ Allow checking collisions for names hidden behind monadic
        -- actions:
    , p1TextsBelow :: Map Text (Set T.Tag)
        -- ^ We keep the texts below each node so we can check if an
        -- auto-generated name collides with any name in inner scopes
        -- We only use the keys in this map, but we do not strip the
        -- values so all p1TextsBelow can be shared
    }
newtype Pass1PropagateUp (tm :: * -> *) a = Pass1PropagateUp (Writer P1Out a)
    deriving (Functor, Applicative, Monad, MonadWriter P1Out)
runPass1PropagateUp :: Pass1PropagateUp tm a -> (a, P1Out)
runPass1PropagateUp (Pass1PropagateUp act) = runWriter act

tellSome :: MonadWriter w m => Lens.ASetter' w a -> a -> m ()
tellSome l v = mempty & l .~ v & Writer.tell

instance Monad tm => MonadNaming (Pass1PropagateUp tm) where
    type OldName (Pass1PropagateUp tm) = P0Name
    type NewName (Pass1PropagateUp tm) = P1Name
    type SM (Pass1PropagateUp tm) = tm
    opRun = pure (pure . fst . runPass1PropagateUp)
    opWithName _ = p1name Nothing
    opGetName mDisambiguator nameType p0Name =
        p1name mDisambiguator nameType p0Name & runcps

p1Anon ::
    Maybe UUID -> Pass1PropagateUp tm r ->
    Pass1PropagateUp tm (Lamdu.Sugar.Names.Add.P1Name, r)
p1Anon ctx inner =
    case ctx of
    Nothing -> error "Anon tag with no context"
    Just uuid ->
        inner
        & Writer.listen
        <&> \(r, innerOut) ->
            ( P1Name
                { p1KindedName = P1AnonName uuid
                , p1TagsBelow = innerOut ^. p1Tags
                , p1TextsBelow = innerOut ^. p1Texts
                }, r)

p1name ::
    Maybe Disambiguator -> Walk.NameType -> P0Name ->
    CPS (Pass1PropagateUp tm) P1Name
p1name mDisambiguator nameType (P0Name tagText internalName) =
    CPS $ \inner ->
    if tag == anonTag
    then p1Anon ctx inner
    else
        do
            traverse_ (tellSome p1Contexts . Map.singleton tag . Set.singleton) ctx
            (r, innerOut) <-
                tellSome p1Tags (tagMapSingleton tag aName)
                *> tellSome p1Texts (Map.singleton tagText (Set.singleton tag))
                *> inner
                & Writer.listen
            let tags = innerOut ^. p1Tags
            when
                ( Walk.isLocal nameType
                  && (Clash.isClash . tvIsClash) (tags ^. tagMap . Lens.ix tag)
                ) (tellSome p1LocalCollisions (Set.singleton tag))
            pure
                ( P1Name
                    { p1KindedName = P1AnnotatedName aName tagText
                    , p1TagsBelow = tags
                    , p1TextsBelow = innerOut ^. p1Texts
                    }
                , r
                )
    where
        aName =
            Clash.AnnotatedName
            { _anInternal = internalName
            , _anDisambiguator = mDisambiguator
            , _anNameType = nameType
            }
        InternalName ctx tag = internalName

-------------------------------------
---------- Pass1 -> Pass2 -----------
-------------------------------------

makeTagTexts :: Map Text (Set T.Tag) -> Map T.Tag TagText
makeTagTexts p1texts =
    p1texts
    & Lens.imapped %@~ mkTagTexts
    & fold
    where
        mkTagTexts text tags
            | isCollidingName text tags =
                zipWith (mkCollision text) [0..] (Set.toList tags) & Map.fromList
            | otherwise =
                Map.fromSet (const (TagText text NoCollision)) tags
        mkCollision text idx tag = (tag, TagText text (Collision idx))
        -- Type signature needed to avoid an inferred flexible context
        isCollidingName name tagsOfName = isReserved name || Set.size tagsOfName > 1

isReserved :: Text -> Bool
isReserved name =
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
            ]

toSuffixMap :: Map T.Tag (Set UUID) -> Map TaggedVarId Int
toSuffixMap tagContexts =
    tagContexts & Lens.imapped %@~ eachTag & (^.. Lens.folded) & mconcat
    where
        eachTag tag contexts = zipWith (item tag) [0..] (Set.toList contexts) & Map.fromList
        item tag idx uuid = (TaggedVarId uuid tag, idx)

initialP2Env :: P1Out -> P2Env
initialP2Env (P1Out p1tags p1contexts p1localCollisions p1texts) =
    P2Env
    { _p2NameGen = NameGen.initial
    , _p2TagTexts = tagTexts
    , _p2Texts = Map.keysSet p1texts
    , _p2TagSuffixes =
        Map.unionWith (<>) localCollisionContexts globalCollisionContexts
        & toSuffixMap
    , _p2TagsAbove = globalTags
    , _p2TextsAbove = Map.keys globalTags <&> lookupText & Set.fromList
    , _p2Tags = p1tags ^. tagMap <&> tvIsClash
    }
    where
        lookupText tag =
            tagTexts ^? Lens.ix tag . ttText
            & fromMaybe (error "Cannot find global tag in tagTexts")
        tagTexts = makeTagTexts p1texts
        -- TODO: Use OrderedSet for nice ordered suffixes
        localCollisionContexts =
            p1localCollisions & Map.fromSet toContexts
        globalCollisionContexts =
            globalTags & Map.filter Clash.isClash
            & Lens.imapped %@~ \tag _ -> toContexts tag
        toContexts k =
            p1contexts ^. Lens.at k
            & fromMaybe (error "No Contexts for clashing tag??")
        globalTags = p1tags ^. tagMap & Map.mapMaybe justGlobals
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
    , _p2TagTexts :: Map T.Tag TagText
    , _p2Texts :: Set Text
        -- ^ The set of all texts seen in P1 traversal (we do not see hole results)
        -- This is used to identify textual collisions in hole result tags
    , _p2TagSuffixes :: Map TaggedVarId Int
        -- ^ When N (>1) different entities have the same tag in the
        -- same scope, the tag gets a different suffix for each of its
        -- entities
    , _p2TagsAbove :: Map T.Tag IsClash
        -- ^ Tags used in containing scopes (above) -- used to
        -- generate "UnknownCollision" inside hole results
    , _p2TextsAbove :: Set Text
        -- ^ Used to prevent auto-names from re-using texts from above
    , _p2Tags :: Map T.Tag IsClash
        -- ^ All tags including locals from all inner scopes -- used to
        -- check collision of globals in hole results with everything.
    }
Lens.makeLenses ''P2Env

newtype Pass2MakeNames (tm :: * -> *) a = Pass2MakeNames { runPass2MakeNames :: Reader P2Env a }
    deriving (Functor, Applicative, Monad, MonadReader P2Env)

runPass2MakeNamesInitial :: P1Out -> Pass2MakeNames tm a -> a
runPass2MakeNamesInitial p1out act =
    initialP2Env p1out & (runReader . runPass2MakeNames) act

getCollision :: TagMap -> AnnotatedName -> Pass2MakeNames tm Collision
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
            case tags ^. Lens.ix tag <> Clash.isClashOf aName of
            NoClash{} -> NoCollision
            Clash ->
                -- Once a collision, other non-colliding instances
                -- also get a suffix, so we have no idea what suffix
                -- we'll get:
                UnknownCollision
            where
                tags
                    | isGlobal = env ^. p2Tags
                    | otherwise = Map.unionWith (<>) (tagsBelow ^. tagMap <&> tvIsClash) (env ^. p2TagsAbove)
        where
            taggedVarId = TaggedVarId ctx tag
    where
        isGlobal = Lens.has (Clash.anNameType . Walk._GlobalDef) aName
        InternalName mCtx tag = aName ^. Clash.anInternal

instance Monad tm => MonadNaming (Pass2MakeNames tm) where
    type OldName (Pass2MakeNames tm) = P1Name
    type NewName (Pass2MakeNames tm) = Name (T tm)
    type SM (Pass2MakeNames tm) = tm
    opRun = Lens.view id <&> flip (runReader . runPass2MakeNames) <&> (pure .)
    opWithName varInfo _ = p2cpsNameConvertor varInfo
    opGetName _ _ = p2nameConvertor

getTagText :: T.Tag -> Text -> Pass2MakeNames tm TagText
getTagText tag text =
    Lens.view id
    <&> \env ->
    env ^. p2TagTexts . Lens.at tag
    & fromMaybe (TagText text (checkCollision env))
    where
        checkCollision env
            | Set.member text (env ^. p2Texts) = UnknownCollision
            | otherwise = NoCollision

storedName :: Monad tm => TagMap -> AnnotatedName -> Text -> Pass2MakeNames tm (Name (T tm))
storedName tagsBelow aName text =
    Stored setName <$> getTagText tag text <*> getCollision tagsBelow aName
    where
        setName = setP (Anchors.assocTagNameRef tag)
        tag = aName ^. Clash.anTag

p2nameConvertor :: Monad tm => P1Name -> Pass2MakeNames tm (Name (T tm))
p2nameConvertor (P1Name (P1AnnotatedName aName text) tagsBelow _) =
    storedName tagsBelow aName text
p2nameConvertor (P1Name (P1AnonName uuid) _ _) =
    Lens.view p2NameGen
    <&> evalState (NameGen.existingName uuid)
    <&> AutoGenerated

p2cpsNameConvertor :: Monad tm => NameGen.VarInfo -> Walk.CPSNameConvertor (Pass2MakeNames tm)
p2cpsNameConvertor varInfo (P1Name kName tagsBelow textsBelow) =
    CPS $ \inner ->
    do
        env0 <- Lens.view id
        let accept autoText =
                Map.notMember autoText textsBelow
                && Set.notMember autoText (env0 ^. p2TextsAbove)
        (newNameForm, env1) <-
            case kName of
            P1AnnotatedName aName text ->
                storedName tagsBelow aName text
                <&> flip (,) (env0 & p2TagsAbove . Lens.at tag <>~ Just isClash)
                where
                    isClash = Clash.isClashOf aName
                    tag = aName ^. Clash.anTag
            P1AnonName ctx ->
                NameGen.newName accept varInfo ctx
                <&> AutoGenerated
                & Lens.zoom p2NameGen
                & (`runState` env0)
                & pure
        let text = visible newNameForm ^. _1 . ttText
        let env2 = env1 & p2TextsAbove %~ Set.insert text
        res <- Reader.local (const env2) inner
        pure (newNameForm, res)

runPasses ::
    Functor tm =>
    (a -> Pass0LoadNames tm b) -> (b -> Pass1PropagateUp tm c) -> (c -> Pass2MakeNames tm d) ->
    a -> T tm d
runPasses f0 f1 f2 =
    fmap (pass2 . pass1) . pass0
    where
        pass0 = runPass0LoadNames . f0
        pass1 = runPass1PropagateUp . f1
        pass2 (x, p1out) = f2 x & runPass2MakeNamesInitial p1out

addToWorkArea ::
    Monad tm =>
    WorkArea InternalName (T tm) a -> T tm (WorkArea (Name (T tm)) (T tm) a)
addToWorkArea =
    runPasses f f f
    where
        f = Walk.toWorkArea
