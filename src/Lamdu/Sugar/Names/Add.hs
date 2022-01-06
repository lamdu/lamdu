{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction, TupleSections, DerivingVia, NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeApplications #-}

module Lamdu.Sugar.Names.Add
    ( addToWorkArea
    , -- re-export for tests
      addToWorkAreaTest
    , InternalName(..), inTag, inContext, runPasses
    ) where

import           Control.Lens (ALens)
import qualified Control.Lens as Lens
import           Control.Lens.Extended ((~~>))
import           Control.Monad (zipWithM)
import           Control.Monad.Reader (ReaderT(..), Reader, runReader)
import qualified Control.Monad.Reader as Reader
import           Control.Monad.State (evalState)
import qualified Control.Monad.Writer as Writer
import           Control.Monad.Trans.FastWriter (Writer, runWriter, MonadWriter)
import qualified Data.Char as Char
import           Data.Coerce (coerce)
import           Data.Foldable (fold)
import           Data.MMap (MMap(..))
import qualified Data.MMap as MMap
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Set.Ordered (OSet, Bias(..), L)
import qualified Data.Set.Ordered as OrderedSet
import qualified Data.Text as Text
import qualified Data.Tuple as Tuple
import           Data.UUID.Types (UUID)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Data.Anchors (anonTag)
import qualified Lamdu.Data.Tag as Tag
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.Name as Texts
import           Lamdu.Name
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Names.Annotated as Annotated
import           Lamdu.Sugar.Names.CPS (CPS(..), runcps, liftCPS)
import qualified Lamdu.Sugar.Names.Clash as Clash
import           Lamdu.Sugar.Names.Walk (MonadNaming(..), Disambiguator)
import qualified Lamdu.Sugar.Names.Walk as Walk
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

------------------------------
---------- Pass 0 ------------
------------------------------
data P0Name = P0Name
    { _p0TagName :: Tag.TextsInLang
    , _p0IsOperator :: Tag.IsOperator
    , _p0InternalName :: InternalName
    }

newtype P0Env i = P0Env
    { _p0GetName :: T.Tag -> i (Tag.IsOperator, Tag.TextsInLang)
    }
Lens.makeLenses ''P0Env

newtype Pass0LoadNames i a =
    Pass0LoadNames { unPass0LoadNames :: ReaderT (P0Env i) i a }
    deriving newtype (Functor, Applicative, Monad, MonadReader (P0Env i))

runPass0LoadNames :: P0Env i -> Pass0LoadNames i a -> i a
runPass0LoadNames r = (`runReaderT` r) . unPass0LoadNames

instance Monad i => MonadNaming (Pass0LoadNames i) where
    type OldName (Pass0LoadNames i) = InternalName
    type NewName (Pass0LoadNames i) = P0Name
    type IM (Pass0LoadNames i) = i
    opRun = Reader.ask <&> runPass0LoadNames
    opWithName _ _ n = CPS $ \inner -> (,) <$> getP0Name n <*> inner
    opGetName _ _ _ = getP0Name
    opWithNewTag _ _ = id

p0lift :: Monad i => i a -> Pass0LoadNames i a
p0lift = Pass0LoadNames . lift

getP0Name :: Monad i => InternalName -> Pass0LoadNames i P0Name
getP0Name internalName =
    Lens.view p0GetName ?? internalName ^. inTag >>= p0lift
    <&> \(isOp, x) -> P0Name x isOp internalName

------------------------------
---------- Pass 1 ------------
------------------------------

newtype Collider = Collider Clash.Info deriving stock Show
instance Semigroup Collider where
    Collider x <> Collider y = Collider (x `Clash.collide` y)

-- 2 wrappers for coerce for readability/safety
uncolliders :: MMap T.Tag Collider -> MMap T.Tag Clash.Info
uncolliders = coerce

colliders :: MMap T.Tag Clash.Info -> MMap T.Tag Collider
colliders = coerce

data P1Out = P1Out
    { _p1Globals :: MMap T.Tag Collider
        -- ^ Used in P2 to check against local hole results
    , _p1Locals :: MMap T.Tag Clash.Info
        -- ^ Used in P2 to check against global hole results
    , _p1Contexts :: MMap T.Tag (MMap Walk.NameType (Bias L (OSet UUID)))
        -- ^ Needed to generate suffixes
    , _p1TypeVars :: Bias L (OSet UUID)
        -- ^ Type vars met
    , _p1Texts :: Map T.Tag Tag.TextsInLang
    }
    deriving stock (Generic, Show)
    deriving (Semigroup, Monoid) via Generically P1Out
Lens.makeLenses ''P1Out

data P1KindedName
    = P1TagName Annotated.Name Tag.IsOperator Tag.TextsInLang
    | P1AnonName UUID

data P1Name = P1Name
    { p1KindedName :: P1KindedName
    , p1LocalsBelow :: MMap T.Tag Clash.Info
        -- ^ Allow checking collisions for names hidden behind monadic
        -- actions.
    , p1IsAutoGen :: Bool
    }
newtype Pass1PropagateUp (im :: * -> *) (am :: * -> *) a =
    Pass1PropagateUp (Writer P1Out a)
    deriving newtype (Functor, Applicative, Monad, MonadWriter P1Out)
runPass1PropagateUp :: Pass1PropagateUp i o a -> (a, P1Out)
runPass1PropagateUp (Pass1PropagateUp act) = runWriter act

tellSome :: MonadWriter w m => Lens.ASetter' w a -> a -> m ()
tellSome l v = mempty & l .~ v & Writer.tell

instance Monad i => MonadNaming (Pass1PropagateUp i o) where
    type OldName (Pass1PropagateUp i o) = P0Name
    type NewName (Pass1PropagateUp i o) = P1Name
    type IM (Pass1PropagateUp i o) = i
    opRun = pure (pure . fst . runPass1PropagateUp)
    opWithName = p1Name Nothing
    opGetName mDisambiguator u nameType p0Name =
        p1Name mDisambiguator u nameType p0Name & runcps
    opWithNewTag _ _ = id

displayOf :: Has (Texts.Name Text) env => env -> Text -> Text
displayOf env text
    | Lens.has Lens._Empty text = env ^. has . Texts.emptyName
    | otherwise = text

p1Name ::
    Maybe Disambiguator -> Walk.IsUnambiguous -> Walk.NameType -> P0Name ->
    CPS (Pass1PropagateUp i o) P1Name
p1Name mDisambiguator u nameType (P0Name texts isOp internalName) =
    -- NOTE: We depend on the anonTag key in the map
    liftCPS (traverse_ tellCtx ctx) *>
    CPS (\inner ->
        tells
        *> inner
        & Writer.censor addTags -- censor avoids clash-skipping monoid instance
        & Writer.listen
        <&> Tuple.swap
        <&> _1 %~ \innerOut ->
        P1Name
        { p1KindedName =
            if tag == anonTag
            then
                case ctx of
                Nothing -> error "Anon tag with no context"
                Just uuid -> P1AnonName uuid
            else P1TagName aName isOp texts
        , p1LocalsBelow = innerOut ^. p1Locals
        , p1IsAutoGen = internalName ^. inIsAutoName
        }
    )
    where
        tells
            | tag /= anonTag = tag ~~> texts & tellSome p1Texts
            | otherwise = pure ()
        addTags =
            case u of
            Walk.Unambiguous -> id
            Walk.MayBeAmbiguous
                | Walk.isGlobal nameType -> p1Globals <>~ myTags
                | otherwise -> p1Locals . col <>~ myTags
            where
                col = Lens.iso colliders uncolliders -- makes it have colliders
                myTags = tag ~~> Collider (Clash.infoOf aName)
        tellCtx x
            | nameType == Walk.TypeVar =
                tellSome p1TypeVars (Bias (OrderedSet.singleton x))
            | otherwise =
                tellSome p1Contexts (tag ~~> (nameType ~~> Bias (OrderedSet.singleton x)))
        InternalName ctx tag _ = internalName
        aName =
            Annotated.Name
            { Annotated._internal = internalName
            , Annotated._disambiguator = mDisambiguator
            , Annotated._nameType = nameType
            }

-------------------------------------
---------- Pass1 -> Pass2 -----------
-------------------------------------

tagText :: Has (Texts.Name Text) env => env -> Text -> Collision -> TagText
tagText env = TagText . displayOf env

countMissing :: Int -> ALens a b (Maybe i) (Either Int i) -> [a] -> [b]
countMissing _ _ [] = []
countMissing c l (x:xs) =
    case x ^# l of
    Just i -> (x & Lens.cloneLens l .~ Right i) : countMissing c l xs
    Nothing -> (x & Lens.cloneLens l .~ Left c) : countMissing (c+1) l xs

makeTagTexts ::
    ( Has (Texts.Name Text) env
    , Has (Texts.Code Text) env
    ) =>
    env -> Map T.Tag Tag.TextsInLang -> Map T.Tag TagText
makeTagTexts env p1texts =
    p1texts
    ^@.. Lens.itraversed
    <&> (\(tag, texts) -> (texts ^. Tag.name, Map.singleton tag (texts ^. Tag.disambiguationText)))
    & MMap.fromList
    & Lens.imapped %@~ mkTagTexts
    & fold
    where
        mkTagTexts fullText tags
            | isReserved env fullText || Map.size tags > 1 =
                countMissing 0 _2 (tags ^@.. Lens.itraversed)
                <&> _2 %~ tagText env fullText . Collision . either (Text.pack . show) id
                & Map.fromList
            | otherwise =
                Lens.imap mkTagText tags
                where
                    mkTagText tag _ =
                        tagText env text NoCollision
                        where
                            text =
                                abbreviations ^? Lens.ix tag
                                & fromMaybe fullText
        fullTexts = p1texts ^.. traverse . Tag.name & Set.fromList
        abbreviationTags =
            p1texts
            ^@.. Lens.itraversed <. Tag.abbreviation . Lens._Just
            <&> (\(tag, abr) -> (abr, Set.singleton tag))
            & filter (not . (`Set.member` fullTexts) . fst)
            & MMap.fromList
        abbreviations =
            abbreviationTags
            ^@.. Lens.itraversed <. Lens.to singleTag . Lens._Just
            <&> Tuple.swap
            & Map.fromList
        singleTag tags
            | Set.size tags > 1 = Nothing
            | otherwise = Set.lookupMin tags

isReserved ::
    ( Has (Texts.Name Text) env
    , Has (Texts.Code Text) env
    ) =>
    env -> Text -> Bool
isReserved env name =
    reservedWords ^. Lens.contains name
    || (name ^? Lens.ix 0 & any Char.isDigit)
    where
        reservedWords =
            env ^.. (has @(Texts.Code Text) . Lens.folded <> has @(Texts.Name Text) . Lens.folded)
            & Set.fromList

toSuffixMap ::
    Map T.Tag TagText ->
    MMap T.Tag (MMap Walk.NameType (Bias L (OSet UUID))) ->
    MMap T.Tag Clash.Info ->
    Map TaggedVarId Int
toSuffixMap tagTexts contexts top =
    evalState (Lens.itraverse eachTag collisions) nonCollisionTexts ^. Lens.folded
    where
        eachTag tag ctx =
            do
                texts <- Lens.use id
                let indices = filter (\i -> not (texts ^. Lens.contains (addSuf i))) [0 ..]
                zipWithM item indices (ctx ^.. Lens.folded . Lens.folded)
                <&> Map.fromList
            where
                addSuf :: Int -> Text
                addSuf i = txt <> Text.pack (show i)
                txt = tagTexts ^?! Lens.ix tag . ttText
                item idx uuid =
                    (TaggedVarId uuid tag, idx) <$ (Lens.contains (addSuf idx) .= True)
        collisions =
            MMap.filter Clash.isClash top
            & Lens.imapped %@~ \tag _ -> toContexts tag
        nonCollisionTexts =
            MMap.filter (not . Clash.isClash) top ^.. Lens.ifolded . Lens.asIndex
            & foldMap (\t -> tagTexts ^.. Lens.ix t . ttText)
            & Set.fromList
        toContexts k =
            fromMaybe (error "No Contexts for clashing tag??") (contexts ^. Lens.at k)
            ^. traverse

numberCycle :: [Text] -> [Text]
numberCycle s =
    (s <>) . mconcat . Lens.imap appendAll $ repeat s
    where
        appendAll num = map (<> Text.pack (show num))

initialP2Env ::
    ( Has (Texts.Name Text) env
    , Has (Texts.Code Text) env
    ) =>
    env -> P1Out -> P2Env
initialP2Env env P1Out{_p1Globals, _p1Locals, _p1Contexts, _p1TypeVars, _p1Texts} =
    P2Env
    { _p2TypeVars =
        numberCycle ["a", "b", "c"]
        & zip (_p1TypeVars ^.. Lens.folded . Lens.folded)
        & Map.fromList
    , _p2TagTexts = tagTexts
    , _p2Texts = _p1Texts ^. traverse . Tag.name . Lens.to Set.singleton
    , _p2TagSuffixes = toSuffixMap tagTexts _p1Contexts top
    , _p2TagsAbove = uncolliders _p1Globals
        -- all globals are "above" everything, and locals add up as
        -- we descend
    , _p2AutoNames = mempty
    , _p2Tags = top
    , _p2NameTexts = env ^. has
    }
    where
        tagTexts = makeTagTexts env _p1Texts
        top = colliders _p1Locals <> _p1Globals & uncolliders


------------------------------
---------- Pass 2 ------------
------------------------------
----- Add tag suffixes -------
------------------------------

-- Like InternalName, but necessarily with a context and non-anonymous tag
data TaggedVarId = TaggedVarId
    { _tvCtx :: UUID -- InternalName's context
    , _tvTag :: T.Tag   -- InternalName's tag
    } deriving (Eq, Ord, Show)

data P2Env = P2Env
    { _p2TypeVars :: Map UUID Text
        -- ^ Names for type variables. Globally unique.
    , _p2TagTexts :: Map T.Tag TagText
    , _p2Texts :: Set Text
        -- ^ The set of all texts seen in P1 traversal (we do not see hole results)
        -- This is used to identify textual collisions in hole result tags
    , _p2TagSuffixes :: Map TaggedVarId Int
        -- ^ When tags collide in the overlapping scopes, the tag gets
        -- a different suffix for each of its entities in ALL scopes
    , _p2AutoNames :: Map UUID T.Tag
    , _p2TagsAbove :: MMap T.Tag Clash.Info
        -- ^ All global tags AND local tags from above -- used to
        -- generate "UnknownCollision" inside hole results
    , _p2Tags :: MMap T.Tag Clash.Info
        -- ^ All local AND global tags from all scopes everywhere --
        -- used to check collision of globals in hole results with
        -- everything.
    , _p2NameTexts :: Texts.Name Text
    }
Lens.makeLenses ''P2Env

instance Has (Texts.Name Text) P2Env where has = p2NameTexts

newtype Pass2MakeNames (im :: * -> *) (am :: * -> *) a = Pass2MakeNames { runPass2MakeNames :: Reader P2Env a }
    deriving newtype (Functor, Applicative, Monad, MonadReader P2Env)

runPass2MakeNamesInitial ::
    ( Has (Texts.Name Text) env
    , Has (Texts.Code Text) env
    ) =>
    env -> P1Out -> Pass2MakeNames i o a -> a
runPass2MakeNamesInitial env p1out act =
    initialP2Env env p1out & (runReader . runPass2MakeNames) act

getCollision :: MMap T.Tag Clash.Info -> Annotated.Name -> Pass2MakeNames i o Collision
getCollision tagsBelow aName =
    case mCtx of
    Nothing -> pure NoCollision -- simple tag has no tag collisions
    Just ctx ->
        Lens.view id
        <&> \env ->
        case env ^. p2TagSuffixes . Lens.at (TaggedVarId ctx tag) of
        Just suffix -> Collision (Text.pack (show suffix))
        Nothing ->
            -- In hole results, the collsions suffixes are not precomputed,
            -- but rather computed here:
            if Clash.collide (tags ^. Lens.ix tag) (Clash.infoOf aName) & Clash.isClash
            then
                -- Once a collision, other non-colliding instances
                -- also get a suffix, so we have no idea what suffix
                -- we'll get:
                UnknownCollision
            else NoCollision
            where
                tags
                    -- A global name in a hole result must be checked
                    -- against ALL names everywhere:
                    | Walk.isGlobal (aName ^. Annotated.nameType) = env ^. p2Tags
                    -- A non-global name needs to be checked against
                    -- names above/below (but not sibling scopes)
                    | otherwise =
                        MMap.unionWith Clash.collide tagsBelow (env ^. p2TagsAbove)
    where
        InternalName mCtx tag _ = aName ^. Annotated.internal

instance Monad i => MonadNaming (Pass2MakeNames i o) where
    type OldName (Pass2MakeNames i o) = P1Name
    type NewName (Pass2MakeNames i o) = Name
    type IM (Pass2MakeNames i o) = i
    opRun = Lens.view id <&> flip (runReader . runPass2MakeNames) <&> (pure .)
    opWithName u _ = p2cpsNameConvertor u
    opGetName _ = p2nameConvertor
    opWithNewTag tag text = local (p2TagTexts . Lens.at tag ?~ TagText text NoCollision)

getTag :: Bool -> Annotated.Name -> Pass2MakeNames i o T.Tag
getTag autoGen aName =
    case aName ^. Annotated.internal . inContext of
    Just uuid | autoGen -> Lens.view (p2AutoNames . Lens.at uuid)
    _ -> pure Nothing
    <&> fromMaybe (aName ^. Annotated.tag)

getTagText :: T.Tag -> Tag.TextsInLang -> Pass2MakeNames i o TagText
getTagText tag texts =
    Lens.view id
    <&>
    \env ->
    let displayText = displayOf env (texts ^. Tag.name)
        collision
            | env ^. p2Texts . Lens.contains displayText = UnknownCollision
            | otherwise = NoCollision
    in
    env ^. p2TagTexts . Lens.at tag
    & fromMaybe (TagText displayText collision)

p2tagName ::
    Walk.IsUnambiguous -> MMap T.Tag Clash.Info ->
    Annotated.Name -> Tag.TextsInLang -> Bool -> Tag.IsOperator ->
    Pass2MakeNames i o Name
p2tagName u tagsBelow aName texts isAutoGen isOp =
    do
        tag <- getTag isAutoGen aName
        let aNameNew = aName & Annotated.tag .~ tag
        TagName
            <$> getTagText tag texts
            <*> case u of
                Walk.Unambiguous -> pure NoCollision
                Walk.MayBeAmbiguous -> getCollision tagsBelow aNameNew
            ?? isAutoGen
            ?? isOp == Tag.IsAnOperator
            <&> NameTag

p2globalAnon :: UUID -> Pass2MakeNames i o Name
p2globalAnon uuid =
    Lens.view (p2TagSuffixes . Lens.at (TaggedVarId uuid anonTag))
    <&> Unnamed . fromMaybe 0

p2nameConvertor :: Walk.IsUnambiguous -> Walk.NameType -> P1Name -> Pass2MakeNames i o Name
p2nameConvertor u nameType (P1Name (P1TagName aName isOp texts) tagsBelow isAutoGen) =
    p2tagName u tagsBelow aName texts isAutoGen isOp <&>
    case nameType of
    Walk.TaggedNominal -> _NameTag . tnDisplayText . ttText . Lens.ix 0 %~ Char.toUpper
    _ -> id
p2nameConvertor _ nameType (P1Name (P1AnonName uuid) _ _) =
    case nameType of
    Walk.Tag -> error "TODO: Refactor types to rule this out"
    Walk.TaggedVar -> p2globalAnon uuid
    Walk.TypeVar ->
        Lens.view (p2TypeVars . Lens.at uuid)
        <&> fromMaybe "?" -- TODO: Type variable in hole result
        <&> AutoGenerated
    Walk.TaggedNominal -> p2globalAnon uuid
    Walk.GlobalDef -> p2globalAnon uuid

p2cpsNameConvertor :: Walk.IsUnambiguous -> Walk.CPSNameConvertor (Pass2MakeNames i o)
p2cpsNameConvertor _ (P1Name (P1AnonName uuid) _ _) = p2globalAnon uuid & liftCPS
p2cpsNameConvertor u (P1Name (P1TagName aName isOp texts) tagsBelow isAutoGen) =
    CPS $ \inner ->
    (,)
    <$> p2tagName u tagsBelow aName texts isAutoGen isOp
    <*> local (addToTagsAbove . addAutoName) inner
    where
        isClash = Clash.infoOf aName
        tag = aName ^. Annotated.tag
        mUuid = aName ^. Annotated.internal . inContext
        addAutoName =
            case mUuid of
            Just uuid | isAutoGen -> p2AutoNames . Lens.at uuid ?~ tag
            _ -> id
        addToTagsAbove = case u of
            Walk.Unambiguous -> id
            Walk.MayBeAmbiguous -> p2TagsAbove . Lens.at tag %~ Just . maybe isClash (Clash.collide isClash)

runPasses ::
    ( Has (Texts.Name Text) env
    , Has (Texts.Code Text) env
    , Functor i
    ) =>
    env ->
    (T.Tag -> i (Tag.IsOperator, Tag.TextsInLang)) ->
    (a -> Pass0LoadNames i b) ->
    (b -> Pass1PropagateUp i o c) ->
    (c -> Pass2MakeNames i o d) ->
    a -> i d
runPasses env getName f0 f1 f2 =
    fmap (pass2 . pass1) . pass0
    where
        pass0 = runPass0LoadNames (P0Env getName) . f0
        pass1 = runPass1PropagateUp . f1
        pass2 (x, p1out) = f2 x & runPass2MakeNamesInitial env p1out

addToWorkArea ::
    ( Has (Texts.Name Text) env
    , Has (Texts.Code Text) env
    , Monad i
    ) =>
    env ->
    (T.Tag -> i (Tag.IsOperator, Tag.TextsInLang)) ->
    WorkArea (Annotation (EvaluationScopes InternalName i) InternalName) InternalName i o
        (ConvertPayload m (Annotation (EvaluationScopes InternalName i) InternalName, a)) ->
    i (WorkArea (Annotation (EvaluationScopes Name i) Name) Name i o
        (ConvertPayload m (Annotation (EvaluationScopes Name i) Name, a)))
addToWorkArea env getName =
    runPasses env getName f f f
    where
        f = Walk.toWorkArea

-- TODO: Switch to regular type in tests
addToWorkAreaTest ::
    ( Has (Texts.Name Text) env
    , Has (Texts.Code Text) env
    , Monad i
    ) =>
    env ->
    (T.Tag -> i (Tag.IsOperator, Tag.TextsInLang)) ->
    WorkArea (Annotation (EvaluationScopes InternalName i) InternalName) InternalName i o
        (Payload (Annotation (EvaluationScopes InternalName i) InternalName) o) ->
    i (WorkArea (Annotation (EvaluationScopes Name i) Name) Name i o
        (Payload (Annotation (EvaluationScopes Name i) Name) o))
addToWorkAreaTest env getName =
    runPasses env getName f f f
    where
        f = Walk.toWorkAreaTest
