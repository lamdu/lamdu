{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns, MultiParamTypeClasses, FlexibleInstances, TypeApplications #-}

module Lamdu.Sugar.Names.Add.Pass2MakeNames
    ( Pass2MakeNames(..), runPass2MakeNamesInitial
    ) where

import           Control.Lens (ALens)
import qualified Control.Lens as Lens
import           Control.Monad (zipWithM)
import           Control.Monad.Reader (ReaderT(..), Reader, runReader)
import           Control.Monad.State (evalState)
import qualified Data.Char as Char
import           Data.Foldable (fold)
import           Data.Kind (Type)
import           Data.MMap (MMap(..))
import qualified Data.MMap as MMap
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Set.Ordered (OSet, Bias(..), L)
import qualified Data.Text as Text
import           Data.UUID.Types (UUID)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Data.Anchors (anonTag)
import qualified Lamdu.Data.Tag as Tag
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.Name as Texts
import           Lamdu.Name
import           Lamdu.Sugar.Internal
import           Lamdu.Sugar.Names.Add.Abbreviations (abbreviations)
import           Lamdu.Sugar.Names.Add.Pass1PropagateUp
import qualified Lamdu.Sugar.Names.Annotated as Annotated
import           Lamdu.Sugar.Names.CPS (CPS(..), liftCPS)
import qualified Lamdu.Sugar.Names.Clash as Clash
import           Lamdu.Sugar.Names.Walk (MonadNameWalk(..))
import qualified Lamdu.Sugar.Names.Walk as Walk
import           Lamdu.Sugar.Types (TagSuffixes, TaggedVarId(..))

import           Lamdu.Prelude

displayOf :: Has (Texts.Name Text) env => env -> Text -> Text
displayOf env text
    | Lens.has Lens._Empty text = env ^. has . Texts.emptyName
    | otherwise = text

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
                            text = abbrevs ^? Lens.ix tag & fromMaybe fullText
        abbrevs = abbreviations p1texts

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
    HasCallStack =>
    Texts.Name Text ->
    Map T.Tag TagText ->
    MMap T.Tag (Bias L (OSet UUID)) ->
    MMap T.Tag Clash.Collider ->
    TagSuffixes
toSuffixMap lang tagTexts contexts top =
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
                txt | tag == anonTag = lang ^. Texts.unnamed
                    | otherwise =
                        tagTexts ^? Lens.ix tag . ttText
                        & fromMaybe (error ("No text for tag: " <> show tag))
                item idx uuid =
                    (TaggedVarId uuid tag, idx) <$ (Lens.contains (addSuf idx) .= True)
        collisions =
            MMap.filter (Lens.has (Clash._Collider . Clash._Clash)) top
            & Lens.imapped %@~ const . toContexts
        nonCollisionTexts =
            MMap.filter (Lens.has (Clash._Collider . Clash._NoClash)) top ^.. Lens.ifolded . Lens.asIndex
            & foldMap (\t -> tagTexts ^.. Lens.ix t . ttText)
            & Set.fromList
        toContexts k =
            fromMaybe (error "No Contexts for clashing tag??") (contexts ^. Lens.at k)

numberCycle :: [Text] -> [Text]
numberCycle s =
    (s <>) . mconcat . Lens.imap appendAll $ repeat s
    where
        appendAll num = map (<> Text.pack (show num))

initialP2Env ::
    ( HasCallStack
    , Has (Texts.Name Text) env
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
    , _p2TagSuffixes = (toSuffixMap (env ^. has) tagTexts <$> _p1Contexts <*> top) ^. Lens.folded
    , _p2TagsAbove = _p1Globals
        -- all globals are "above" everything, and locals add up as
        -- we descend
    , _p2AutoNames = mempty
    , _p2Tags = top
    , _p2NameTexts = env ^. has
    }
    where
        tagTexts = makeTagTexts env _p1Texts
        top = _p1Globals & Clash.nsLower <>~ _p1Locals ^. Lens.coerced

data P2Env = P2Env
    { _p2TypeVars :: Map UUID Text
        -- ^ Names for type variables. Globally unique.
    , _p2TagTexts :: Map T.Tag TagText
    , _p2Texts :: Set Text
        -- ^ The set of all texts seen in P1 traversal (we do not see hole results)
        -- This is used to identify textual collisions in hole result tags
    , _p2TagSuffixes :: TagSuffixes
        -- ^ When tags collide in the overlapping scopes, the tag gets
        -- a different suffix for each of its entities in ALL scopes
    , _p2AutoNames :: Map UUID T.Tag
    , _p2TagsAbove :: Clash.NameSpaces (MMap T.Tag Clash.Collider)
        -- ^ All global tags AND local tags from above -- used to
        -- generate "UnknownCollision" inside hole results
    , _p2Tags :: Clash.NameSpaces (MMap T.Tag Clash.Collider)
        -- ^ All local AND global tags from all scopes everywhere --
        -- used to check collision of globals in hole results with
        -- everything.
    , _p2NameTexts :: Texts.Name Text
    }
Lens.makeLenses ''P2Env

instance Has (Texts.Name Text) P2Env where has = p2NameTexts

newtype Pass2MakeNames (im :: Type -> Type) (am :: Type -> Type) a = Pass2MakeNames { runPass2MakeNames :: Reader P2Env a }
    deriving newtype (Functor, Applicative, Monad, MonadReader P2Env)

runPass2MakeNamesInitial ::
    ( HasCallStack
    , Has (Texts.Name Text) env
    , Has (Texts.Code Text) env
    ) =>
    env -> P1Out -> Pass2MakeNames i o a -> a
runPass2MakeNamesInitial env p1out act =
    initialP2Env env p1out & (runReader . runPass2MakeNames) act

getCollision :: MMap T.Tag Clash.Collider -> Annotated.Name -> Pass2MakeNames i o Collision
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
            if tags ^. Clash.nameTypeSpace (aName ^. Annotated.nameType) . Lens.ix tag <> Clash.Collider (Clash.toIsClash aName)
                & Lens.has (Clash._Collider . Clash._Clash)
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
                        env ^. p2TagsAbove & Clash.nsLower %~ MMap.unionWith (<>) tagsBelow
    where
        InternalName mCtx tag _ = aName ^. Annotated.internal

instance Monad i => MonadNameWalk (Pass2MakeNames i o) where
    type OldName (Pass2MakeNames i o) = P1Name
    type NewName (Pass2MakeNames i o) = Name
    type IM (Pass2MakeNames i o) = i
    opRun = Lens.view id <&> flip (runReader . runPass2MakeNames) <&> (pure .)
    opWithName u _ = p2cpsNameConvertor u
    opGetName _ = p2nameConvertor
    opWithNewTag tag text = local (p2TagTexts . Lens.at tag ?~ TagText text NoCollision)
    tagSuffixes = Lens.view p2TagSuffixes

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
    Walk.IsUnambiguous -> MMap T.Tag Clash.Collider ->
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
    p2tagName u (tagsBelow ^. Lens.coerced) aName texts isAutoGen isOp <&>
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

p2cpsNameConvertor :: Walk.IsUnambiguous -> P1Name -> CPS (Pass2MakeNames i o) Name
p2cpsNameConvertor _ (P1Name (P1AnonName uuid) _ _) = p2globalAnon uuid & liftCPS
p2cpsNameConvertor u (P1Name (P1TagName aName isOp texts) tagsBelow isAutoGen) =
    CPS $ \inner ->
    (,)
    <$> p2tagName u (tagsBelow ^. Lens.coerced) aName texts isAutoGen isOp
    <*> local (addToTagsAbove . addAutoName) inner
    where
        isClash = Clash.toIsClash aName & Clash.Collider
        tag = aName ^. Annotated.tag
        mUuid = aName ^. Annotated.internal . inContext
        addAutoName =
            case mUuid of
            Just uuid | isAutoGen -> p2AutoNames . Lens.at uuid ?~ tag
            _ -> id
        addToTagsAbove = case u of
            Walk.Unambiguous -> id
            Walk.MayBeAmbiguous ->
                p2TagsAbove . Clash.nameTypeSpace (aName ^. Annotated.nameType) . Lens.at tag %~
                Just . (<> isClash) . (^. Lens._Just)
