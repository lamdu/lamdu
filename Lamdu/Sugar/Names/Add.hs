{-# LANGUAGE LambdaCase, NoImplicitPrelude, GeneralizedNewtypeDeriving, RecordWildCards, TypeFamilies, TemplateHaskell, DeriveGeneric, KindSignatures, NoMonomorphismRestriction, OverloadedStrings #-}
module Lamdu.Sugar.Names.Add
    ( addToWorkArea
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Trans.Reader (Reader, runReader)
import qualified Control.Monad.Trans.Reader as Reader
import           Control.Monad.Trans.State (runState, evalState)
import           Control.Monad.Trans.FastWriter (Writer, runWriter)
import qualified Control.Monad.Trans.FastWriter as Writer
import qualified Data.List.Utils as ListUtils
import qualified Data.Map as Map
import qualified Data.Map.Utils as MapUtils
import           Data.Monoid.Generic (def_mempty, def_mappend)
import qualified Data.Set as Set
import           Data.Set.Ordered (OrderedSet)
import qualified Data.Set.Ordered as OrderedSet
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import qualified Data.Text as Text
import           Data.UUID.Types (UUID)
import           GHC.Generics (Generic)
import           Lamdu.Data.Anchors (assocNameRef)
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.Names.CPS (CPS(..))
import           Lamdu.Sugar.Names.NameGen (NameGen)
import qualified Lamdu.Sugar.Names.NameGen as NameGen
import           Lamdu.Sugar.Names.Types
import           Lamdu.Sugar.Names.Walk (MonadNaming)
import qualified Lamdu.Sugar.Names.Walk as Walk
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

type T = Transaction

type StoredName = Text

-- pass 0
data MStoredName = MStoredName
    { _mStoredName :: Maybe StoredName
    , _mStoredUUID :: UUID
    }

newtype Pass0LoadNames tm a = Pass0LoadNames { runPass0LoadNames :: T tm a }
    deriving (Functor, Applicative, Monad)

instance Monad tm => MonadNaming (Pass0LoadNames tm) where
    type OldName (Pass0LoadNames tm) = UUID
    type NewName (Pass0LoadNames tm) = MStoredName
    type TM (Pass0LoadNames tm) = tm
    opRun = pure runPass0LoadNames
    opWithParamName _ = p0cpsNameConvertor
    opWithLetName _ = p0cpsNameConvertor
    opWithTagName = p0cpsNameConvertor
    opGetName _ = p0nameConvertor

getMStoredName :: Monad tm => UUID -> Pass0LoadNames tm MStoredName
getMStoredName uuid =
    Pass0LoadNames $ do
        nameStr <- Transaction.getP $ assocNameRef uuid
        pure MStoredName
            { _mStoredName = if Text.null nameStr then Nothing else Just nameStr
            , _mStoredUUID = uuid
            }

p0nameConvertor :: Monad tm => Walk.NameConvertor (Pass0LoadNames tm)
p0nameConvertor = getMStoredName

p0cpsNameConvertor :: Monad tm => Walk.CPSNameConvertor (Pass0LoadNames tm)
p0cpsNameConvertor uuid =
    CPS $ \k -> (,) <$> getMStoredName uuid <*> k

type FunctionSignature = Apply () ()

-- | Info about a single instance of use of a name:
data NameInstance = NameInstance
    { _niUUID :: !UUID
    , -- | Is the name used in a function application context? We consider
      -- the application as a disambiguator
      _niMApplied :: !(Maybe FunctionSignature)
    , _niNameType :: !Walk.NameType
    } deriving (Eq, Ord, Show)
Lens.makeLenses ''NameInstance

-- Wrap the Map for a more sensible (recursive) Monoid instance
newtype NameUUIDMap = NameUUIDMap (Map StoredName (OrderedSet NameInstance))
    deriving Show

type instance Lens.Index NameUUIDMap = StoredName
type instance Lens.IxValue NameUUIDMap = OrderedSet NameInstance

instance Lens.Ixed NameUUIDMap where
    ix k f (NameUUIDMap m) = NameUUIDMap <$> Lens.ix k f m
    {-# INLINE ix #-}
instance Lens.At NameUUIDMap where
    at k f (NameUUIDMap m) = NameUUIDMap <$> Lens.at k f m
    {-# INLINE at #-}

instance Monoid NameUUIDMap where
    mempty = NameUUIDMap Map.empty
    NameUUIDMap x `mappend` NameUUIDMap y =
        NameUUIDMap $ Map.unionWith mappend x y

nameUUIDMapSingleton :: NameInstance -> StoredName -> NameUUIDMap
nameUUIDMapSingleton nameInstance name =
    OrderedSet.singleton nameInstance & Map.singleton name & NameUUIDMap

data StoredNamesWithin = StoredNamesWithin
    { _snwUUIDMap :: NameUUIDMap
    -- Names of tags and defs: considered conflicted if used in two
    -- different meanings anywhere in the whole definition:
    , _snwGlobalNames :: NameUUIDMap
    } deriving (Generic)
Lens.makeLenses ''StoredNamesWithin
instance Monoid StoredNamesWithin where
    mempty = def_mempty
    mappend = def_mappend

-- pass 1:
data StoredNames = StoredNames
    { _storedName :: MStoredName
    , storedNamesWithin :: StoredNamesWithin
    }
Lens.makeLenses ''StoredNames
newtype Pass1PropagateUp (tm :: * -> *) a = Pass1PropagateUp (Writer StoredNamesWithin a)
    deriving (Functor, Applicative, Monad)
p1TellStoredNames :: StoredNamesWithin -> Pass1PropagateUp tm ()
p1TellStoredNames = Pass1PropagateUp . Writer.tell
p1ListenStoredNames :: Pass1PropagateUp tm a -> Pass1PropagateUp tm (a, StoredNamesWithin)
p1ListenStoredNames (Pass1PropagateUp act) = Pass1PropagateUp $ Writer.listen act
runPass1PropagateUp :: Pass1PropagateUp tm a -> (a, StoredNamesWithin)
runPass1PropagateUp (Pass1PropagateUp act) = runWriter act

data NameScope = Local | Global

nameTypeScope :: Walk.NameType -> NameScope
nameTypeScope Walk.ParamName = Local
nameTypeScope Walk.TagName = Global
nameTypeScope Walk.NominalName = Global
nameTypeScope Walk.DefName = Global

instance Monad tm => MonadNaming (Pass1PropagateUp tm) where
    type OldName (Pass1PropagateUp tm) = MStoredName
    type NewName (Pass1PropagateUp tm) = StoredNames
    type TM (Pass1PropagateUp tm) = tm
    opRun = pure (return . fst . runPass1PropagateUp)
    opWithParamName _ = p1cpsNameConvertor Walk.ParamName
    opWithLetName _ = p1cpsNameConvertor Walk.ParamName
    opWithTagName = p1cpsNameConvertor Walk.TagName
    opGetName = p1nameConvertor Nothing
    opGetAppliedFuncName apply =
        p1nameConvertor (Just ctx)
        where
            -- Ignore operator precedence
            ctx = apply & aSpecialArgs . _InfixArgs . _1 .~ 0

pass1Result ::
    Maybe FunctionSignature -> Walk.NameType -> MStoredName ->
    Pass1PropagateUp tm (StoredNamesWithin -> StoredNames)
pass1Result mApplied nameType sn@(MStoredName mName uuid) =
    do
        p1TellStoredNames myStoredNamesWithin
        pure $ \storedNamesUnder -> StoredNames
            { _storedName = sn
            , storedNamesWithin = myStoredNamesWithin `mappend` storedNamesUnder
            }
    where
        myStoredNamesWithin =
            case mName of
            Nothing -> mempty
            Just name ->
                nameUUIDMapSingleton
                NameInstance
                { _niUUID = uuid
                , _niMApplied = mApplied
                , _niNameType = nameType
                } name
                & buildStoredNamesWithin
        buildStoredNamesWithin myNameUUIDMap =
            globalNames myNameUUIDMap
            & StoredNamesWithin myNameUUIDMap
        globalNames myNameUUIDMap =
            case nameTypeScope nameType of
            Local -> mempty
            Global -> myNameUUIDMap

p1nameConvertor :: Maybe FunctionSignature -> Walk.NameType -> Walk.NameConvertor (Pass1PropagateUp tm)
p1nameConvertor mApplied nameType mStoredName =
    pass1Result mApplied nameType mStoredName
    <&> ($ mempty)

p1cpsNameConvertor :: Walk.NameType -> Walk.CPSNameConvertor (Pass1PropagateUp tm)
p1cpsNameConvertor nameType mNameSrc =
    CPS $ \k -> do
        result <- pass1Result Nothing nameType mNameSrc
        (res, storedNamesBelow) <- p1ListenStoredNames k
        pure (result storedNamesBelow, res)

-- pass 2:
data P2Env = P2Env
    { _p2NameGen :: NameGen UUID
    , _p2StoredNameSuffixes :: Map UUID Int
    , _p2StoredNames :: Set Text
    }
Lens.makeLenses ''P2Env

-- | Textual Name ambiguity
--
-- In the visible grammar, there are different types of names (see
-- Walk.NameType):
-- DefName, TagName, NominalName, ParamName
--
-- Each type can collide with itself. Nominals can only collide with
-- themselves (due to their grammatic context being unique).
--
-- Definitions and tags cannot collide with each other but both
-- can collide with param names.
--
-- Hence, we check collisions in three groups:
-- * Nominals
-- * Tags+Params
-- * Defs+Params
--
-- Defs+Params can also be disambiguated if used exclusively in
-- labeled apply contexts, and with differing signatures.

uuidSuffixes :: OrderedSet NameInstance -> Map UUID Int
uuidSuffixes nameInstances
    | namesClash (getNames Walk.NominalName)
    || namesClash (getNames Walk.TagName ++ getNames Walk.ParamName)
    || namesClash (getNames Walk.DefName ++ getNames Walk.ParamName)
    = zip uuids [0..] & Map.fromList
    | otherwise = mempty
    where
        getNames nameType = byType ^. Lens.ix nameType
        byType =
            nameInstances ^.. Lens.folded & MapUtils.partition (^. niNameType)
        uuids = (nameInstances ^. Lens.folded . niUUID . Lens.to OrderedSet.singleton) ^.. Lens.folded

-- | Given a list of UUIDs that are being referred to via the same
-- textual name, generate a suffix map
namesClash :: [NameInstance] -> Bool
namesClash nameInstances =
    Set.size uuids >= 2
    &&
    ( ListUtils.isLengthAtLeast 2 nameRefs
      || hasBothTypes
      || hasSameCallType
    )
    where
        uuids = nameInstances ^.. Lens.folded . niUUID & Set.fromList
        nameRefs = nameInstances ^.. Lens.folded . niMApplied . Lens._Nothing
        nameApps = nameInstances ^.. Lens.folded . niMApplied . Lens._Just
        -- | Same textual name used both as an application and as a
        -- normal name:
        hasBothTypes = not (null nameRefs) && not (null nameApps)
        -- | Multiple applications have same signature for differing
        -- UUIDs, still ambiguous:
        hasSameCallType = uniqueCount nameApps < length nameApps
        uniqueCount = Set.size . Set.fromList

emptyP2Env :: NameUUIDMap -> P2Env
emptyP2Env (NameUUIDMap globalNamesMap) =
    P2Env
    { _p2NameGen = NameGen.initial
    , _p2StoredNames = mempty
    , _p2StoredNameSuffixes = globalNamesMap ^.. traverse <&> uuidSuffixes & mconcat
    }

newtype Pass2MakeNames (tm :: * -> *) a = Pass2MakeNames (Reader P2Env a)
    deriving (Functor, Applicative, Monad)
runPass2MakeNames :: P2Env -> Pass2MakeNames tm a -> a
runPass2MakeNames initial (Pass2MakeNames act) = runReader act initial
p2GetEnv :: Pass2MakeNames tm P2Env
p2GetEnv = Pass2MakeNames Reader.ask
p2WithEnv :: (P2Env -> P2Env) -> Pass2MakeNames tm a -> Pass2MakeNames tm a
p2WithEnv f (Pass2MakeNames act) = Pass2MakeNames $ Reader.local f act

runPass2MakeNamesInitial :: StoredNamesWithin -> Pass2MakeNames tm a -> a
runPass2MakeNamesInitial storedNamesBelow = runPass2MakeNames (emptyP2Env (storedNamesBelow ^. snwGlobalNames))

setName :: Monad tm => UUID -> StoredName -> T tm ()
setName = Transaction.setP . assocNameRef

instance Monad tm => MonadNaming (Pass2MakeNames tm) where
    type OldName (Pass2MakeNames tm) = StoredNames
    type NewName (Pass2MakeNames tm) = Name tm
    type TM (Pass2MakeNames tm) = tm
    opRun = p2GetEnv <&> runPass2MakeNames <&> (return .)
    opWithTagName = p2cpsNameConvertorGlobal "tag_"
    opWithParamName = p2cpsNameConvertorLocal
    opWithLetName = p2cpsNameConvertorLocal
    opGetName Walk.ParamName (StoredNames (MStoredName mName uuid) storedNamesUnder) =
        case mName of
            Just name ->
                makeFinalName name storedNamesUnder uuid <$> p2GetEnv
            Nothing ->
                do
                    nameGen <- (^. p2NameGen) <$> p2GetEnv
                    let name = evalState (NameGen.existingName uuid) nameGen
                    pure $
                        Name NameSourceAutoGenerated NoCollision (setName uuid) name
    opGetName Walk.TagName x = p2nameConvertor "tag_" x
    opGetName Walk.NominalName x = p2nameConvertor "nom_" x
    opGetName Walk.DefName x = p2nameConvertor "def_" x

makeFinalName ::
    Monad tm => StoredName -> StoredNamesWithin -> UUID -> P2Env -> Name tm
makeFinalName name storedNamesBelow uuid env =
    fst $ makeFinalNameEnv name storedNamesBelow uuid env

compose :: [a -> a] -> a -> a
compose = foldr (.) id

makeFinalNameEnv ::
    Monad tm =>
    StoredName ->
    StoredNamesWithin -> UUID -> P2Env -> (Name tm, P2Env)
makeFinalNameEnv name storedNamesBelow uuid env =
    (Name NameSourceStored collision (setName uuid) name, newEnv)
    where
        (collision, newEnv) =
            case (mSuffixFromAbove, collidingUUIDs) of
                (Just suffix, _) -> (Collision suffix, env)
                (Nothing, []) -> (NoCollision, envWithName [])
                (Nothing, otherUUIDs) -> (Collision 0, envWithName (uuid:otherUUIDs))
        envWithName uuids = env
            & p2StoredNames %~ Set.insert name
            -- This name is first occurence, so we get suffix 0
            & p2StoredNameSuffixes %~ compose ((Lens.itraversed %@~ flip Map.insert) uuids)
        mSuffixFromAbove =
            Map.lookup uuid $ env ^. p2StoredNameSuffixes
        collidingUUIDs =
            storedNamesBelow
            ^. snwUUIDMap . Lens.at name . Lens._Just .
                Lens.folded . niUUID . Lens.filtered (/= uuid) . Lens.to OrderedSet.singleton
            ^.. Lens.folded

p2cpsNameConvertor ::
    Monad tm =>
    StoredNames ->
    (P2Env -> (Name tm, P2Env)) ->
    CPS (Pass2MakeNames tm) (Name tm)
p2cpsNameConvertor (StoredNames mStoredName storedNamesBelow) nameMaker =
    CPS $ \k ->
    do
        oldEnv <- p2GetEnv
        let (newName, newEnv) =
                case mName of
                Just name -> makeFinalNameEnv name storedNamesBelow uuid oldEnv
                Nothing -> nameMaker oldEnv
        res <- p2WithEnv (const newEnv) k
        return (newName, res)
    where
        MStoredName mName uuid = mStoredName

makeUUIDName :: Monad tm => Text -> MStoredName -> Name tm
makeUUIDName prefix (MStoredName _ uuid) =
    Name NameSourceAutoGenerated NoCollision (setName uuid)
    (prefix <> Text.pack (take 8 (show uuid)))

p2cpsNameConvertorGlobal :: Monad tm => Text -> Walk.CPSNameConvertor (Pass2MakeNames tm)
p2cpsNameConvertorGlobal prefix storedNames =
    p2cpsNameConvertor storedNames $
    \p2env -> (makeUUIDName prefix (storedNames ^. storedName), p2env)

p2cpsNameConvertorLocal :: Monad tm => NameGen.VarInfo -> Walk.CPSNameConvertor (Pass2MakeNames tm)
p2cpsNameConvertorLocal isFunction storedNames =
    p2cpsNameConvertor storedNames $ \p2env ->
    (`runState` p2env) . Lens.zoom p2NameGen $
        let conflict name =
                Lens.has (snwUUIDMap . Lens.at name . Lens._Just) storedNamesBelow ||
                (p2env ^. p2StoredNames . Lens.contains name)
        in
            Name NameSourceAutoGenerated NoCollision (setName uuid) <$>
            NameGen.newName (not . conflict) isFunction uuid
    where
        StoredNames (MStoredName _ uuid) storedNamesBelow = storedNames

p2nameConvertor :: Monad tm => Text -> Walk.NameConvertor (Pass2MakeNames tm)
p2nameConvertor prefix (StoredNames mStoredName storedNamesBelow) =
    case mName of
    Just str -> makeFinalName str storedNamesBelow uuid <$> p2GetEnv
    Nothing -> pure $ makeUUIDName prefix mStoredName
    where
        MStoredName mName uuid = mStoredName

fixVarToTags :: Monad m => VarToTags -> T m ()
fixVarToTags VarToTags {..} =
    do
        Transaction.setP tagName =<< Transaction.getP varName
        Transaction.setP varName ""
    where
        varName = assocNameRef vttReplacedVar
        tagName = assocNameRef (vttReplacedByTag ^. tagVal)

fixParamAddResult :: Monad m => ParamAddResult -> T m ()
fixParamAddResult (ParamAddResultVarToTags v) = fixVarToTags v
fixParamAddResult _ = return ()

fixParamDelResult :: Monad m => ParamDelResult -> T m ()
fixParamDelResult (ParamDelResultTagsToVar TagsToVar {..}) =
    do
        Transaction.setP varName =<< Transaction.getP tagName
        Transaction.setP tagName ""
    where
        varName = assocNameRef ttvReplacedByVar
        tagName = assocNameRef (ttvReplacedTag ^. tagVal)
fixParamDelResult _ = return ()

fixLetFloatResult :: Monad m => LetFloatResult -> T m ()
fixLetFloatResult = maybe (return ()) fixVarToTags . lfrMVarToTags

-- mutual recursion fixBinder<->fixExpr

fixBinder ::
    Monad m =>
    Binder name m (Expression name m a) ->
    Binder name m (Expression name m a)
fixBinder binder =
    binder
    & SugarLens.binderFuncParamAdds %~ postProcess fixParamAddResult
    & SugarLens.binderFuncParamDeletes %~ postProcess fixParamDelResult
    & bBody %~ fixBinderBody
    & SugarLens.binderLetActions . laFloat %~ postProcess fixLetFloatResult
    where
        fixBinderBody bb =
            bb
            & bbContent . _BinderExpr %~ fixExpr
            & bbContent . _BinderLet . lValue %~ fixBinder
            & bbContent . _BinderLet . lBody %~ fixBinderBody
        postProcess f action =
            do
                res <- action
                () <- f res
                return res

fixExpr :: Monad m => Expression name m a -> Expression name m a
fixExpr expr =
    expr & rBody %~ \case
    BodyLam lam -> lam & lamBinder %~ fixBinder & BodyLam
    body -> body <&> fixExpr

runPasses ::
    Functor tm =>
    (a -> Pass0LoadNames tm b) -> (b -> Pass1PropagateUp tm c) -> (c -> Pass2MakeNames tm d) ->
    a -> T tm d
runPasses f0 f1 f2 =
    fmap (pass2 . pass1) . pass0
    where
        pass0 = runPass0LoadNames . f0
        pass1 = runPass1PropagateUp . f1
        pass2 (x, storedNamesBelow) =
            f2 x & runPass2MakeNamesInitial storedNamesBelow

fixDef ::
    Monad tm =>
    Definition name tm (Expression name tm a) ->
    Definition name tm (Expression name tm a)
fixDef = drBody . _DefinitionBodyExpression . deContent %~ fixBinder

addToWorkArea :: Monad tm => WorkArea UUID tm a -> T tm (WorkArea (Name tm) tm a)
addToWorkArea workArea =
    workArea
    & waPanes . traverse . paneDefinition %~ fixDef
    & waRepl %~ fixExpr
    & runPasses f f f
    where
        f = Walk.toWorkArea
