{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, TypeFamilies, TemplateHaskell, DeriveGeneric, FlexibleInstances, KindSignatures #-}
module Lamdu.Sugar.Names
    ( addToDef
    ) where

import           Control.Applicative (Applicative(..), (<$>))
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Monad.Trans.Reader (Reader, runReader)
import qualified Control.Monad.Trans.Reader as Reader
import           Control.Monad.Trans.State (runState, evalState)
import           Control.Monad.Trans.Writer (Writer, runWriter)
import qualified Control.Monad.Trans.Writer as Writer
import           Control.MonadA (MonadA)
import           Data.Foldable (toList)
import qualified Data.List.Utils as ListUtils
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Monoid (Monoid(..))
import           Data.Monoid.Generic (def_mempty, def_mappend)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Set.Ordered (OrderedSet)
import qualified Data.Set.Ordered as OrderedSet
import           Data.Store.Guid (Guid)
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
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

type T = Transaction

type StoredName = String

-- pass 0
data MStoredName = MStoredName
    { _mStoredName :: Maybe StoredName
    , _mStoredGuid :: Guid
    }

newtype Pass0M tm a = Pass0M { runPass0M :: T tm a }
    deriving (Functor, Applicative, Monad)

instance MonadA tm => MonadNaming (Pass0M tm) where
    type OldName (Pass0M tm) = Guid
    type NewName (Pass0M tm) = MStoredName
    type TM (Pass0M tm) = tm
    opRun = pure $ Walk.InTransaction runPass0M
    opWithParamName _ = p0cpsNameConvertor
    opWithWhereItemName _ = p0cpsNameConvertor
    opWithDefName = p0cpsNameConvertor
    opWithTagName = p0cpsNameConvertor
    opGetParamName = p0nameConvertor
    opGetHiddenParamsName = p0nameConvertor
    opGetTagName = p0nameConvertor
    opGetTIdName = p0nameConvertor
    opGetDefName = p0nameConvertor

getMStoredName :: MonadA tm => Guid -> Pass0M tm MStoredName
getMStoredName guid =
    Pass0M $ do
        nameStr <- Transaction.getP $ assocNameRef guid
        pure MStoredName
            { _mStoredName = if null nameStr then Nothing else Just nameStr
            , _mStoredGuid = guid
            }

p0nameConvertor :: MonadA tm => Walk.NameConvertor (Pass0M tm)
p0nameConvertor = getMStoredName

p0cpsNameConvertor :: MonadA tm => Walk.CPSNameConvertor (Pass0M tm)
p0cpsNameConvertor guid =
    CPS $ \k -> (,) <$> getMStoredName guid <*> k

-- Wrap the Map for a more sensible (recursive) Monoid instance
newtype NameGuidMap = NameGuidMap (Map StoredName (OrderedSet Guid))
    deriving Show

type instance Lens.Index NameGuidMap = StoredName
type instance Lens.IxValue NameGuidMap = OrderedSet Guid

-- ghc-7.7.20131205 fails deriving these instances on its own.
instance Lens.Ixed NameGuidMap where
    ix k f (NameGuidMap m) = NameGuidMap <$> Lens.ix k f m
    {-# INLINE ix #-}
instance Lens.At NameGuidMap where
    at k f (NameGuidMap m) = NameGuidMap <$> Lens.at k f m
    {-# INLINE at #-}

instance Monoid NameGuidMap where
    mempty = NameGuidMap Map.empty
    NameGuidMap x `mappend` NameGuidMap y =
        NameGuidMap $ Map.unionWith mappend x y

nameGuidMapSingleton :: StoredName -> Guid -> NameGuidMap
nameGuidMapSingleton name guid = NameGuidMap . Map.singleton name $ OrderedSet.singleton guid

data StoredNamesWithin = StoredNamesWithin
    { _snwGuidMap :: NameGuidMap
    -- Names of tags and defs: considered conflicted if used in two
    -- different meanings anywhere in the whole definition:
    , _snwGlobalNames :: NameGuidMap
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
newtype Pass1M (tm :: * -> *) a = Pass1M (Writer StoredNamesWithin a)
    deriving (Functor, Applicative, Monad)
p1TellStoredNames :: StoredNamesWithin -> Pass1M tm ()
p1TellStoredNames = Pass1M . Writer.tell
p1ListenStoredNames :: Pass1M tm a -> Pass1M tm (a, StoredNamesWithin)
p1ListenStoredNames (Pass1M act) = Pass1M $ Writer.listen act
runPass1M :: Pass1M tm a -> (a, StoredNamesWithin)
runPass1M (Pass1M act) = runWriter act

data NameScope = Local | Global

instance MonadA tm => MonadNaming (Pass1M tm) where
    type OldName (Pass1M tm) = MStoredName
    type NewName (Pass1M tm) = StoredNames
    type TM (Pass1M tm) = tm
    opRun = pure $ Walk.InTransaction $ return . fst . runPass1M
    opWithParamName _ = p1cpsNameConvertor Local
    opWithWhereItemName _ = p1cpsNameConvertor Local
    opWithDefName = p1cpsNameConvertor Local
    opWithTagName = p1cpsNameConvertor Local
    opGetParamName = p1nameConvertor Local
    opGetHiddenParamsName = p1nameConvertor Local
    opGetTagName = p1nameConvertor Global
    opGetTIdName = p1nameConvertor Global
    opGetDefName = p1nameConvertor Global

pass1Result ::
    NameScope -> MStoredName ->
    Pass1M tm (StoredNamesWithin -> StoredNames)
pass1Result scope sn@(MStoredName mName guid) =
    do
        p1TellStoredNames myStoredNamesWithin
        pure $ \storedNamesUnder -> StoredNames
            { _storedName = sn
            , storedNamesWithin = myStoredNamesWithin `mappend` storedNamesUnder
            }
    where
        myStoredNamesWithin =
            maybe mempty
            (buildStoredNamesWithin .
              (`nameGuidMapSingleton` guid)) mName
        buildStoredNamesWithin myNameGuidMap =
            StoredNamesWithin myNameGuidMap $
            globalNames myNameGuidMap
        globalNames myNameGuidMap =
            case scope of
            Local -> mempty
            Global -> myNameGuidMap

p1nameConvertor :: NameScope -> Walk.NameConvertor (Pass1M tm)
p1nameConvertor scope mStoredName = ($ mempty) <$> pass1Result scope mStoredName

p1cpsNameConvertor :: NameScope -> Walk.CPSNameConvertor (Pass1M tm)
p1cpsNameConvertor scope mNameSrc =
    CPS $ \k -> do
        result <- pass1Result scope mNameSrc
        (res, storedNamesBelow) <- p1ListenStoredNames k
        pure (result storedNamesBelow, res)

-- pass 2:
data P2Env = P2Env
    { _p2NameGen :: NameGen Guid
    , _p2StoredNameSuffixes :: Map Guid Int
    , _p2StoredNames :: Set String
    }
Lens.makeLenses ''P2Env

newtype Pass2M (tm :: * -> *) a = Pass2M (Reader P2Env a)
    deriving (Functor, Applicative, Monad)
runPass2M :: P2Env -> Pass2M tm a -> a
runPass2M initial (Pass2M act) = runReader act initial
p2GetEnv :: Pass2M tm P2Env
p2GetEnv = Pass2M Reader.ask
p2WithEnv :: (P2Env -> P2Env) -> Pass2M tm a -> Pass2M tm a
p2WithEnv f (Pass2M act) = Pass2M $ Reader.local f act

setName :: MonadA tm => Guid -> StoredName -> T tm ()
setName = Transaction.setP . assocNameRef

instance MonadA tm => MonadNaming (Pass2M tm) where
    type OldName (Pass2M tm) = StoredNames
    type NewName (Pass2M tm) = Name tm
    type TM (Pass2M tm) = tm
    opRun = (\env -> Walk.InTransaction (return . runPass2M env)) <$> p2GetEnv
    opWithDefName = p2cpsNameConvertorGlobal "def_"
    opWithTagName = p2cpsNameConvertorGlobal "tag_"
    opWithParamName = p2cpsNameConvertorLocal
    opWithWhereItemName = p2cpsNameConvertorLocal
    opGetParamName (StoredNames (MStoredName mName guid) storedNamesUnder) =
        case mName of
            Just name ->
                makeFinalName name storedNamesUnder guid <$> p2GetEnv
            Nothing ->
                do
                    nameGen <- (^. p2NameGen) <$> p2GetEnv
                    let name = evalState (NameGen.existingName guid) nameGen
                    pure $
                        Name NameSourceAutoGenerated NoCollision (setName guid) name
    opGetHiddenParamsName (StoredNames (MStoredName mName guid) _) =
        pure $ maybe
        (Name NameSourceAutoGenerated NoCollision (setName guid) "params")
        (Name NameSourceStored NoCollision (setName guid))
        mName
    opGetTagName = p2nameConvertor "tag_"
    opGetTIdName = p2nameConvertor "nom_"
    opGetDefName = p2nameConvertor "def_"

makeFinalName ::
    MonadA tm => StoredName -> StoredNamesWithin -> Guid -> P2Env -> Name tm
makeFinalName name storedNamesBelow guid env =
    fst $ makeFinalNameEnv name storedNamesBelow guid env

compose :: [a -> a] -> a -> a
compose = foldr (.) id

makeFinalNameEnv ::
    MonadA tm =>
    StoredName ->
    StoredNamesWithin -> Guid -> P2Env -> (Name tm, P2Env)
makeFinalNameEnv name storedNamesBelow guid env =
    (Name NameSourceStored collision (setName guid) name, newEnv)
    where
        (collision, newEnv) =
            case (mSuffixFromAbove, collidingGuids) of
                (Just suffix, _) -> (Collision suffix, env)
                (Nothing, []) -> (NoCollision, envWithName [])
                (Nothing, otherGuids) -> (Collision 0, envWithName (guid:otherGuids))
        envWithName guids = env
            & p2StoredNames %~ Set.insert name
            -- This name is first occurence, so we get suffix 0
            & p2StoredNameSuffixes %~ compose ((Lens.itraversed %@~ flip Map.insert) guids)
        mSuffixFromAbove =
            Map.lookup guid $ env ^. p2StoredNameSuffixes
        collidingGuids =
            maybe [] (filter (/= guid) . toList) $
            storedNamesBelow ^. snwGuidMap . Lens.at name

p2cpsNameConvertor ::
    MonadA tm =>
    StoredNames ->
    (P2Env -> (Name tm, P2Env)) ->
    CPS (Pass2M tm) (Name tm)
p2cpsNameConvertor (StoredNames mStoredName storedNamesBelow) nameMaker =
    CPS $ \k ->
    do
        oldEnv <- p2GetEnv
        let (newName, newEnv) =
                case mName of
                Just name -> makeFinalNameEnv name storedNamesBelow guid oldEnv
                Nothing -> nameMaker oldEnv
        res <- p2WithEnv (const newEnv) k
        return (newName, res)
    where
        MStoredName mName guid = mStoredName

makeGuidName :: MonadA tm => String -> MStoredName -> Name tm
makeGuidName prefix (MStoredName _ guid) =
    Name NameSourceAutoGenerated NoCollision (setName guid) (prefix ++ show guid)

p2cpsNameConvertorGlobal :: MonadA tm => String -> Walk.CPSNameConvertor (Pass2M tm)
p2cpsNameConvertorGlobal prefix storedNames =
    p2cpsNameConvertor storedNames $
    \p2env -> (makeGuidName prefix (storedNames ^. storedName), p2env)

p2cpsNameConvertorLocal :: MonadA tm => NameGen.IsFunction -> Walk.CPSNameConvertor (Pass2M tm)
p2cpsNameConvertorLocal isFunction storedNames =
    p2cpsNameConvertor storedNames $ \p2env ->
    (`runState` p2env) . Lens.zoom p2NameGen $
        let conflict name =
                Lens.has (snwGuidMap . Lens.at name . Lens._Just) storedNamesBelow ||
                (p2env ^. p2StoredNames . Lens.contains name)
        in
            Name NameSourceAutoGenerated NoCollision (setName guid) <$>
            NameGen.newName (not . conflict) isFunction guid
    where
        StoredNames (MStoredName _ guid) storedNamesBelow = storedNames

p2nameConvertor :: MonadA tm => String -> Walk.NameConvertor (Pass2M tm)
p2nameConvertor prefix (StoredNames mStoredName storedNamesBelow) =
    case mName of
    Just str -> makeFinalName str storedNamesBelow guid <$> p2GetEnv
    Nothing -> pure $ makeGuidName prefix mStoredName
    where
        MStoredName mName guid = mStoredName

fixParamAddResult :: MonadA m => ParamAddResult -> T m ()
fixParamAddResult (ParamAddResultVarToTags VarToTags {..}) =
    do
        Transaction.setP tagName =<< Transaction.getP varName
        Transaction.setP varName ""
    where
        varName = assocNameRef vttReplacedVar
        tagName = assocNameRef (vttReplacedByTag ^. tagVal)
fixParamAddResult _ = return ()

fixParamDelResult :: MonadA m => ParamDelResult -> T m ()
fixParamDelResult (ParamDelResultTagsToVar TagsToVar {..}) =
    do
        Transaction.setP varName =<< Transaction.getP tagName
        Transaction.setP tagName ""
    where
        varName = assocNameRef ttvReplacedByVar
        tagName = assocNameRef (ttvReplacedTag ^. tagVal)
fixParamDelResult _ = return ()

fixBinder ::
    MonadA m =>
    Binder name m (Expression name m a) ->
    Binder name m (Expression name m a)
fixBinder binder =
    binder
    & SugarLens.binderFuncParamAdds %~ postProcess fixParamAddResult
    & SugarLens.binderFuncParamDeletes %~ postProcess fixParamDelResult
    where
        postProcess f action =
            do
                res <- action
                () <- f res
                return res

addToDef :: MonadA tm => DefinitionU tm a -> T tm (DefinitionN tm a)
addToDef origDef =
    origDef
    & drBody . _DefinitionBodyExpression . deContent %~ fixBinder
    & pass0
    <&> pass1
    <&> pass2
    where
        emptyP2Env (NameGuidMap globalNamesMap) = P2Env
            { _p2NameGen = NameGen.initial
            , _p2StoredNames = mempty
            , _p2StoredNameSuffixes =
                mconcat .
                map Map.fromList . filter (ListUtils.isLengthAtLeast 2) .
                map ((`zip` [0..]) . toList) $ Map.elems globalNamesMap
            }
        pass0 = runPass0M . Walk.toDef
        pass1 = runPass1M . Walk.toDef
        pass2 (def, storedNamesBelow) =
            runPass2M (emptyP2Env (storedNamesBelow ^. snwGlobalNames)) $ Walk.toDef def
