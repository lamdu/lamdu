{-# LANGUAGE LambdaCase, NoImplicitPrelude, GeneralizedNewtypeDeriving, RecordWildCards, TypeFamilies, TemplateHaskell, DeriveGeneric, FlexibleInstances, KindSignatures, NoMonomorphismRestriction, OverloadedStrings #-}
module Lamdu.Sugar.Names.Add
    ( addToDef, addToExpr
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Trans.Reader (Reader, runReader)
import qualified Control.Monad.Trans.Reader as Reader
import           Control.Monad.Trans.State (runState, evalState)
import           Control.Monad.Trans.Writer (Writer, runWriter)
import qualified Control.Monad.Trans.Writer as Writer
import           Data.Foldable (toList)
import qualified Data.List.Utils as ListUtils
import qualified Data.Map as Map
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

newtype Pass0M tm a = Pass0M { runPass0M :: T tm a }
    deriving (Functor, Applicative, Monad)

instance Monad tm => MonadNaming (Pass0M tm) where
    type OldName (Pass0M tm) = UUID
    type NewName (Pass0M tm) = MStoredName
    type TM (Pass0M tm) = tm
    opRun = pure $ Walk.InTransaction runPass0M
    opWithParamName _ = p0cpsNameConvertor
    opWithLetName _ = p0cpsNameConvertor
    opWithDefName = p0cpsNameConvertor
    opWithTagName = p0cpsNameConvertor
    opGetName _ = p0nameConvertor

getMStoredName :: Monad tm => UUID -> Pass0M tm MStoredName
getMStoredName uuid =
    Pass0M $ do
        nameStr <- Transaction.getP $ assocNameRef uuid
        pure MStoredName
            { _mStoredName = if Text.null nameStr then Nothing else Just nameStr
            , _mStoredUUID = uuid
            }

p0nameConvertor :: Monad tm => Walk.NameConvertor (Pass0M tm)
p0nameConvertor = getMStoredName

p0cpsNameConvertor :: Monad tm => Walk.CPSNameConvertor (Pass0M tm)
p0cpsNameConvertor uuid =
    CPS $ \k -> (,) <$> getMStoredName uuid <*> k

-- Wrap the Map for a more sensible (recursive) Monoid instance
newtype NameUUIDMap = NameUUIDMap (Map StoredName (OrderedSet UUID))
    deriving Show

type instance Lens.Index NameUUIDMap = StoredName
type instance Lens.IxValue NameUUIDMap = OrderedSet UUID

-- ghc-7.7.20131205 fails deriving these instances on its own.
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

nameUUIDMapSingleton :: StoredName -> UUID -> NameUUIDMap
nameUUIDMapSingleton name uuid = NameUUIDMap . Map.singleton name $ OrderedSet.singleton uuid

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
newtype Pass1M (tm :: * -> *) a = Pass1M (Writer StoredNamesWithin a)
    deriving (Functor, Applicative, Monad)
p1TellStoredNames :: StoredNamesWithin -> Pass1M tm ()
p1TellStoredNames = Pass1M . Writer.tell
p1ListenStoredNames :: Pass1M tm a -> Pass1M tm (a, StoredNamesWithin)
p1ListenStoredNames (Pass1M act) = Pass1M $ Writer.listen act
runPass1M :: Pass1M tm a -> (a, StoredNamesWithin)
runPass1M (Pass1M act) = runWriter act

data NameScope = Local | Global

instance Monad tm => MonadNaming (Pass1M tm) where
    type OldName (Pass1M tm) = MStoredName
    type NewName (Pass1M tm) = StoredNames
    type TM (Pass1M tm) = tm
    opRun = pure $ Walk.InTransaction $ return . fst . runPass1M
    opWithParamName _ = p1cpsNameConvertor Local
    opWithLetName _ = p1cpsNameConvertor Local
    opWithDefName = p1cpsNameConvertor Local
    opWithTagName = p1cpsNameConvertor Local
    opGetName nameType =
        case nameType of
        Walk.ParamName -> Local
        Walk.TagName -> Global
        Walk.NominalName -> Global
        Walk.DefName -> Global
        & p1nameConvertor

pass1Result ::
    NameScope -> MStoredName ->
    Pass1M tm (StoredNamesWithin -> StoredNames)
pass1Result scope sn@(MStoredName mName uuid) =
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
              (`nameUUIDMapSingleton` uuid)) mName
        buildStoredNamesWithin myNameUUIDMap =
            StoredNamesWithin myNameUUIDMap $
            globalNames myNameUUIDMap
        globalNames myNameUUIDMap =
            case scope of
            Local -> mempty
            Global -> myNameUUIDMap

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
    { _p2NameGen :: NameGen UUID
    , _p2StoredNameSuffixes :: Map UUID Int
    , _p2StoredNames :: Set Text
    }
Lens.makeLenses ''P2Env

emptyP2Env :: NameUUIDMap -> P2Env
emptyP2Env (NameUUIDMap globalNamesMap) = P2Env
    { _p2NameGen = NameGen.initial
    , _p2StoredNames = mempty
    , _p2StoredNameSuffixes =
        mconcat .
        map Map.fromList . filter (ListUtils.isLengthAtLeast 2) .
        map ((`zip` [0..]) . toList) $ Map.elems globalNamesMap
    }

newtype Pass2M (tm :: * -> *) a = Pass2M (Reader P2Env a)
    deriving (Functor, Applicative, Monad)
runPass2M :: P2Env -> Pass2M tm a -> a
runPass2M initial (Pass2M act) = runReader act initial
p2GetEnv :: Pass2M tm P2Env
p2GetEnv = Pass2M Reader.ask
p2WithEnv :: (P2Env -> P2Env) -> Pass2M tm a -> Pass2M tm a
p2WithEnv f (Pass2M act) = Pass2M $ Reader.local f act

runPass2MInitial :: StoredNamesWithin -> Pass2M tm a -> a
runPass2MInitial storedNamesBelow = runPass2M (emptyP2Env (storedNamesBelow ^. snwGlobalNames))

setName :: Monad tm => UUID -> StoredName -> T tm ()
setName = Transaction.setP . assocNameRef

instance Monad tm => MonadNaming (Pass2M tm) where
    type OldName (Pass2M tm) = StoredNames
    type NewName (Pass2M tm) = Name tm
    type TM (Pass2M tm) = tm
    opRun = (\env -> Walk.InTransaction (return . runPass2M env)) <$> p2GetEnv
    opWithDefName = p2cpsNameConvertorGlobal "def_"
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
            maybe [] (filter (/= uuid) . toList) $
            storedNamesBelow ^. snwUUIDMap . Lens.at name

p2cpsNameConvertor ::
    Monad tm =>
    StoredNames ->
    (P2Env -> (Name tm, P2Env)) ->
    CPS (Pass2M tm) (Name tm)
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

p2cpsNameConvertorGlobal :: Monad tm => Text -> Walk.CPSNameConvertor (Pass2M tm)
p2cpsNameConvertorGlobal prefix storedNames =
    p2cpsNameConvertor storedNames $
    \p2env -> (makeUUIDName prefix (storedNames ^. storedName), p2env)

p2cpsNameConvertorLocal :: Monad tm => NameGen.VarInfo -> Walk.CPSNameConvertor (Pass2M tm)
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

p2nameConvertor :: Monad tm => Text -> Walk.NameConvertor (Pass2M tm)
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
    (a -> Pass0M tm b) -> (b -> Pass1M tm c) -> (c -> Pass2M tm d) ->
    a -> Transaction tm d
runPasses f0 f1 f2 =
    fmap (pass2 . pass1) . pass0
    where
        pass0 = runPass0M . f0
        pass1 = runPass1M . f1
        pass2 (x, storedNamesBelow) =
            f2 x & runPass2MInitial storedNamesBelow

addToDef :: Monad tm => DefinitionU tm a -> T tm (DefinitionN tm a)
addToDef def =
    def
    & drBody . _DefinitionBodyExpression . deContent %~ fixBinder
    & runPasses f f f
    where
        f = Walk.toDef Walk.toExpression

addToExpr :: Monad tm => Expression UUID tm a -> T tm (Expression (Name tm) tm a)
addToExpr expr =
    expr
    & fixExpr
    & runPasses f f f
    where
        f = Walk.toExpression
