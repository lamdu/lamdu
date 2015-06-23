{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, TypeFamilies, TemplateHaskell, RankNTypes, DeriveGeneric, FlexibleInstances, KindSignatures, FlexibleContexts #-}
module Lamdu.Sugar.AddNames
    ( addToDef
    ) where

import           Control.Applicative (Applicative(..), (<$>))
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad ((<=<))
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
import           Data.Traversable (Traversable, traverse)
import           GHC.Generics (Generic)
import           Lamdu.Data.Anchors (assocNameRef)
import           Lamdu.Expr.Type (Type)
import qualified Lamdu.Expr.Type as T
import           Lamdu.Sugar.AddNames.CPS (CPS(..))
import           Lamdu.Sugar.AddNames.NameGen (NameGen)
import qualified Lamdu.Sugar.AddNames.NameGen as NameGen
import           Lamdu.Sugar.AddNames.Types
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.Types

type T = Transaction

type CPSNameConvertor m = OldName m -> CPS m (NewName m)
type NameConvertor m = OldName m -> m (NewName m)

newtype InTransaction m tm = InTransaction (forall a. m a -> T tm a)

class (MonadA m, MonadA (TM m)) => MonadNaming m where
    type OldName m
    type NewName m
    type TM m :: * -> *
    opRun :: m (InTransaction m (TM m))

    opWithParamName :: NameGen.IsFunction -> CPSNameConvertor m
    opWithWhereItemName :: NameGen.IsFunction -> CPSNameConvertor m
    opWithDefName :: CPSNameConvertor m
    opWithTagName :: CPSNameConvertor m
    opGetDefName :: NameConvertor m
    opGetTagName :: NameConvertor m
    opGetParamName :: NameConvertor m
    opGetHiddenParamsName :: NameConvertor m

type OldExpression m a = Expression (OldName m) (TM m) a
type NewExpression m a = Expression (NewName m) (TM m) a

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
    opRun = pure $ InTransaction runPass0M
    opWithParamName _ = p0cpsNameConvertor
    opWithWhereItemName _ = p0cpsNameConvertor
    opWithDefName = p0cpsNameConvertor
    opWithTagName = p0cpsNameConvertor
    opGetParamName = p0nameConvertor
    opGetHiddenParamsName = p0nameConvertor
    opGetTagName = p0nameConvertor
    opGetDefName = p0nameConvertor

getMStoredName :: MonadA tm => Guid -> Pass0M tm MStoredName
getMStoredName guid =
    Pass0M $ do
        nameStr <- Transaction.getP $ assocNameRef guid
        pure MStoredName
            { _mStoredName = if null nameStr then Nothing else Just nameStr
            , _mStoredGuid = guid
            }

p0nameConvertor :: MonadA tm => NameConvertor (Pass0M tm)
p0nameConvertor = getMStoredName

p0cpsNameConvertor :: MonadA tm => CPSNameConvertor (Pass0M tm)
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
    opRun = pure $ InTransaction $ return . fst . runPass1M
    opWithParamName _ = p1cpsNameConvertor Local
    opWithWhereItemName _ = p1cpsNameConvertor Local
    opWithDefName = p1cpsNameConvertor Local
    opWithTagName = p1cpsNameConvertor Local
    opGetParamName = p1nameConvertor Local
    opGetHiddenParamsName = p1nameConvertor Local
    opGetTagName = p1nameConvertor Global
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

p1nameConvertor :: NameScope -> NameConvertor (Pass1M tm)
p1nameConvertor scope mStoredName = ($ mempty) <$> pass1Result scope mStoredName

p1cpsNameConvertor :: NameScope -> CPSNameConvertor (Pass1M tm)
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
    opRun = (\env -> InTransaction (return . runPass2M env)) <$> p2GetEnv
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

p2cpsNameConvertorGlobal :: MonadA tm => String -> CPSNameConvertor (Pass2M tm)
p2cpsNameConvertorGlobal prefix storedNames =
    p2cpsNameConvertor storedNames $
    \p2env -> (makeGuidName prefix (storedNames ^. storedName), p2env)

p2cpsNameConvertorLocal :: MonadA tm => NameGen.IsFunction -> CPSNameConvertor (Pass2M tm)
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

p2nameConvertor :: MonadA tm => String -> NameConvertor (Pass2M tm)
p2nameConvertor prefix (StoredNames mStoredName storedNamesBelow) =
    case mName of
    Just str -> makeFinalName str storedNamesBelow guid <$> p2GetEnv
    Nothing -> pure $ makeGuidName prefix mStoredName
    where
        MStoredName mName guid = mStoredName

isFunctionType :: Type -> NameGen.IsFunction
isFunctionType T.TFun {} = NameGen.Function
isFunctionType _ = NameGen.NotFunction

toTagG :: MonadNaming m => TagG (OldName m) -> m (TagG (NewName m))
toTagG = tagGName opGetTagName

toRecordField ::
    MonadNaming m =>
    RecordField (OldName m) (TM m) (OldExpression m a) ->
    m (RecordField (NewName m) (TM m) (NewExpression m a))
toRecordField recordField@RecordField {..} =
    do
        tag <- toTagG _rfTag
        expr <- toExpression _rfExpr
        pure recordField
            { _rfTag = tag
            , _rfExpr = expr
            }

toRecord ::
    MonadNaming m =>
    Record (OldName m) (TM m) (OldExpression m a) ->
    m (Record (NewName m) (TM m) (NewExpression m a))
toRecord record@Record {..} =
    do
        items <- traverse toRecordField _rItems
        t <- traverse toExpression _rTail
        pure record { _rItems = items, _rTail = t }

toCaseAlt ::
    MonadNaming m =>
    CaseAlt (OldName m) (TM m) (OldExpression m a) ->
    m (CaseAlt (NewName m) (TM m) (NewExpression m a))
toCaseAlt alt@CaseAlt {..} =
    do
        tag <- toTagG _caTag
        handler <- toExpression _caHandler
        pure alt
            { _caTag = tag
            , _caHandler = handler
            }

toCase ::
    MonadNaming m =>
    Case (OldName m) (TM m) (OldExpression m a) ->
    m (Case (NewName m) (TM m) (NewExpression m a))
toCase case_@Case {..} =
    do
        kind <- traverse toExpression _cKind
        alts <- traverse toCaseAlt _cAlts
        t <- traverse toExpression _cTail
        pure case_ { _cKind = kind, _cAlts = alts, _cTail = t }

toGetField ::
    MonadNaming m =>
    GetField (OldName m) (TM m) (OldExpression m a) ->
    m (GetField (NewName m) (TM m) (NewExpression m a))
toGetField getField@GetField {..} =
    do
        record <- toExpression _gfRecord
        tag <- toTagG _gfTag
        pure getField { _gfRecord = record, _gfTag = tag }

toInject ::
    MonadNaming m =>
    Inject (OldName m) (TM m) (OldExpression m a) ->
    m (Inject (NewName m) (TM m) (NewExpression m a))
toInject inject@Inject {..} =
    do
        val <- toExpression _iVal
        tag <- toTagG _iTag
        pure inject { _iVal = val, _iTag = tag }

toScopeGetVar ::
    MonadNaming m =>
    ScopeGetVar (OldName m) (TM m) ->
    m (ScopeGetVar (NewName m) (TM m))
toScopeGetVar (ScopeGetVar gv val) = (`ScopeGetVar` val) <$> toGetVar gv

toHoleActions ::
    MonadNaming m =>
    HoleActions (OldName m) (TM m) ->
    m (HoleActions (NewName m) (TM m))
toHoleActions ha@HoleActions {..} =
    do
        InTransaction run <- opRun
        pure ha
            { _holeScope =
                run . traverse toScopeGetVar =<< _holeScope
            , _holeResults =
                _holeResults
                & Lens.mapped . Lens.mapped . _2 %~
                    (>>= holeResultConverted (run . toExpression))
            }

toHoleArg ::
    MonadNaming m =>
    HoleArg (OldName m) (TM m) (OldExpression m a) ->
    m (HoleArg (NewName m) (TM m) (NewExpression m a))
toHoleArg arg@HoleArg{..} =
    do
        expr <- toExpression _haExpr
        tags <- mapM toTagG _haGetFieldTags
        pure arg
            { _haExpr = expr
            , _haGetFieldTags = tags
            }

toHole ::
    MonadNaming m =>
    Hole (OldName m) (TM m) (OldExpression m a) ->
    m (Hole (NewName m) (TM m) (NewExpression m a))
toHole hole@Hole {..} =
    do
        mActions <- _holeMActions & Lens._Just %%~ toHoleActions
        mArg <- _holeMArg & Lens._Just %%~ toHoleArg
        suggestedInjectTags <- traverse toTagG _holeSuggestedInjectTags
        pure hole
            { _holeMActions = mActions
            , _holeMArg = mArg
            , _holeSuggestedInjectTags = suggestedInjectTags
            }

toNamedVar ::
    MonadNaming m =>
    NamedVar (OldName m) (TM m) ->
    m (NamedVar (NewName m) (TM m))
toNamedVar namedVar =
    nvName f namedVar
    where
        f =
            case namedVar ^. nvVarType of
            GetParameter      -> opGetParamName
            GetFieldParameter -> opGetTagName
            GetDefinition     -> opGetDefName

toParamsRecordVar ::
    MonadNaming m => ParamsRecordVar (OldName m) ->
    m (ParamsRecordVar (NewName m))
toParamsRecordVar (ParamsRecordVar names) =
    ParamsRecordVar <$> traverse opGetTagName names

toGetVar ::
    MonadNaming m =>
    GetVar (OldName m) (TM m) ->
    m (GetVar (NewName m) (TM m))
toGetVar (GetVarNamed x) =
    GetVarNamed <$> toNamedVar x
toGetVar (GetVarParamsRecord x) =
    GetVarParamsRecord <$> toParamsRecordVar x

toApply ::
    MonadNaming m =>
    Apply (OldName m) (OldExpression m a) ->
    m (Apply (NewName m) (NewExpression m a))
toApply la@Apply{..} =
    do
        func <- toExpression _aFunc
        specialArgs <- traverse toExpression _aSpecialArgs
        annotatedArgs <- traverse (aaTag toTagG <=< aaExpr toExpression) _aAnnotatedArgs
        pure la
            { _aFunc = func
            , _aSpecialArgs = specialArgs
            , _aAnnotatedArgs = annotatedArgs
            }

traverseToExpr ::
    (MonadNaming m, Traversable t) =>
    (t (NewExpression m a) -> b) -> t (OldExpression m a) ->
    m b
traverseToExpr cons body = cons <$> traverse toExpression body

toBody ::
    MonadNaming m =>
    Body (OldName m) (TM m) (OldExpression m a) ->
    m (Body (NewName m) (TM m) (NewExpression m a))
toBody (BodyList x)           = traverseToExpr BodyList x
toBody (BodyLiteralInteger x) = pure $ BodyLiteralInteger x
--
toBody (BodyGetField x) = BodyGetField <$> toGetField x
toBody (BodyInject x) = BodyInject <$> toInject x
toBody (BodyRecord x) = BodyRecord <$> toRecord x
toBody (BodyCase x) = BodyCase <$> toCase x
toBody (BodyLam x) = BodyLam <$> toBinder x
toBody (BodyApply x) = BodyApply <$> toApply x
toBody (BodyHole x) = BodyHole <$> toHole x
toBody (BodyGetVar x) = BodyGetVar <$> toGetVar x

toExpression ::
    MonadNaming m => OldExpression m a ->
    m (NewExpression m a)
toExpression = rBody toBody

withWhereItem ::
    MonadNaming m =>
    WhereItem (OldName m) (TM m) (OldExpression m a) ->
    CPS m (WhereItem (NewName m) (TM m) (NewExpression m a))
withWhereItem item@WhereItem{..} =
    CPS $ \k -> do
        (name, (value, res)) <-
            runCPS (opWithWhereItemName (isFunctionType (_wiAnnotation ^. aInferredType)) _wiName) $
            (,) <$> toBinder _wiValue <*> k
        pure (item { _wiValue = value, _wiName = name }, res)

withBinderParams ::
    MonadNaming m =>
    BinderParams (OldName m) (TM m) -> CPS m (BinderParams (NewName m) (TM m))
withBinderParams NoParams = pure NoParams
withBinderParams (VarParam FuncParam{..}) =
    opWithParamName (isFunctionType (_fpAnnotation ^. aInferredType)) _fpName
    <&> VarParam . \_fpName -> FuncParam{..}
withBinderParams (FieldParams xs) =
    (traverse . _2) f xs <&> FieldParams
    where
        f FuncParam{..} = opWithTagName _fpName <&> \_fpName -> FuncParam{..}

toBinder ::
    MonadNaming m =>
    Binder (OldName m) (TM m) (OldExpression m a) ->
    m (Binder (NewName m) (TM m) (NewExpression m a))
toBinder binder@Binder{..} =
    do
        (params, (whereItems, body)) <-
            runCPS (withBinderParams _bParams) .
            runCPS (traverse withWhereItem _bWhereItems) $
            toExpression _bBody
        binder
            { _bParams = params
            , _bBody = body
            , _bWhereItems = whereItems
            } & pure

toDefinitionBody ::
    MonadNaming m =>
    DefinitionBody (OldName m) (TM m) (OldExpression m a) ->
    m (DefinitionBody (NewName m) (TM m) (NewExpression m a))
toDefinitionBody (DefinitionBodyBuiltin bi) =
    pure (DefinitionBodyBuiltin bi)
toDefinitionBody
    (DefinitionBodyExpression (DefinitionExpression typeInfo content)) =
        DefinitionBodyExpression <$>
        (DefinitionExpression typeInfo <$> toBinder content)

toDef ::
    MonadNaming m =>
    Definition (OldName m) (TM m) (OldExpression m a) ->
    m (Definition (NewName m) (TM m) (NewExpression m a))
toDef def@Definition {..} =
    do
        (name, body) <- runCPS (opWithDefName _drName) $ toDefinitionBody _drBody
        pure def { _drName = name, _drBody = body }

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
        pass0 = runPass0M . toDef
        pass1 = runPass1M . toDef
        pass2 (def, storedNamesBelow) =
            runPass2M (emptyP2Env (storedNamesBelow ^. snwGlobalNames)) $ toDef def
