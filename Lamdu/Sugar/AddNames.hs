{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, TypeFamilies, TemplateHaskell, RankNTypes, DeriveGeneric, MultiParamTypeClasses, FlexibleInstances #-}
module Lamdu.Sugar.AddNames
  ( addToDef
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Lens.Tuple
import Control.Monad ((<=<))
import Control.Monad.Trans.Reader (Reader, runReader)
import Control.Monad.Trans.State (runState, evalState)
import Control.Monad.Trans.Writer (Writer, runWriter)
import Control.MonadA (MonadA)
import Data.Foldable (toList)
import Data.Map (Map)
import Data.Monoid (Monoid(..))
import Data.Monoid.Generic (def_mempty, def_mappend)
import Data.Set (Set)
import Data.Set.Ordered (OrderedSet)
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Data.Traversable (Traversable, traverse)
import GHC.Generics (Generic)
import Lamdu.Data.Anchors (assocNameRef)
import Lamdu.Expr.Type (Type)
import Lamdu.Sugar.AddNames.CPS (CPS(..))
import Lamdu.Sugar.AddNames.NameGen (NameGen)
import Lamdu.Sugar.AddNames.Types
import Lamdu.Sugar.Types
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.List.Utils as ListUtils
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Set.Ordered as OrderedSet
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Sugar.AddNames.NameGen as NameGen

type T = Transaction

type CPSNameConvertor m tm = NameProperty (OldName m) tm -> CPS m (NameProperty (NewName m) tm)
type NameConvertor m tm = NameProperty (OldName m) tm -> m (NameProperty (NewName m) tm)

newtype InTransaction m tm = InTransaction (forall a. m a -> T tm a)

class (MonadA m, MonadA tm) => MonadNaming m tm where
  type OldName m
  type NewName m
  opRun :: m (InTransaction m tm)

  opWithParamName :: NameGen.IsFunction -> CPSNameConvertor m tm
  opWithWhereItemName :: NameGen.IsFunction -> CPSNameConvertor m tm
  opWithDefName :: CPSNameConvertor m tm
  opWithTagName :: CPSNameConvertor m tm
  opGetDefName :: NameConvertor m tm
  opGetTagName :: NameConvertor m tm
  opGetParamName :: NameConvertor m tm
  opGetHiddenParamsName :: NameConvertor m tm

type StoredName = String

-- pass 0
data MStoredName = MStoredName
  { __mStoredName :: Maybe StoredName
  , _mStoredGuid :: Guid
  }
Lens.makeLenses ''MStoredName

newtype Pass0M tm a = Pass0M { runPass0M :: T tm a }
  deriving (Functor, Applicative, Monad)

instance MonadA tm => MonadNaming (Pass0M tm) tm where
  type OldName (Pass0M tm) = Guid
  type NewName (Pass0M tm) = MStoredName
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
      { __mStoredName = if null nameStr then Nothing else Just nameStr
      , _mStoredGuid = guid
      }

p0nameConvertor :: MonadA tm => NameConvertor (Pass0M tm) tm
p0nameConvertor = npName %%~ getMStoredName

p0cpsNameConvertor :: MonadA tm => CPSNameConvertor (Pass0M tm) tm
p0cpsNameConvertor nameProp =
  CPS $ \k -> (,) <$> p0nameConvertor nameProp <*> k

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
newtype Pass1M a = Pass1M (Writer StoredNamesWithin a)
  deriving (Functor, Applicative, Monad)
p1TellStoredNames :: StoredNamesWithin -> Pass1M ()
p1TellStoredNames = Pass1M . Writer.tell
p1ListenStoredNames :: Pass1M a -> Pass1M (a, StoredNamesWithin)
p1ListenStoredNames (Pass1M act) = Pass1M $ Writer.listen act
runPass1M :: Pass1M a -> (a, StoredNamesWithin)
runPass1M (Pass1M act) = runWriter act

data NameScope = Local | Global

instance MonadA tm => MonadNaming Pass1M tm where
  type OldName Pass1M = MStoredName
  type NewName Pass1M = StoredNames
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
  NameScope -> NameProperty MStoredName tm ->
  Pass1M (StoredNamesWithin -> NameProperty StoredNames tm)
pass1Result scope nameProp =
  nameProp
  & npName %%~ go
  <&> Lens.sequenceAOf npName
  where
    go (MStoredName mName guid) = do
      p1TellStoredNames myStoredNamesWithin
      pure $ \storedNamesUnder -> StoredNames
        { _storedName = MStoredName mName guid
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

p1nameConvertor :: NameScope -> NameConvertor Pass1M tm
p1nameConvertor scope nameProp = ($ mempty) <$> pass1Result scope nameProp

p1cpsNameConvertor :: NameScope -> CPSNameConvertor Pass1M tm
p1cpsNameConvertor scope mNameProperty = CPS $ \k -> do
  result <- pass1Result scope mNameProperty
  (res, storedNamesBelow) <- p1ListenStoredNames k
  pure (result storedNamesBelow, res)

-- pass 2:
data P2Env = P2Env
  { _p2NameGen :: NameGen Guid
  , _p2StoredNameSuffixes :: Map Guid Int
  , _p2StoredNames :: Set String
  }
Lens.makeLenses ''P2Env

newtype Pass2M a = Pass2M (Reader P2Env a)
  deriving (Functor, Applicative, Monad)
runPass2M :: P2Env -> Pass2M a -> a
runPass2M initial (Pass2M act) = runReader act initial
p2GetEnv :: Pass2M P2Env
p2GetEnv = Pass2M Reader.ask
p2WithEnv :: (P2Env -> P2Env) -> Pass2M a -> Pass2M a
p2WithEnv f (Pass2M act) = Pass2M $ Reader.local f act

instance MonadA tm => MonadNaming Pass2M tm where
  type OldName Pass2M = StoredNames
  type NewName Pass2M = Name
  opRun = (\env -> InTransaction (return . runPass2M env)) <$> p2GetEnv
  opWithDefName = p2cpsNameConvertorGlobal "def_"
  opWithTagName = p2cpsNameConvertorGlobal "tag_"
  opWithParamName = p2cpsNameConvertorLocal
  opWithWhereItemName = p2cpsNameConvertorLocal
  opGetParamName nameProp =
    nameProp & npName %%~
    \(StoredNames (MStoredName mName guid) storedNamesUnder) ->
    case mName of
      Just name ->
        makeStoredName name storedNamesUnder guid <$> p2GetEnv
      Nothing ->
        do
          nameGen <- (^. p2NameGen) <$> p2GetEnv
          pure . Name NameSourceAutoGenerated NoCollision $
            evalState (NameGen.existingName guid) nameGen
  opGetHiddenParamsName nameProp =
    nameProp & npName %%~
    \(StoredNames (MStoredName mName _) _) ->
    pure $ maybe
    (Name NameSourceAutoGenerated NoCollision "params")
    (Name NameSourceStored NoCollision)
    mName
  opGetTagName = p2nameConvertor "tag_"
  opGetDefName = p2nameConvertor "def_"

makeStoredName :: StoredName -> StoredNamesWithin -> Guid -> P2Env -> Name
makeStoredName name storedNamesBelow guid env =
  fst $ makeStoredNameEnv name storedNamesBelow guid env

compose :: [a -> a] -> a -> a
compose = foldr (.) id

makeStoredNameEnv ::
  StoredName -> StoredNamesWithin -> Guid -> P2Env -> (Name, P2Env)
makeStoredNameEnv name storedNamesBelow guid env =
  (Name NameSourceStored collision name, newEnv)
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
  NameProperty StoredNames tm ->
  (StoredNamesWithin -> P2Env -> (Name, P2Env)) ->
  CPS Pass2M (NameProperty Name tm)
p2cpsNameConvertor nameProp nameMaker =
  CPS $ \k -> do
    oldEnv <- p2GetEnv
    let
      (newName, newEnv) =
        case mName of
        Just name -> makeStoredNameEnv name storedNamesBelow guid oldEnv
        Nothing -> nameMaker storedNamesBelow oldEnv
    res <- p2WithEnv (const newEnv) k
    return (NameProperty newName setName, res)
  where
    StoredNames (MStoredName mName guid) storedNamesBelow = storedNames
    NameProperty storedNames setName = nameProp

makeGuidName :: Show guid => String -> guid -> Name
makeGuidName prefix guid = Name NameSourceAutoGenerated NoCollision $ prefix ++ show guid

p2cpsNameConvertorGlobal :: String -> CPSNameConvertor Pass2M tm
p2cpsNameConvertorGlobal prefix nameProp =
  p2cpsNameConvertor nameProp $
  \_ p2env -> (makeGuidName prefix (nameProp ^. npName.storedName.mStoredGuid), p2env)

p2cpsNameConvertorLocal :: NameGen.IsFunction -> CPSNameConvertor Pass2M tm
p2cpsNameConvertorLocal isFunction nameProp =
  p2cpsNameConvertor nameProp $
  \storedNamesBelow p2env ->
  (`runState` p2env) . Lens.zoom p2NameGen $
    let
      conflict name =
        Lens.has (snwGuidMap . Lens.at name . Lens._Just) storedNamesBelow ||
        (p2env ^. p2StoredNames . Lens.contains name)
    in
      Name NameSourceAutoGenerated NoCollision <$>
      NameGen.newName (not . conflict) isFunction
      (nameProp^.npName.storedName.mStoredGuid)

p2nameConvertor :: String -> NameConvertor Pass2M tm
p2nameConvertor prefix (NameProperty storedNames setName) =
  mkNameProperty <$>
  case mName of
  Just str -> makeStoredName str storedNamesBelow guid <$> p2GetEnv
  Nothing -> pure $ makeGuidName prefix guid
  where
    StoredNames (MStoredName mName guid) storedNamesBelow = storedNames
    mkNameProperty name = NameProperty name setName

isFunctionType :: Type -> NameGen.IsFunction
isFunctionType T.TFun {} = NameGen.Function
isFunctionType _ = NameGen.NotFunction

withFuncParam ::
  (MonadA tm, MonadNaming m tm) =>
  FuncParam (OldName m) tm -> CPS m (FuncParam (NewName m) tm)
withFuncParam fp@FuncParam{..} = CPS $ \k -> do
  (name, res) <-
    (`runCPS` k) $
    case _fpVarKind of
    FuncFieldParameter -> opWithTagName _fpName
    FuncParameter -> opWithParamName (isFunctionType _fpInferredType) _fpName
  pure
    ( fp { _fpName = name }
    , res
    )

toLam ::
  (MonadA tm, MonadNaming m tm) =>
  Lam (OldName m) tm (Expression (OldName m) tm a) ->
  m (Lam (NewName m) tm (Expression (NewName m) tm a))
toLam lam@Lam {..} = do
  (param, result) <- runCPS (withFuncParam _lParam) $ toExpression _lResult
  pure lam { _lParam = param, _lResult = result }

toTagG :: MonadNaming m tm => TagG (OldName m) tm -> m (TagG (NewName m) tm)
toTagG = tagGName opGetTagName

toRecordField ::
  (MonadA tm, MonadNaming m tm) =>
  RecordField (OldName m) tm (Expression (OldName m) tm a) ->
  m (RecordField (NewName m) tm (Expression (NewName m) tm a))
toRecordField recordField@RecordField {..} = do
  tag <- toTagG _rfTag
  expr <- toExpression _rfExpr
  pure recordField
    { _rfTag = tag
    , _rfExpr = expr
    }

toRecord ::
  (MonadA tm, MonadNaming m tm) =>
  Record (OldName m) tm (Expression (OldName m) tm a) ->
  m (Record (NewName m) tm (Expression (NewName m) tm a))
toRecord record@Record {..} = do
  items <- traverse toRecordField _rItems
  t <- traverse toExpression _rTail
  pure record { _rItems = items, _rTail = t }

toGetField ::
  (MonadA tm, MonadNaming m tm) =>
  GetField (OldName m) tm (Expression (OldName m) tm a) ->
  m (GetField (NewName m) tm (Expression (NewName m) tm a))
toGetField getField@GetField {..} = do
  record <- toExpression _gfRecord
  tag <- toTagG _gfTag
  pure getField { _gfRecord = record, _gfTag = tag }

toScope :: MonadNaming m tm => Scope (OldName m) tm -> m (Scope (NewName m) tm)
toScope (Scope l g p) =
  Scope
  <$> (traverse . _1) toGetVar l
  <*> (traverse . _1) toGetVar g
  <*> (traverse . _1) toGetParams p

toHoleActions ::
  (MonadA tm, MonadNaming m tm) =>
  HoleActions (OldName m) tm ->
  m (HoleActions (NewName m) tm)
toHoleActions ha@HoleActions {..} = do
  InTransaction run <- opRun
  pure ha
    { _holeScope =
      run . toScope =<< _holeScope
    , holeResult =
      holeResult
      & Lens.mapped %~
        (run . (Lens.traversed . holeResultConverted %%~ toExpression) =<<)
    }

toHoleSuggested ::
  (MonadA tm, MonadNaming m tm) =>
  HoleSuggested (OldName m) tm ->
  m (HoleSuggested (NewName m) tm)
toHoleSuggested inferred = do
  InTransaction run <- opRun
  inferred
    & hsMakeConverted %~ (run . toExpression =<<)
    & return

toHole ::
  (MonadA tm, MonadNaming m tm) =>
  Hole (OldName m) tm (Expression (OldName m) tm a) ->
  m (Hole (NewName m) tm (Expression (NewName m) tm a))
toHole hole@Hole {..} = do
  mActions <- _holeMActions & Lens._Just %%~ toHoleActions
  inferred <- toHoleSuggested _holeSuggested
  mArg <- _holeMArg & Lens._Just . Lens.traversed %%~ toExpression
  pure hole
    { _holeMActions = mActions
    , _holeMArg = mArg
    , _holeSuggested = inferred
    }

toGetVar ::
  MonadNaming m tm => GetVar (OldName m) tm ->
  m (GetVar (NewName m) tm)
toGetVar getVar =
  gvName f getVar
  where
    f =
      case getVar ^. gvVarType of
      GetParameter -> opGetParamName
      GetFieldParameter -> opGetTagName
      GetDefinition -> opGetDefName

toGetParams ::
  MonadNaming m tm => GetParams (OldName m) tm ->
  m (GetParams (NewName m) tm)
toGetParams = gpDefName opGetDefName

toApply ::
  (MonadNaming m tm, MonadA tm) =>
  Apply (OldName m) tm (Expression (OldName m) tm a) ->
  m (Apply (NewName m) tm (Expression (NewName m) tm a))
toApply la@Apply{..} = do
  func <- toExpression _aFunc
  specialArgs <- traverse toExpression _aSpecialArgs
  annotatedArgs <- traverse (aaTag toTagG <=< aaExpr toExpression) _aAnnotatedArgs
  pure la
    { _aFunc = func
    , _aSpecialArgs = specialArgs
    , _aAnnotatedArgs = annotatedArgs
    }

traverseToExpr ::
  (MonadA tm, MonadNaming m tm, Traversable t) =>
  (t (Expression (NewName m) tm a) -> b) -> t (Expression (OldName m) tm a) ->
  m b
traverseToExpr cons body = cons <$> traverse toExpression body

toBody ::
  (MonadA tm, MonadNaming m tm) =>
  Body (OldName m) tm (Expression (OldName m) tm a) ->
  m (Body (NewName m) tm (Expression (NewName m) tm a))
toBody (BodyList x)           = traverseToExpr BodyList x
toBody (BodyLiteralInteger x) = pure $ BodyLiteralInteger x
--
toBody (BodyGetField x) = BodyGetField <$> toGetField x
toBody (BodyRecord x) = BodyRecord <$> toRecord x
toBody (BodyLam x) = BodyLam <$> toLam x
toBody (BodyApply x) = BodyApply <$> toApply x
toBody (BodyHole x) = BodyHole <$> toHole x
toBody (BodyGetVar x) = BodyGetVar <$> toGetVar x
toBody (BodyGetParams x) = BodyGetParams <$> toGetParams x

toExpression ::
  (MonadA tm, MonadNaming m tm) => Expression (OldName m) tm a ->
  m (Expression (NewName m) tm a)
toExpression = rBody toBody

withWhereItem ::
  (MonadA tm, MonadNaming m tm) =>
  WhereItem (OldName m) tm (Expression (OldName m) tm a) ->
  CPS m (WhereItem (NewName m) tm (Expression (NewName m) tm a))
withWhereItem item@WhereItem{..} = CPS $ \k -> do
  (name, (value, res)) <-
    runCPS (opWithWhereItemName (isFunctionType _wiInferredType) _wiName) $
    (,) <$> toDefinitionContent _wiValue <*> k
  pure (item { _wiValue = value, _wiName = name }, res)

toDefinitionContent ::
  (MonadA tm, MonadNaming m tm) =>
  DefinitionContent (OldName m) tm (Expression (OldName m) tm a) ->
  m (DefinitionContent (NewName m) tm (Expression (NewName m) tm a))
toDefinitionContent def@DefinitionContent{..} = do
  (params, (whereItems, body)) <-
    runCPS (traverse withFuncParam _dParams) .
    runCPS (traverse withWhereItem _dWhereItems) $
    toExpression _dBody
  pure def
    { _dParams = params
    , _dBody = body
    , _dWhereItems = whereItems
    }

toDefinitionBody ::
  (MonadA tm, MonadNaming m tm) =>
  DefinitionBody (OldName m) tm (Expression (OldName m) tm a) ->
  m (DefinitionBody (NewName m) tm (Expression (NewName m) tm a))
toDefinitionBody (DefinitionBodyBuiltin bi) =
  pure (DefinitionBodyBuiltin bi)
toDefinitionBody
  (DefinitionBodyExpression (DefinitionExpression typeInfo content)) =
    DefinitionBodyExpression <$>
    (DefinitionExpression typeInfo <$> toDefinitionContent content)

toDef ::
  (MonadA tm, MonadNaming m tm) =>
  Definition (OldName m) tm (Expression (OldName m) tm a) ->
  m (Definition (NewName m) tm (Expression (NewName m) tm a))
toDef def@Definition {..} = do
  (name, body) <- runCPS (opWithDefName _drName) $ toDefinitionBody _drBody
  pure def { _drName = name, _drBody = body }

addToDef :: MonadA tm => DefinitionU tm a -> T tm (DefinitionN tm a)
addToDef =
  fmap (pass2 . pass1) . pass0
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
