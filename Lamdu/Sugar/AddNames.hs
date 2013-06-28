{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, TypeFamilies, TemplateHaskell, RankNTypes #-}
module Lamdu.Sugar.AddNames
  ( addToDef
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Monad ((<=<))
import Control.Monad.Trans.Reader (Reader, runReader)
import Control.Monad.Trans.State (runState, evalState)
import Control.Monad.Trans.Writer (Writer, runWriter)
import Control.MonadA (MonadA)
import Data.Derive.Monoid (makeMonoid)
import Data.DeriveTH (derive)
import Data.Map (Map)
import Data.Monoid (Monoid(..))
import Data.Set (Set)
import Data.Store.Guid (Guid)
import Data.Traversable (Traversable, traverse)
import Lamdu.Sugar.AddNames.CPS (CPS(..))
import Lamdu.Sugar.AddNames.NameGen (NameGen)
import Lamdu.Sugar.Types
import Prelude hiding (pi)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.List.Utils as ListUtils
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lamdu.Sugar.AddNames.NameGen as NameGen

type CPSNameConvertor m = Guid -> OldName m -> CPS m (NewName m)
type NameConvertor m = Guid -> OldName m -> m (NewName m)

newtype RunMonad m = RunMonad (forall a. m a -> a)

class MonadA m => MonadNaming m where
  type OldName m
  type NewName m
  opRun :: m (RunMonad m)

  opWithParamName :: NameGen.IsDependent -> CPSNameConvertor m
  opWithWhereItemName :: CPSNameConvertor m
  opWithDefName :: CPSNameConvertor m
  opWithTagName :: CPSNameConvertor m
  opGetDefName :: NameConvertor m
  opGetTagName :: NameConvertor m
  opGetParamName :: NameConvertor m
  opGetHiddenParamsName :: NameConvertor m

newtype SetList a = SetList { getSetList :: [a] }
  deriving (Show)
instance Eq a => Monoid (SetList a) where
  mempty = SetList []
  SetList xs `mappend` SetList ys = SetList $ xs ++ filter (`notElem` xs) ys

type StoredName = String
newtype NameGuidMap = NameGuidMap (Map StoredName (SetList Guid))
  deriving (Show, Lens.At)
type instance Lens.Index NameGuidMap = StoredName
type instance Lens.IxValue NameGuidMap = SetList Guid
instance Monoid NameGuidMap where
  mempty = NameGuidMap Map.empty
  NameGuidMap x `mappend` NameGuidMap y =
    NameGuidMap $ Map.unionWith mappend x y

nameGuidMapSingleton :: StoredName -> Guid -> NameGuidMap
nameGuidMapSingleton name guid = NameGuidMap . Map.singleton name $ SetList [guid]

data StoredNamesWithin = StoredNamesWithin
  { _snwGuidMap :: NameGuidMap
  -- Names of tags and defs: considered conflicted if used in two
  -- different meanings anywhere in the whole definition:
  , _snwGlobalNames :: NameGuidMap
  }
Lens.makeLenses ''StoredNamesWithin
derive makeMonoid ''StoredNamesWithin

-- Pass 0:
data StoredNames = StoredNames
  { storedName :: Maybe StoredName
  , storedNamesWithin :: StoredNamesWithin
  }
newtype Pass0M a = Pass0M (Writer StoredNamesWithin a)
  deriving (Functor, Applicative, Monad)
p0TellStoredNames :: StoredNamesWithin -> Pass0M ()
p0TellStoredNames = Pass0M . Writer.tell
p0ListenStoredNames :: Pass0M a -> Pass0M (a, StoredNamesWithin)
p0ListenStoredNames (Pass0M act) = Pass0M $ Writer.listen act
runPass0M :: Pass0M a -> (a, StoredNamesWithin)
runPass0M (Pass0M act) = runWriter act

data NameScope = Local | Global

instance MonadNaming Pass0M where
  type OldName Pass0M = MStoredName
  type NewName Pass0M = StoredNames
  opRun = pure $ RunMonad $ fst . runPass0M
  opWithParamName _ = p0cpsNameConvertor Local
  opWithWhereItemName = p0cpsNameConvertor Local
  opWithDefName = p0cpsNameConvertor Local
  opWithTagName = p0cpsNameConvertor Local
  opGetParamName = p0nameConvertor Local
  opGetHiddenParamsName = p0nameConvertor Local
  opGetTagName = p0nameConvertor Global
  opGetDefName = p0nameConvertor Global

pass0Result :: NameScope -> Guid -> MStoredName -> Pass0M (StoredNamesWithin -> StoredNames)
pass0Result scope guid mName = do
  p0TellStoredNames myStoredNamesWithin
  pure $ \storedNamesUnder -> StoredNames
    { storedName = mName
    , storedNamesWithin = myStoredNamesWithin `mappend` storedNamesUnder
    }
  where
    myStoredNamesWithin =
      maybe mempty (buildStoredNamesWithin . (`nameGuidMapSingleton` guid)) mName
    buildStoredNamesWithin myNameGuidMap =
      StoredNamesWithin myNameGuidMap $
      globalNames myNameGuidMap
    globalNames myNameGuidMap =
      case scope of
      Local -> mempty
      Global -> myNameGuidMap

p0nameConvertor :: NameScope -> NameConvertor Pass0M
p0nameConvertor scope guid mName =
  ($ mempty) <$> pass0Result scope guid mName

p0cpsNameConvertor :: NameScope -> CPSNameConvertor Pass0M
p0cpsNameConvertor scope guid mName = CPS $ \k -> do
  result <- pass0Result scope guid mName
  (res, storedNamesBelow) <- p0ListenStoredNames k
  pure (result storedNamesBelow, res)

-- Pass 1:
data P1Env = P1Env
  { _p1NameGen :: NameGen Guid
  , _p1StoredNameSuffixes :: Map Guid Int
  , _p1StoredNames :: Set String
  }
Lens.makeLenses ''P1Env

newtype Pass1M a = Pass1M (Reader P1Env a)
  deriving (Functor, Applicative, Monad)
runPass1M :: P1Env -> Pass1M a -> a
runPass1M initial (Pass1M act) = runReader act initial
p1GetEnv :: Pass1M P1Env
p1GetEnv = Pass1M Reader.ask
p1WithEnv :: (P1Env -> P1Env) -> Pass1M a -> Pass1M a
p1WithEnv f (Pass1M act) = Pass1M $ Reader.local f act

instance MonadNaming Pass1M where
  type OldName Pass1M = StoredNames
  type NewName Pass1M = Name
  opRun = (\x -> RunMonad (runPass1M x)) <$> p1GetEnv
  opWithDefName = p1cpsNameConvertorGlobal "def_"
  opWithTagName = p1cpsNameConvertorGlobal "tag_"
  opWithParamName = p1cpsNameConvertorLocal
  opWithWhereItemName = p1cpsNameConvertorLocal NameGen.Independent
  opGetParamName guid (StoredNames (Just str) storedNamesUnder) =
    makeStoredName str storedNamesUnder guid <$> p1GetEnv
  opGetParamName guid (StoredNames Nothing _) = do
    nameGen <- (^. p1NameGen) <$> p1GetEnv
    pure . Name AutoGeneratedName NoCollision $
      evalState (NameGen.existingName guid) nameGen
  opGetHiddenParamsName _ (StoredNames mName _) =
    pure $ maybe (Name AutoGeneratedName NoCollision "params") (Name StoredName NoCollision) mName
  opGetTagName = p1nameConvertor "tag_"
  opGetDefName = p1nameConvertor "def_"

makeStoredName :: StoredName -> StoredNamesWithin -> Guid -> P1Env -> Name
makeStoredName storedName storedNamesBelow guid env =
  fst $ makeStoredNameEnv storedName storedNamesBelow guid env

compose :: [a -> a] -> a -> a
compose = foldr (.) id

makeStoredNameEnv ::
  StoredName -> StoredNamesWithin -> Guid -> P1Env -> (Name, P1Env)
makeStoredNameEnv storedName storedNamesBelow guid env =
  (Name StoredName collision storedName, newEnv)
  where
    (collision, newEnv) =
      case (mSuffixFromAbove, collidingGuids) of
        (Just suffix, _) -> (Collision suffix, env)
        (Nothing, []) -> (NoCollision, envWithName [])
        (Nothing, otherGuids) -> (Collision 0, envWithName (guid:otherGuids))
    envWithName guids = env
      & p1StoredNames %~ Set.insert storedName
      -- This name is first occurence, so we get suffix 0
      & p1StoredNameSuffixes %~ compose ((Lens.itraversed %@~ flip Map.insert) guids)
    mSuffixFromAbove =
      Map.lookup guid $ env ^. p1StoredNameSuffixes
    collidingGuids =
      maybe [] (filter (/= guid) . getSetList) $
      storedNamesBelow ^. snwGuidMap . Lens.at storedName

p1cpsNameConvertor ::
  Guid -> StoredNames -> (StoredNamesWithin -> P1Env -> (Name, P1Env)) -> CPS Pass1M Name
p1cpsNameConvertor guid storedNames nameMaker =
  CPS $ \k -> do
    oldEnv <- p1GetEnv
    let
      (name, newEnv) =
        case storedNames of
        StoredNames (Just storedName) storedNamesBelow ->
          makeStoredNameEnv storedName storedNamesBelow guid oldEnv
        StoredNames Nothing storedNamesBelow ->
          nameMaker storedNamesBelow oldEnv
    res <- p1WithEnv (const newEnv) k
    return (name, res)

p1cpsNameConvertorGlobal :: String -> CPSNameConvertor Pass1M
p1cpsNameConvertorGlobal prefix guid storedNames =
  p1cpsNameConvertor guid storedNames $
  \_ p1env -> (makeGuidName prefix guid, p1env)

p1cpsNameConvertorLocal :: NameGen.IsDependent -> CPSNameConvertor Pass1M
p1cpsNameConvertorLocal isDep guid storedNames =
  p1cpsNameConvertor guid storedNames $
  \storedNamesBelow p1env ->
  (`runState` p1env) . Lens.zoom p1NameGen $
    let
      conflict name =
        (storedNamesBelow ^. snwGuidMap . Lens.containsAt name) ||
        (p1env ^. p1StoredNames . Lens.contains name)
    in
      Name AutoGeneratedName NoCollision <$>
      NameGen.newName (not . conflict)
      isDep guid

p1nameConvertor :: String -> NameConvertor Pass1M
p1nameConvertor _ guid (StoredNames (Just str) storedNamesBelow) =
  makeStoredName str storedNamesBelow guid <$> p1GetEnv
p1nameConvertor prefix guid (StoredNames Nothing _) = pure $ makeGuidName prefix guid

makeGuidName :: Show guid => String -> guid -> Name
makeGuidName prefix guid = Name AutoGeneratedName NoCollision $ prefix ++ show guid

withFuncParam ::
  (MonadA tm, MonadNaming m) =>
  NameGen.IsDependent ->
  FuncParam (OldName m) tm (Expression (OldName m) tm a) ->
  CPS m (FuncParam (NewName m) tm (Expression (NewName m) tm a))
withFuncParam isDep fp@FuncParam{..} = CPS $ \k -> do
  mActions <- traverse toFuncParamActions _fpMActions
  typ <- toExpression _fpType
  (name, res) <-
    case _fpVarKind of
    FuncParameter ->
      runCPS (opWithParamName isDep _fpGuid _fpName) k
    FuncFieldParameter ->
      runCPS (opWithTagName _fpGuid _fpName) k
  pure
    ( fp
      { _fpName = name
      , _fpMActions = mActions
      , _fpType = typ
      }
    , res
    )

toLam ::
  (MonadA tm, MonadNaming m) =>
  Lam (OldName m) tm (Expression (OldName m) tm a) ->
  m (Lam (NewName m) tm (Expression (NewName m) tm a))
toLam lam@Lam {..} = do
  (param, resultType) <- runCPS (withFuncParam isDep _lParam) $ toExpression _lResultType
  pure lam { _lParam = param, _lResultType = resultType }
  where
    isDep =
      case _lKind of
      KVal | not _lIsDep -> NameGen.Independent
      _ -> NameGen.Dependent

toScope :: MonadNaming m => Scope (OldName m) tm -> m (Scope (NewName m) tm)
toScope (Scope l g t p) =
  Scope
  <$> (traverse . Lens._1) toGetVar l
  <*> (traverse . Lens._1) toGetVar g
  <*> (traverse . Lens._1) toTag t
  <*> (traverse . Lens._1) toGetParams p

toHoleActions ::
  (MonadA tm, MonadNaming m) =>
  HoleActions (OldName m) tm ->
  m (HoleActions (NewName m) tm)
toHoleActions ha@HoleActions {..} = do
  RunMonad run <- opRun
  pure ha
    { _holeScope =
      fmap (run . toScope) _holeScope
    , holeResult =
      (fmap . fmap . fmap) (run . holeResultConverted toExpression) holeResult
    }

toHole ::
  (MonadA tm, MonadNaming m) =>
  Hole (OldName m) tm (Expression (OldName m) tm a) ->
  m (Hole (NewName m) tm (Expression (NewName m) tm a))
toHole =
  (holeMArg . Lens._Just . Lens.traversed) toExpression <=<
  (holeMActions . Lens.traversed) toHoleActions

toInferred ::
  (MonadA tm, MonadNaming m) =>
  Inferred (OldName m) tm (Expression (OldName m) tm a) ->
  m (Inferred (NewName m) tm (Expression (NewName m) tm a))
toInferred Inferred {..} = do
  value <- toExpression _iValue
  hole <- toHole _iHole
  pure Inferred { _iValue = value, _iHole = hole, .. }

toCollapsed ::
  (MonadA tm, MonadNaming m) =>
  Collapsed (OldName m) tm (Expression (OldName m) tm a) ->
  m (Collapsed (NewName m) tm (Expression (NewName m) tm a))
toCollapsed Collapsed {..} = do
  compact <- toGetVar _cCompact
  fullExpression <- toExpression _cFullExpression
  pure Collapsed { _cCompact = compact, _cFullExpression = fullExpression, .. }

toTag ::
  MonadNaming m => TagG (OldName m) ->
  m (TagG (NewName m))
toTag (TagG guid oldName) = do
  name <- opGetTagName guid oldName
  pure $ TagG guid name

toGetVar ::
  MonadNaming m => GetVar (OldName m) tm ->
  m (GetVar (NewName m) tm)
toGetVar getVar@GetVar{..} =
  gvName (f _gvIdentifier) getVar
  where
    f =
      case _gvVarType of
      GetParameter -> opGetParamName
      GetFieldParameter -> opGetTagName
      GetDefinition -> opGetDefName

toGetParams ::
  MonadNaming m => GetParams (OldName m) tm ->
  m (GetParams (NewName m) tm)
toGetParams getParams@GetParams{..} =
  gpDefName (opGetDefName _gpDefGuid) getParams

toApply ::
  (MonadNaming m, MonadA tm) =>
  Apply (OldName m) (Expression (OldName m) tm a) ->
  m (Apply (NewName m) (Expression (NewName m) tm a))
toApply la@Apply{..} = do
  func <- toExpression _aFunc
  specialArgs <- traverse toExpression _aSpecialArgs
  annotatedArgs <- traverse (aaTag toTag <=< aaExpr toExpression) _aAnnotatedArgs
  pure la
    { _aFunc = func
    , _aSpecialArgs = specialArgs
    , _aAnnotatedArgs = annotatedArgs
    }

traverseToExpr ::
  (MonadA tm, MonadNaming m, Traversable t) =>
  (t (Expression (NewName m) tm a) -> b) -> t (Expression (OldName m) tm a) ->
  m b
traverseToExpr cons body = cons <$> traverse toExpression body

toBody ::
  (MonadA tm, MonadNaming m) =>
  Body (OldName m) tm (Expression (OldName m) tm a) ->
  m (Body (NewName m) tm (Expression (NewName m) tm a))
toBody (BodyList x)           = traverseToExpr BodyList x
toBody (BodyRecord x)         = traverseToExpr BodyRecord x
toBody (BodyGetField x)       = traverseToExpr BodyGetField x
toBody (BodyLiteralInteger x) = pure $ BodyLiteralInteger x
toBody (BodyAtom x)           = pure $ BodyAtom x
--
toBody (BodyLam x) = BodyLam <$> toLam x
toBody (BodyApply x) = BodyApply <$> toApply x
toBody (BodyHole x) = BodyHole <$> toHole x
toBody (BodyInferred x) = BodyInferred <$> toInferred x
toBody (BodyCollapsed x) = BodyCollapsed <$> toCollapsed x
toBody (BodyTag x) = BodyTag <$> toTag x
toBody (BodyGetVar x) = BodyGetVar <$> toGetVar x
toBody (BodyGetParams x) = BodyGetParams <$> toGetParams x

toPayload ::
  (MonadA tm, MonadNaming m) => Payload (OldName m) tm a ->
  m (Payload (NewName m) tm a)
toPayload = (plInferredTypes . traverse) toExpression

toExpression ::
  (MonadA tm, MonadNaming m) => Expression (OldName m) tm a ->
  m (Expression (NewName m) tm a)
toExpression expr@Expression{..} = do
  body <- toBody _rBody
  pl <- toPayload _rPayload
  pure expr { _rBody = body, _rPayload = pl }

toFuncParamActions ::
  (MonadNaming m, MonadA tm) => FuncParamActions (OldName m) tm ->
  m (FuncParamActions (NewName m) tm)
toFuncParamActions fpa = do
  RunMonad run <- opRun
  pure $ fpa & fpGetExample . Lens.mapped %~ run . toExpression

withWhereItem ::
  (MonadA tm, MonadNaming m) =>
  WhereItem (OldName m) tm (Expression (OldName m) tm a) ->
  CPS m (WhereItem (NewName m) tm (Expression (NewName m) tm a))
withWhereItem item@WhereItem{..} = CPS $ \k -> do
  (name, (value, res)) <-
    runCPS (opWithWhereItemName _wiGuid _wiName) $
    (,) <$> toDefinitionContent _wiValue <*> k
  pure (item { _wiValue = value, _wiName = name }, res)

toDefinitionContent ::
  (MonadA tm, MonadNaming m) =>
  DefinitionContent (OldName m) tm (Expression (OldName m) tm a) ->
  m (DefinitionContent (NewName m) tm (Expression (NewName m) tm a))
toDefinitionContent def@DefinitionContent{..} = do
  (depParams, (params, (whereItems, body))) <-
    runCPS (traverse (withFuncParam NameGen.Dependent) _dDepParams) .
    runCPS (traverse (withFuncParam NameGen.Independent) _dParams) .
    runCPS (traverse withWhereItem _dWhereItems) $
    toExpression _dBody
  pure def
    { _dDepParams = depParams
    , _dParams = params
    , _dBody = body
    , _dWhereItems = whereItems
    }

toDefinitionBody ::
  (MonadA tm, MonadNaming m) =>
  DefinitionBody (OldName m) tm (Expression (OldName m) tm a) ->
  m (DefinitionBody (NewName m) tm (Expression (NewName m) tm a))
toDefinitionBody (DefinitionBodyBuiltin bi) =
  DefinitionBodyBuiltin <$> traverse toExpression bi
toDefinitionBody
  (DefinitionBodyExpression (DefinitionExpression typeInfo content)) =
    DefinitionBodyExpression <$>
    (DefinitionExpression <$>
     traverse toExpression typeInfo <*>
     toDefinitionContent content)

toDef ::
  (MonadA tm, MonadNaming m) =>
  Definition (OldName m) tm (Expression (OldName m) tm a) ->
  m (Definition (NewName m) tm (Expression (NewName m) tm a))
toDef def@Definition {..} = do
  (name, body) <-
    runCPS (opWithDefName _drGuid _drName) $ toDefinitionBody _drBody
  pure def { _drName = name, _drBody = body }

addToDef :: MonadA m => DefinitionU m a -> DefinitionN m a
addToDef =
  pass1 . runPass0M . toDef
  where
    emptyP1Env (NameGuidMap globalNamesMap) = P1Env
      { _p1NameGen = NameGen.initial
      , _p1StoredNames = mempty
      , _p1StoredNameSuffixes =
        mconcat .
        map Map.fromList . filter (ListUtils.isLengthAtLeast 2) .
        map ((`zip` [0..]) . getSetList) . Map.elems $ globalNamesMap
      }
    pass1 (def, storedNamesBelow) =
      runPass1M (emptyP1Env (storedNamesBelow ^. snwGlobalNames)) $ toDef def
