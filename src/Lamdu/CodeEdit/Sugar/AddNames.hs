{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, TypeFamilies, TemplateHaskell #-}
module Lamdu.CodeEdit.Sugar.AddNames
  ( addToDef
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Monad ((<=<))
import Control.Monad.Trans.Reader (Reader, runReader)
import Control.Monad.Trans.State (runState, evalState)
import Control.Monad.Trans.Writer (Writer, runWriter)
import Control.MonadA (MonadA)
import Data.Map (Map)
import Data.Monoid (Monoid(..))
import Data.Set (Set)
import Data.Store.Guid (Guid)
import Data.Traversable (Traversable, traverse)
import Lamdu.CodeEdit.Sugar.AddNames.CPS (CPS(..))
import Lamdu.CodeEdit.Sugar.NameGen (NameGen)
import Lamdu.CodeEdit.Sugar.Types
import Prelude hiding (pi)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lamdu.CodeEdit.Sugar.NameGen as NameGen

type CPSNameConvertor m = Guid -> OldName m -> CPS m (NewName m)
type NameConvertor m = Guid -> OldName m -> m (NewName m)

class MonadA m => MonadNaming m where
  type OldName m
  type NewName m
  opRun :: m (m res -> res)

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

-- Pass 0:
data StoredNames = StoredNames
  { storedName :: Maybe StoredName
  , storedNamesWithin :: NameGuidMap
  }
newtype Pass0M a = Pass0M (Writer NameGuidMap a)
  deriving (Functor, Applicative, Monad)
p0TellStoredNames :: NameGuidMap -> Pass0M ()
p0TellStoredNames = Pass0M . Writer.tell
p0ListenStoredNames :: Pass0M a -> Pass0M (a, NameGuidMap)
p0ListenStoredNames (Pass0M act) = Pass0M $ Writer.listen act
runPass0M :: Pass0M a -> (a, NameGuidMap)
runPass0M (Pass0M act) = runWriter act

instance MonadNaming Pass0M where
  type OldName Pass0M = MStoredName
  type NewName Pass0M = StoredNames
  opRun = pure (fst . runPass0M)
  opWithParamName _ = p0cpsNameConvertor
  opWithWhereItemName = p0cpsNameConvertor
  opWithDefName = p0cpsNameConvertor
  opWithTagName = p0cpsNameConvertor
  opGetParamName = p0nameConvertor
  opGetHiddenParamsName = p0nameConvertor
  opGetTagName = p0nameConvertor
  opGetDefName = p0nameConvertor

pass0Result :: Guid -> MStoredName -> Pass0M (NameGuidMap -> StoredNames)
pass0Result guid mName = do
  p0TellStoredNames myNameGuidMap
  pure $ \storedNameGuidMapBelow -> StoredNames
    { storedName = mName
    , storedNamesWithin = myNameGuidMap `mappend` storedNameGuidMapBelow
    }
  where
    myNameGuidMap =
      maybe mempty (`nameGuidMapSingleton` guid) mName

p0nameConvertor :: NameConvertor Pass0M
p0nameConvertor guid mName =
  ($ mempty) <$> pass0Result guid mName

p0cpsNameConvertor :: CPSNameConvertor Pass0M
p0cpsNameConvertor guid mName = CPS $ \k -> do
  result <- pass0Result guid mName
  (res, storedNameGuidMapBelow) <- p0ListenStoredNames k
  pure (result storedNameGuidMapBelow, res)

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
  opRun = runPass1M <$> p1GetEnv
  opWithDefName = p1cpsNameConvertorGlobal "def_"
  opWithTagName = p1cpsNameConvertorGlobal "tag_"
  opWithParamName = p1cpsNameConvertorLocal
  opWithWhereItemName = p1cpsNameConvertorLocal NameGen.Independent
  opGetParamName guid (StoredNames (Just str) nameGuidMapBelow) =
    makeStoredName str nameGuidMapBelow guid <$> p1GetEnv
  opGetParamName guid (StoredNames Nothing _) = do
    nameGen <- (^. p1NameGen) <$> p1GetEnv
    pure . Name AutoGeneratedName NoCollision $
      evalState (NameGen.existingName guid) nameGen
  opGetHiddenParamsName _ (StoredNames mName _) =
    pure $ maybe (Name AutoGeneratedName NoCollision "params") (Name StoredName NoCollision) mName
  opGetTagName = p1nameConvertor "tag_"
  opGetDefName = p1nameConvertor "def_"

makeStoredName :: StoredName -> NameGuidMap -> Guid -> P1Env -> Name
makeStoredName storedName nameGuidMapBelow guid env =
  fst $ makeStoredNameEnv storedName nameGuidMapBelow guid env

compose :: [a -> a] -> a -> a
compose = foldr (.) id

makeStoredNameEnv ::
  StoredName -> NameGuidMap -> Guid -> P1Env -> (Name, P1Env)
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
      storedNamesBelow ^. Lens.at storedName

p1cpsNameConvertor ::
  Guid -> StoredNames -> (NameGuidMap -> P1Env -> (Name, P1Env)) -> CPS Pass1M Name
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
  \storedNameGuidMapBelow p1env ->
  (`runState` p1env) . Lens.zoom p1NameGen $
    let
      conflict name =
        (storedNameGuidMapBelow ^. Lens.containsAt name) ||
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
  FuncParam (OldName m) tm (Expression (OldName m) tm) ->
  CPS m (FuncParam (NewName m) tm (Expression (NewName m) tm))
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
  Lam (OldName m) tm (Expression (OldName m) tm) ->
  m (Lam (NewName m) tm (Expression (NewName m) tm))
toLam lam@Lam {..} = do
  (param, resultType) <- runCPS (withFuncParam isDep _lParam) $ toExpression _lResultType
  pure lam { _lParam = param, _lResultType = resultType }
  where
    isDep =
      case _lKind of
      Val | not _lIsDep -> NameGen.Independent
      _ -> NameGen.Dependent

toScope :: MonadNaming m => Scope (OldName m) tm -> m (Scope (NewName m) tm)
toScope (Scope l g t p) =
  Scope
  <$> (traverse . Lens._1) toGetVar l
  <*> (traverse . Lens._1) toGetVar g
  <*> (traverse . Lens._1) toTag t
  <*> (traverse . Lens._1) toGetParams p

toHoleActions ::
  (MonadA tm, MonadNaming m) => HoleActions (OldName m) tm ->
  m (HoleActions (NewName m) tm)
toHoleActions ha@HoleActions {..} = do
  run0 <- opRun
  run1 <- opRun
  pure ha
    { _holeScope =
      fmap (run1 . toScope) _holeScope
    , _holeResult =
      (fmap . fmap . fmap) (run0 . holeResultConverted toExpression) _holeResult
    }

toHole ::
  (MonadA tm, MonadNaming m) => Hole (OldName m) tm ->
  m (Hole (NewName m) tm)
toHole = (holeMActions . Lens.traversed) toHoleActions

toInferred ::
  (MonadA tm, MonadNaming m) => Inferred (OldName m) tm (Expression (OldName m) tm) ->
  m (Inferred (NewName m) tm (Expression (NewName m) tm))
toInferred Inferred {..} = do
  value <- toExpression _iValue
  hole <- toHole _iHole
  pure Inferred { _iValue = value, _iHole = hole, .. }

toCollapsed ::
  (MonadA tm, MonadNaming m) =>
  Collapsed (OldName m) tm (Expression (OldName m) tm) ->
  m (Collapsed (NewName m) tm (Expression (NewName m) tm))
toCollapsed Collapsed {..} = do
  compact <- toGetVar _pCompact
  fullExpression <- toExpression _pFullExpression
  pure Collapsed { _pCompact = compact, _pFullExpression = fullExpression, .. }

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

toLabeledApply ::
  (MonadNaming m, MonadA tm) =>
  LabeledApply (OldName m) (Expression (OldName m) tm) ->
  m (LabeledApply (NewName m) (Expression (NewName m) tm))
toLabeledApply la@LabeledApply{..} = do
  func <- toExpression _laFunc
  args <- traverse (Lens._1 toTag <=< Lens._2 toExpression) _laArgs
  pure la
    { _laFunc = func
    , _laArgs = args
    }

traverseToExpr ::
  (MonadA tm, MonadNaming m, Traversable t) =>
  (t (Expression (NewName m) tm) -> b) -> t (Expression (OldName m) tm) ->
  m b
traverseToExpr cons body = cons <$> traverse toExpression body

toBody ::
  (MonadA tm, MonadNaming m) =>
  Body (OldName m) tm (Expression (OldName m) tm) ->
  m (Body (NewName m) tm (Expression (NewName m) tm))
toBody (BodyApply hp x)       = traverseToExpr (BodyApply hp) x
toBody (BodySection hp x)     = traverseToExpr (BodySection hp) x
toBody (BodyList x)           = traverseToExpr BodyList x
toBody (BodyRecord x)         = traverseToExpr BodyRecord x
toBody (BodyGetField x)       = traverseToExpr BodyGetField x
toBody (BodyLiteralInteger x) = pure $ BodyLiteralInteger x
toBody (BodyAtom x)           = pure $ BodyAtom x
--
toBody (BodyLam hp x) = BodyLam hp <$> toLam x
toBody (BodyLabeledApply x) = BodyLabeledApply <$> toLabeledApply x
toBody (BodyHole x) = BodyHole <$> toHole x
toBody (BodyInferred x) = BodyInferred <$> toInferred x
toBody (BodyCollapsed x) = BodyCollapsed <$> toCollapsed x
toBody (BodyTag x) = BodyTag <$> toTag x
toBody (BodyGetVar x) = BodyGetVar <$> toGetVar x
toBody (BodyGetParams x) = BodyGetParams <$> toGetParams x

toPayload ::
  (MonadA tm, MonadNaming m) => Payload (OldName m) tm ->
  m (Payload (NewName m) tm)
toPayload pl@Payload {..} = do
  inferredTypes <- traverse toExpression _plInferredTypes
  nextHole <- traverse toExpression _plNextHole
  pure pl
    { _plInferredTypes = inferredTypes
    , _plNextHole = nextHole
    }

toExpression ::
  (MonadA tm, MonadNaming m) => Expression (OldName m) tm ->
  m (Expression (NewName m) tm)
toExpression expr@Expression{..} = do
  body <- toBody _rBody
  pl <- toPayload _rPayload
  pure expr { _rBody = body, _rPayload = pl }

toFuncParamActions ::
  (MonadNaming m, MonadA tm) => FuncParamActions (OldName m) tm ->
  m (FuncParamActions (NewName m) tm)
toFuncParamActions fpa = do
  run <- opRun
  pure $
    fpa & fpGetExample . Lens.mapped %~ run . toExpression

withWhereItem ::
  (MonadA tm, MonadNaming m) => WhereItem (OldName m) tm ->
  CPS m (WhereItem (NewName m) tm)
withWhereItem item@WhereItem{..} = CPS $ \k -> do
  (name, (value, res)) <-
    runCPS (opWithWhereItemName wiGuid wiName) $
    (,) <$> toDefinitionContent wiValue <*> k
  pure (item { wiValue = value, wiName = name }, res)

toDefinitionContent ::
  (MonadA tm, MonadNaming m) => DefinitionContent (OldName m) tm ->
  m (DefinitionContent (NewName m) tm)
toDefinitionContent def@DefinitionContent{..} = do
  (depParams, (params, (whereItems, body))) <-
    runCPS (traverse (withFuncParam NameGen.Dependent) dDepParams) .
    runCPS (traverse (withFuncParam NameGen.Independent) dParams) .
    runCPS (traverse withWhereItem dWhereItems) $
    toExpression dBody
  pure def
    { dDepParams = depParams
    , dParams = params
    , dBody = body
    , dWhereItems = whereItems
    }

toDefinitionNewType ::
  (MonadA tm, MonadNaming m) => DefinitionNewType (OldName m) tm ->
  m (DefinitionNewType (NewName m) tm)
toDefinitionNewType dnt@DefinitionNewType{..} = do
  newType <- toExpression dntNewType
  pure dnt { dntNewType = newType }

toDefinitionBody ::
  (MonadA tm, MonadNaming m) => DefinitionBody (OldName m) tm ->
  m (DefinitionBody (NewName m) tm)
toDefinitionBody (DefinitionBodyBuiltin bi) =
  pure $ DefinitionBodyBuiltin bi
toDefinitionBody
  (DefinitionBodyExpression
   def@DefinitionExpression {..}) =
    DefinitionBodyExpression <$> do
      content <- toDefinitionContent _deContent
      mNewType <- traverse toDefinitionNewType _deMNewType
      pure def
        { _deContent = content
        , _deMNewType = mNewType
        }

toDef ::
  (MonadA tm, MonadNaming m) => Definition (OldName m) tm ->
  m (Definition (NewName m) tm)
toDef def@Definition {..} = do
  (name, (typ, body)) <-
    runCPS (opWithDefName _drGuid _drName) $
    (,) <$> toExpression _drType <*> toDefinitionBody _drBody
  pure def { _drName = name, _drType = typ, _drBody = body }

addToDef :: MonadA m => DefinitionU m -> DefinitionN m
addToDef =
  pass1 . runPass0M . toDef
  where
    emptyP1Env = P1Env
      { _p1NameGen = NameGen.initial
      , _p1StoredNames = mempty
      , _p1StoredNameSuffixes = mempty
      }
    pass1 (def, _) =
      runPass1M emptyP1Env $ toDef def
