{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, DeriveFunctor #-}

module Editor.CodeEdit.Sugar
  ( Definition(..), DefinitionBody(..)
  , DefinitionExpression(..), DefinitionContent(..), DefinitionNewType(..)
  , DefinitionBuiltin(..)
  , Actions(..)
  , ExpressionBody(..)
  , Payload(..)
  , ExpressionP(..)
  , Expression
  , WhereItem(..)
  , Func(..), FuncParam(..), FuncParamActions(..)
  , Pi(..)
  , Section(..)
  , Hole(..), HoleResult
  , LiteralInteger(..)
  , Inferred(..)
  , Polymorphic(..)
  , GetVariable(..)
  , HasParens(..)
  , convertExpressionPure, convertHoleResult
  , loadConvertDefinition, loadConvertExpression
  , removeTypes
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow (second)
import Control.Monad ((<=<), liftM, mzero, void)
import Control.Monad.ListT (ListT)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.Writer (Writer, runWriter)
import Data.Function (on)
import Data.Map (Map)
import Data.Maybe (listToMaybe)
import Data.Monoid (Monoid(..), Any(..))
import Data.Set (Set)
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.Binary.Utils as BinaryUtils
import qualified Data.Foldable as Foldable
import qualified Data.List.Class as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Data.Traversable as Traversable
import qualified Editor.Anchors as Anchors
import qualified Editor.CodeEdit.Infix as Infix
import qualified Editor.Data as Data
import qualified Editor.Data.IRef as DataIRef
import qualified Editor.Data.Load as Load
import qualified Editor.Data.Ops as DataOps
import qualified Editor.Data.Infer as Infer
import qualified System.Random as Random
import qualified System.Random.Utils as RandomUtils

type T = Transaction ViewTag

data Actions m = Actions
  { addNextArg   :: T m Guid
  , giveAsArg    :: T m Guid
  , callWithArg  :: T m Guid
  , lambdaWrap   :: T m Guid
  , addWhereItem :: T m Guid
  , replace      :: T m Guid
  , cut          :: T m Guid
  }

data HasParens = HaveParens | DontHaveParens

data Payload m = Payload
  { plInferredTypes :: [Expression m]
  , plActions :: Maybe (Actions m)
  , plNextArg :: Maybe (Expression m)
  }

data ExpressionP m pl = Expression
  { rGuid :: Guid
  , rExpressionBody :: ExpressionBody m (ExpressionP m pl)
  , rPayload :: pl
  } deriving (Functor)

type Expression m = ExpressionP m (Payload m)

data FuncParamActions m = FuncParamActions
  { fpaAddNextParam :: T m Guid
  , fpaAddPrevParam :: T m Guid
  , fpaDelete :: T m Guid
  }

data FuncParam m expr = FuncParam
  { fpGuid :: Guid
  , fpHiddenLambdaGuid :: Maybe Guid
  , fpType :: expr
  , fpMActions :: Maybe (FuncParamActions m)
  } deriving (Functor)

-- Multi-param Lambda
data Func m expr = Func
  { fParams :: [FuncParam m expr]
  , fBody :: expr
  } deriving (Functor)

data Pi m expr = Pi
  { pParam :: FuncParam m expr
  , pResultType :: expr
  } deriving (Functor)

-- Infix Sections include: (+), (1+), (+1), (1+2). Last is really just
-- infix application, but considered an infix section too.
data Section expr = Section
  { sectionLArg :: Maybe expr
  , sectionOp :: expr -- TODO: Always a GetVariable, use a more specific type
  , sectionRArg :: Maybe expr
  } deriving (Functor)

type HoleResult = Infer.Expression ()

data Hole m = Hole
  { holeScope :: [(Guid, Data.VariableRef)]
  , holePickResult :: Maybe (HoleResult -> T m Guid)
  , holePaste :: Maybe (T m Guid)
  , holeInferResults :: Data.PureExpression -> ListT (T m) HoleResult
  }

data LiteralInteger m = LiteralInteger
  { liValue :: Integer
  , liSetValue :: Maybe (Integer -> T m ())
  }

data Inferred m expr = Inferred
  { iValue :: expr
  , iHole :: Hole m
  } deriving (Functor)

data Polymorphic expr = Polymorphic
  { pCompact :: Maybe expr
  , pFullExpression :: expr
  } deriving (Functor)

data GetVariable
  = GetParameter Guid | GetDefinition Data.DefinitionIRef

data ExpressionBody m expr
  = ExpressionApply   { eHasParens :: HasParens, eApply :: Data.Apply expr }
  | ExpressionSection { eHasParens :: HasParens, eSection :: Section expr }
  | ExpressionFunc    { eHasParens :: HasParens, _eFunc :: Func m expr }
  | ExpressionPi      { eHasParens :: HasParens, _ePi :: Pi m expr }
  | ExpressionGetVariable { _getVariable :: GetVariable }
  | ExpressionHole { _eHole :: Hole m }
  | ExpressionInferred { _eInferred :: Inferred m expr }
  | ExpressionPolymorphic { _ePolymorphic :: Polymorphic expr }
  | ExpressionLiteralInteger { _eLit :: LiteralInteger m }
  | ExpressionAtom { _eAtom :: String }
  deriving (Functor)

data DefinitionNewType m = DefinitionNewType
  { dntNewType :: Expression m
  , dntAcceptNewType :: T m ()
  }

data WhereItem m = WhereItem
  { wiValue :: DefinitionContent m
  , wiGuid :: Guid
  , wiHiddenGuids :: [Guid]
  , wiDelete :: T m Guid
  }

-- Common data for definitions and where-items
data DefinitionContent m = DefinitionContent
  { dBody :: Expression m
  , dParameters :: [FuncParam m (Expression m)]
  , dWhereItems :: [WhereItem m]
  }

data DefinitionExpression m = DefinitionExpression
  { deContent :: DefinitionContent m
  , deIsTypeRedundant :: Bool
  , deMNewType :: Maybe (DefinitionNewType m)
  }

data DefinitionBuiltin m = DefinitionBuiltin
  { biName :: Data.FFIName
  -- Consider removing Maybe'ness here
  , biMSetName :: Maybe (Data.FFIName -> T m ())
  }

data DefinitionBody m
  = DefinitionBodyExpression (DefinitionExpression m)
  | DefinitionBodyBuiltin (DefinitionBuiltin m)

data Definition m = Definition
  { drGuid :: Guid
  , drType :: Expression m
  , drBody :: DefinitionBody m
  }

AtFieldTH.make ''Hole
AtFieldTH.make ''WhereItem
AtFieldTH.make ''FuncParam
AtFieldTH.make ''Func
AtFieldTH.make ''Pi
AtFieldTH.make ''Section
AtFieldTH.make ''ExpressionBody
AtFieldTH.make ''Inferred
AtFieldTH.make ''Actions
AtFieldTH.make ''FuncParamActions
AtFieldTH.make ''Payload
AtFieldTH.make ''ExpressionP

data ExprEntityInferred a = ExprEntityInferred
  { eesInferred :: Infer.Inferred a
  , eesTypeConflicts :: [Data.PureExpression]
  , eesValueConflicts :: [Data.PureExpression]
  } deriving Functor

type ExprEntityStored m = ExprEntityInferred (DataIRef.ExpressionProperty (T m))
type ExprEntityMStored m = ExprEntityInferred (Maybe (DataIRef.ExpressionProperty (T m)))
type ExprEntity m = Data.Expression (Maybe (ExprEntityMStored m))

eeProp :: ExprEntity m -> Maybe (DataIRef.ExpressionProperty (T m))
eeProp = Infer.iStored . eesInferred <=< Data.ePayload

eeFromPure :: Data.PureExpression -> ExprEntity m
eeFromPure = fmap $ const Nothing

newtype ConflictMap =
  ConflictMap { unConflictMap :: Map Infer.Ref (Set Data.PureExpression) }

instance Monoid ConflictMap where
  mempty = ConflictMap mempty
  mappend (ConflictMap x) (ConflictMap y) =
    ConflictMap $ Map.unionWith mappend x y

getConflicts :: Infer.Ref -> ConflictMap -> [Data.PureExpression]
getConflicts ref = maybe [] Set.toList . Map.lookup ref . unConflictMap

argument :: (a -> b) -> (b -> c) -> a -> c
argument = flip (.)

writeIRef
  :: Monad m => DataIRef.ExpressionProperty (T m)
  -> Data.ExpressionBody Data.ExpressionIRef
  -> Transaction t m ()
writeIRef = DataIRef.writeExprBody . Property.value

writeIRefVia
  :: Monad m
  => (a -> DataIRef.ExpressionBody)
  -> DataIRef.ExpressionProperty (T m)
  -> a -> Transaction t m ()
writeIRefVia f = (fmap . argument) f writeIRef

newtype SugarContext = SugarContext
  { scInferState :: Infer.RefMap
  }
AtFieldTH.make ''SugarContext

newtype Sugar m a = Sugar {
  unSugar :: ReaderT SugarContext (T m) a
  } deriving (Monad)
AtFieldTH.make ''Sugar

runSugar :: Monad m => SugarContext -> Sugar m a -> T m a
runSugar ctx (Sugar action) = runReaderT action ctx

readContext :: Monad m => Sugar m SugarContext
readContext = Sugar Reader.ask

liftTransaction :: Monad m => T m a -> Sugar m a
liftTransaction = Sugar . lift

type Convertor m = ExprEntity m -> Sugar m (Expression m)

mkCutter :: Monad m => Data.ExpressionIRef -> T m Guid -> T m Guid
mkCutter iref replaceWithHole = do
  Anchors.modP Anchors.clipboards (iref:)
  replaceWithHole

lambdaGuidToParamGuid :: Guid -> Guid
lambdaGuidToParamGuid = Guid.combine $ Guid.fromString "param"

mkActions :: Monad m => DataIRef.ExpressionProperty (T m) -> Actions m
mkActions stored =
  Actions
  { addNextArg = guidify $ DataOps.callWithArg stored
  , callWithArg = guidify $ DataOps.callWithArg stored
  , giveAsArg = guidify $ DataOps.giveAsArg stored
  , lambdaWrap = paramGuidify $ DataOps.lambdaWrap stored
  , addWhereItem = paramGuidify $ DataOps.redexWrap stored
  , replace = doReplace
  , cut = mkCutter (Property.value stored) doReplace
  }
  where
    paramGuidify = liftM lambdaGuidToParamGuid . guidify
    guidify = liftM DataIRef.exprGuid
    doReplace = guidify $ DataOps.replaceWithHole stored

mkGen :: Int -> Int -> Guid -> Random.StdGen
mkGen select count =
  Random.mkStdGen . (+select) . (*count) . BinaryUtils.decodeS . Guid.bs

mkExpression ::
  Monad m =>
  ExprEntity m ->
  ExpressionBody m (Expression m) -> Sugar m (Expression m)
mkExpression ee expr = do
  inferredTypesRefs <- mapM (convertExpressionI . eeFromPure) types
  return
    Expression
    { rGuid = Data.eGuid ee
    , rExpressionBody = expr
    , rPayload = Payload
      { plInferredTypes = inferredTypesRefs
      , plActions = fmap mkActions $ eeProp ee
      , plNextArg = Nothing
      }
    }
  where
    types =
      zipWith Data.randomizeGuids
      (RandomUtils.splits (mkGen 0 2 (Data.eGuid ee))) .
      maybe [] eesInferredTypes $ Data.ePayload ee

mkDelete
  :: Monad m
  => DataIRef.ExpressionProperty (T m)
  -> DataIRef.ExpressionProperty (T m)
  -> T m Guid
mkDelete parentP replacerP = do
  Property.set parentP replacerI
  return $ DataIRef.exprGuid replacerI
  where
    replacerI = Property.value replacerP

mkFuncParamActions
  :: Monad m
  => DataIRef.ExpressionProperty (T m)
  -> DataIRef.ExpressionProperty (T m)
  -> FuncParamActions m
mkFuncParamActions parentP replacerP = FuncParamActions
  { fpaDelete = mkDelete parentP replacerP
  , fpaAddNextParam = addParam replacerP
  , fpaAddPrevParam = addParam parentP
  }
  where
    addParam =
      liftM (lambdaGuidToParamGuid . DataIRef.exprGuid) . DataOps.lambdaWrap

convertLambda
  :: Monad m
  => Data.Lambda (ExprEntity m)
  -> ExprEntity m -> Sugar m (FuncParam m (Expression m), Expression m)
convertLambda (Data.Lambda paramTypeI bodyI) exprI = do
  sBody <- convertExpressionI bodyI
  typeExpr <- convertExpressionI paramTypeI
  let
    param = FuncParam
      { fpGuid = lambdaGuidToParamGuid $ Data.eGuid exprI
      , fpHiddenLambdaGuid = Nothing
      , fpType = removeRedundantTypes typeExpr
      , fpMActions = mkFuncParamActions <$> eeProp exprI <*> eeProp bodyI
      }
  return (param, sBody)

convertFunc
  :: Monad m
  => Data.Lambda (ExprEntity m)
  -> Convertor m
convertFunc lambda exprI = do
  (param, sBody) <- convertLambda lambda exprI
  mkExpression exprI .
    ExpressionFunc DontHaveParens $
    case rExpressionBody sBody of
      ExpressionFunc _ (Func params body) ->
        Func (deleteToNextParam param : params) body
      _ -> Func [param] sBody
  where
    deleteToNextParam =
      atFpMActions . fmap . atFpaDelete . liftM $ lambdaGuidToParamGuid

convertPi
  :: Monad m
  => Data.Lambda (ExprEntity m)
  -> Convertor m
convertPi lambda exprI = do
  (param, sBody) <- convertLambda lambda exprI
  mkExpression exprI $ ExpressionPi DontHaveParens
    Pi
    { pParam = atFpType addApplyChildParens param
    , pResultType = removeRedundantTypes sBody
    }

addParens :: ExpressionBody m (Expression m) -> ExpressionBody m (Expression m)
addParens (ExpressionInferred (Inferred val hole)) =
  ExpressionInferred $ Inferred (atRExpressionBody addParens val) hole
addParens (ExpressionPolymorphic (Polymorphic compact full)) =
  ExpressionPolymorphic .
  Polymorphic ((fmap . atRExpressionBody) addParens compact) $
  atRExpressionBody addParens full
addParens x = (atEHasParens . const) HaveParens x

addApplyChildParens :: Expression m -> Expression m
addApplyChildParens =
  atRExpressionBody f
  where
    f x@ExpressionApply{} = x
    f x@ExpressionPolymorphic{} = x
    f x = addParens x

isPolymorphicFunc :: ExprEntity m -> Bool
isPolymorphicFunc funcI =
  maybe False
  (Data.isDependentPi . Infer.iType . eesInferred)
  (Data.ePayload funcI)

convertApply :: Monad m => Data.Apply (ExprEntity m) -> Convertor m
convertApply (Data.Apply funcI argI) exprI = do
  funcS <- convertExpressionI funcI
  argS <- convertExpressionI argI
  let apply = Data.Apply (funcS, funcI) (argS, argI)
  case rExpressionBody funcS of
    ExpressionSection _ section ->
      applyOnSection section apply exprI
    _ ->
      convertApplyPrefix apply exprI

setAddArg :: Monad m => ExprEntity m -> Expression m -> Expression m
setAddArg exprI =
  maybe id f $ eeProp exprI
  where
    f stored =
      atRPayload . atPlActions . fmap . atAddNextArg . const .
      liftM DataIRef.exprGuid $ DataOps.callWithArg stored

removeRedundantTypes :: Expression m -> Expression m
removeRedundantTypes exprRef =
  case rExpressionBody exprRef of
    ExpressionHole {} -> exprRef -- Keep types on holes
    _ -> (atRPayload . atPlInferredTypes) removeIfNoErrors exprRef
  where
    removeIfNoErrors [_] = []
    removeIfNoErrors xs = xs

mkExpressionGetVariable :: Data.VariableRef -> ExpressionBody m expr
mkExpressionGetVariable =
  ExpressionGetVariable . mkGetVariable
  where
    mkGetVariable (Data.ParameterRef lambdaGuid) =
      GetParameter $ lambdaGuidToParamGuid lambdaGuid
    mkGetVariable (Data.DefinitionRef defI) =
      GetDefinition defI

applyOnSection ::
  Monad m =>
  Section (Expression m) -> Data.Apply (Expression m, ExprEntity m) -> Convertor m
applyOnSection (Section Nothing op Nothing) (Data.Apply (_, funcI) arg@(argRef, _)) exprI
  | isPolymorphicFunc funcI = do
    newOpRef <-
      convertApplyPrefix (Data.Apply (op, funcI) arg) exprI
    mkExpression exprI . ExpressionSection DontHaveParens $
      Section Nothing (removeRedundantTypes newOpRef) Nothing
  | otherwise =
    mkExpression exprI . ExpressionSection DontHaveParens $
      Section (Just (addApplyChildParens argRef)) op Nothing
applyOnSection (Section (Just left) op Nothing) (Data.Apply _ (argRef, _)) exprI =
  mkExpression exprI . ExpressionSection DontHaveParens $
    Section (Just left) op (Just (addApplyChildParens argRef))
applyOnSection _ apply exprI = convertApplyPrefix apply exprI

convertApplyPrefix ::
  Monad m =>
  Data.Apply (Expression m, ExprEntity m) -> Convertor m
convertApplyPrefix (Data.Apply (funcRef, funcI) (argRef, argI)) exprI =
  if isPolymorphicFunc funcI
  then
    case rExpressionBody funcRef of
    ExpressionPolymorphic (Polymorphic compact full) ->
      makePolymorphic compact . removeRedundantTypes =<<
      (mkExpression exprI . ExpressionApply DontHaveParens) (Data.Apply full newArgRef)
    _ ->
      on (makePolymorphic . Just) removeRedundantTypes funcRef =<< makeFullApply
  else makeFullApply
  where
    newArgRef =
      setAddArg exprI $
      atRExpressionBody addParens argRef
    setNextArg = atRPayload . atPlNextArg . const $ Just newArgRef
    newFuncRef =
      setNextArg .
      addApplyChildParens .
      removeRedundantTypes .
      (atRExpressionBody . atEApply . Data.atApplyArg) setNextArg .
      (atRExpressionBody . atESection . atSectionOp) setNextArg $
      funcRef
    makeFullApply =
      mkExpression exprI . ExpressionApply DontHaveParens $
      Data.Apply newFuncRef newArgRef
    makePolymorphic x =
      (liftM . atRGuid . Guid.combine . Guid.fromString) "polymorphic" .
      mkExpression exprI . ExpressionPolymorphic . Polymorphic
      (if isHole (Data.eValue argI) then x else Nothing)

isHole :: Data.ExpressionBody a -> Bool
isHole (Data.ExpressionLeaf Data.Hole) = True
isHole _ = False

convertGetVariable :: Monad m => Data.VariableRef -> Convertor m
convertGetVariable varRef exprI = do
  isInfix <- liftTransaction $ Infix.isInfixVar varRef
  getVarExpr <-
    mkExpression exprI $
    mkExpressionGetVariable varRef
  if isInfix
    then
      mkExpression exprI .
      ExpressionSection HaveParens $
      Section Nothing ((atRPayload . atPlInferredTypes . const) [] getVarExpr) Nothing
    else return getVarExpr

mkPaste :: Monad m => DataIRef.ExpressionProperty (T m) -> Sugar m (Maybe (T m Guid))
mkPaste exprP = do
  clipboardsP <- liftTransaction Anchors.clipboards
  let
    mClipPop =
      case Property.value clipboardsP of
      [] -> Nothing
      (clip : clips) -> Just (clip, Property.set clipboardsP clips)
  return $ fmap (doPaste (Property.set exprP)) mClipPop
  where
    doPaste replacer (clip, popClip) = do
      ~() <- popClip
      ~() <- replacer clip
      return $ DataIRef.exprGuid clip

zeroGuid :: Guid
zeroGuid = Guid.fromString "applyZero"

pureHole :: Data.PureExpression
pureHole = Data.pureExpression zeroGuid $ Data.ExpressionLeaf Data.Hole

countPis :: Data.PureExpression -> Int
countPis e =
  case Data.eValue e of
  Data.ExpressionPi (Data.Lambda _ resultType) -> 1 + countPis resultType
  _ -> 0

applyForms
  :: Data.PureExpression
  -> Data.PureExpression -> [Data.PureExpression]
applyForms _ e@Data.Expression{ Data.eValue = Data.ExpressionLambda {} } =
  [e]
applyForms exprType expr =
  map Data.canonizeGuids . reverse . take (1 + countPis exprType) $ iterate addApply expr
  where
    addApply =
      Data.pureExpression zeroGuid .
      (`Data.makeApply` pureHole)

convertReadOnlyHole :: Monad m => Convertor m
convertReadOnlyHole exprI =
  mkExpression exprI $ ExpressionHole Hole
  { holeScope = []
  , holePickResult = Nothing
  , holePaste = Nothing
  , holeInferResults = mempty
  }

loader :: Monad m => Infer.Loader (T m)
loader = Infer.Loader Load.loadPureDefinitionType

-- Fill partial holes in an expression. Parital holes are those whose
-- inferred (filler) value itself is not complete, so will not be a
-- useful auto-inferred value. By auto-filling those, we allow the
-- user a chance to access all the partiality that needs filling more
-- easily.
fillPartialHolesInExpression :: Data.Expression (Infer.Inferred a) -> (Data.Expression (), Bool)
fillPartialHolesInExpression = second getAny . runWriter . fillHoleExpr
  where
    fillHoleExpr expr@(Data.Expression _ (Data.ExpressionLeaf Data.Hole) hInferred) =
      if isCompleteType $ Infer.iValue hInferred
      then return $ void expr
      else do
        -- Hole inferred value has holes to fill, no use leaving it as
        -- auto-inferred, just fill it:
        Writer.tell $ Any True
        return $ Infer.iValue hInferred
    fillHoleExpr (Data.Expression g body _) =
      liftM (Data.pureExpression g) $ Traversable.mapM fillHoleExpr body

convertWritableHole ::
  Monad m =>
  ExprEntityInferred (DataIRef.ExpressionProperty (T m)) -> Convertor m
convertWritableHole eeInferred exprI = do
  inferState <- liftM scInferState readContext
  let
    check expr =
      inferExpr expr inferState . Infer.iPoint $ eesInferred eeInferred

    recheck _ (filledExpr, True) = check filledExpr
    recheck iExpr (_, False) = return $ Just iExpr -- no holes filled
    fillPartialHoles Nothing = return Nothing
    fillPartialHoles (Just iExpr) = recheck iExpr $ fillPartialHolesInExpression iExpr

    makeApplyForms _ Nothing = mzero
    makeApplyForms expr (Just i) =
      List.catMaybes . List.mapL (fillPartialHoles <=< check) . List.fromList $
      applyForms (Infer.iType (Data.ePayload i)) expr

    inferResults expr =
      List.joinL . liftM (makeApplyForms expr) $
      uncurry (inferExpr expr) .
      Infer.newNodeWithScope
      ((Infer.nScope . Infer.iPoint . eesInferred) eeInferred) $
      inferState
  mPaste <- mkPaste . Infer.iStored $ eesInferred eeInferred
  let
    onScopeElement (lambdaGuid, _typeExpr) =
      (lambdaGuidToParamGuid lambdaGuid, Data.ParameterRef lambdaGuid)
    hole = Hole
      { holeScope = map onScopeElement . Map.toList . Infer.iScope $ eesInferred eeInferred
      , holePickResult = Just . pickResult . Infer.iStored $ eesInferred eeInferred
      , holePaste = mPaste
      , holeInferResults = inferResults
      }
  mkExpression exprI =<<
    case eesInferredValues eeInferred of
    [Data.Expression { Data.eValue = Data.ExpressionLeaf Data.Hole }] ->
      return $ ExpressionHole hole
    [x] ->
      liftM (ExpressionInferred . (`Inferred` hole)) .
      convertExpressionI . eeFromPure $ Data.randomizeGuids (mkGen 1 2 eGuid) x
    _ -> return $ ExpressionHole hole
  where
    inferExpr expr inferContext inferPoint =
      liftM (fmap fst . Infer.infer (Infer.InferActions (const Nothing))) $
      Infer.load loader inferContext inferPoint Nothing expr
    pickResult irefP =
      liftM (maybe eGuid Data.eGuid . listToMaybe . uninferredHoles) .
      DataIRef.writeExpression (Property.value irefP)
    eGuid = Data.eGuid exprI

-- TODO: This is a DRY violation, implementing isPolymorphic logic
-- here again

-- Also skip param types, those can usually be inferred later, so less
-- useful to fill immediately
uninferredHoles :: HoleResult -> [HoleResult]
uninferredHoles Data.Expression { Data.eValue = Data.ExpressionApply (Data.Apply func arg) } =
  if (Data.isDependentPi . Infer.iType . Data.ePayload) func
  then uninferredHoles func
  else uninferredHoles func ++ uninferredHoles arg
uninferredHoles e@Data.Expression { Data.eValue = Data.ExpressionLeaf Data.Hole } = [e]
uninferredHoles Data.Expression
  { Data.eValue = Data.ExpressionPi (Data.Lambda paramType resultType) } =
    uninferredHoles resultType ++ uninferredHoles paramType
uninferredHoles Data.Expression
  { Data.eValue = Data.ExpressionLambda (Data.Lambda paramType result) } =
    uninferredHoles result ++ uninferredHoles paramType
uninferredHoles Data.Expression { Data.eValue = body } =
  Foldable.concatMap uninferredHoles body

convertHole :: Monad m => Convertor m
convertHole exprI =
  maybe convertReadOnlyHole convertWritableHole mStored exprI
  where
    mStored = f =<< Data.ePayload exprI
    f entity = fmap (g entity) $ (Infer.iStored . eesInferred) entity
    g entity stored =
      (atEesInferred . fmap . const) stored entity
    atEesInferred j x = x { eesInferred = j $ eesInferred x }

convertLiteralInteger :: Monad m => Integer -> Convertor m
convertLiteralInteger i exprI =
  mkExpression exprI . ExpressionLiteralInteger $
  LiteralInteger
  { liValue = i
  , liSetValue =
      fmap (writeIRefVia (Data.ExpressionLeaf . Data.LiteralInteger)) $
      eeProp exprI
  }

convertAtom :: Monad m => String -> Convertor m
convertAtom name exprI =
  mkExpression exprI $ ExpressionAtom name

convertExpressionI :: Monad m => ExprEntity m -> Sugar m (Expression m)
convertExpressionI ee =
  ($ ee) $
  case Data.eValue ee of
  Data.ExpressionLambda x -> convertFunc x
  Data.ExpressionPi x -> convertPi x
  Data.ExpressionApply x -> convertApply x
  Data.ExpressionLeaf (Data.GetVariable x) -> convertGetVariable x
  Data.ExpressionLeaf (Data.LiteralInteger x) -> convertLiteralInteger x
  Data.ExpressionLeaf Data.Hole -> convertHole
  Data.ExpressionLeaf Data.Set -> convertAtom "Set"
  Data.ExpressionLeaf Data.IntegerType -> convertAtom "Int"

-- Check no holes
isCompleteType :: Data.PureExpression -> Bool
isCompleteType = not . any (isHole . Data.eValue) . Data.subExpressions

convertHoleResult ::
  Monad m => HoleResult -> T m (Expression m)
convertHoleResult =
  runSugar ctx . convertExpressionI . fmap toExprEntity
  where
    toExprEntity inferred =
      Just ExprEntityInferred
      { eesInferred = (fmap . const) Nothing inferred
      , eesTypeConflicts = []
      , eesValueConflicts = []
      }
    ctx = SugarContext $ error "pure expression doesnt have infer state"

convertExpressionPure ::
  Monad m => Data.PureExpression -> T m (Expression m)
convertExpressionPure =
  runSugar ctx . convertExpressionI . eeFromPure
  where
    ctx = SugarContext $ error "pure expression doesnt have infer state"

reportError :: Infer.Error -> Writer ConflictMap ()
reportError err =
  Writer.tell . ConflictMap .
  Map.singleton (Infer.errRef err) .
  Set.singleton . Data.canonizeGuids .
  snd $ Infer.errMismatch err

loadConvertExpression ::
  Monad m => DataIRef.ExpressionProperty (T m) -> T m (Expression m)
loadConvertExpression exprP =
  convertLoadedExpression Nothing =<< Load.loadExpression exprP

convertDefinitionParams ::
  Monad m =>
  SugarContext -> Data.Expression (ExprEntityStored m) ->
  T m ([FuncParam m (Expression m)], Data.Expression (ExprEntityStored m))
convertDefinitionParams ctx expr =
  case Data.eValue expr of
  Data.ExpressionLambda (Data.Lambda paramType body) -> do
    paramTypeS <- convertStoredExpression ctx paramType
    let
      param = FuncParam
        { fpGuid = lambdaGuidToParamGuid $ Data.eGuid expr
        , fpHiddenLambdaGuid = Just $ Data.eGuid expr
        , fpType = paramTypeS
        , fpMActions = Just $ mkFuncParamActions (prop expr) (prop body)
        }
    (nextParams, funcBody) <- convertDefinitionParams ctx body
    return (param : nextParams, funcBody)
  _ -> return ([], expr)
  where
    prop = Infer.iStored . eesInferred . Data.ePayload

convertWhereItems ::
  Monad m =>
  SugarContext -> Data.Expression (ExprEntityStored m) ->
  T m ([WhereItem m], Data.Expression (ExprEntityStored m))
convertWhereItems ctx
  topLevel@Data.Expression
  { Data.eValue = Data.ExpressionApply apply@Data.Apply
  { Data.applyFunc = Data.Expression
  { Data.eValue = Data.ExpressionLambda lambda@Data.Lambda
  { Data.lambdaParamType = Data.Expression
  { Data.eValue = Data.ExpressionLeaf Data.Hole
  }}}}} = do
    value <- convertDefinitionContent ctx $ Data.applyArg apply
    let
      body = Data.lambdaBody lambda
      item = WhereItem
        { wiValue = value
        , wiDelete = mkDelete (prop topLevel) (prop body)
        , wiGuid = lambdaGuidToParamGuid . Data.eGuid $ Data.applyFunc apply
        , wiHiddenGuids =
            map Data.eGuid
            [ topLevel
            , Data.lambdaParamType lambda
            ]
        }
    (nextItems, whereBody) <- convertWhereItems ctx body
    return (item : nextItems, whereBody)
  where
    prop = Infer.iStored . eesInferred . Data.ePayload
convertWhereItems _ expr = return ([], expr)

convertDefinitionContent ::
  Monad m =>
  SugarContext -> Data.Expression (ExprEntityStored m) ->
  T m (DefinitionContent m)
convertDefinitionContent sugarContext expr = do
  (params, funcBody) <- convertDefinitionParams sugarContext expr
  (whereItems, whereBody) <- convertWhereItems sugarContext funcBody
  bodyS <- convertStoredExpression sugarContext whereBody
  return DefinitionContent
    { dBody = bodyS
    , dParameters = params
    , dWhereItems = whereItems
    }

loadConvertDefinition ::
  Monad m => Data.DefinitionIRef -> T m (Definition m)
loadConvertDefinition defI = do
  Data.Definition defBody typeL <- Load.loadDefinition defI
  let typeP = void typeL
  body <-
    case defBody of
    Data.DefinitionBuiltin (Data.Builtin name) -> do
      let
        typeI = Property.value $ Data.ePayload typeL
        setName =
          Transaction.writeIRef defI . (`Data.Definition` typeI) .
          Data.DefinitionBuiltin . Data.Builtin
      -- TODO: If we want editable builtin types:
      -- typeS <- convertLoadedExpression Nothing typeL
      return $ DefinitionBodyBuiltin DefinitionBuiltin
        { biName = name
        , biMSetName = Just setName
        }
    Data.DefinitionExpression exprL -> do
      (isSuccess, sugarContext, exprStored) <-
        inferLoadedExpression (Just defI) exprL
      content <- convertDefinitionContent sugarContext exprStored
      let
        inferredTypeP =
          Infer.iType . eesInferred $ Data.ePayload exprStored
        typesMatch = on (==) Data.canonizeGuids typeP inferredTypeP
        mkNewType = do
          inferredTypeS <-
            convertExpressionPure $
            Data.randomizeGuids (mkGen 0 1 (IRef.guid defI)) inferredTypeP
          return DefinitionNewType
            { dntNewType = inferredTypeS
            , dntAcceptNewType =
              Property.set (Data.ePayload typeL) =<<
              DataIRef.newExpression inferredTypeP
            }
      mNewType <-
        if isSuccess && not typesMatch && isCompleteType inferredTypeP
        then liftM Just mkNewType
        else return Nothing
      return $ DefinitionBodyExpression DefinitionExpression
        { deContent = content
        , deMNewType = mNewType
        , deIsTypeRedundant = isSuccess && typesMatch
        }
  typeS <- convertExpressionPure typeP
  return Definition
    { drGuid = IRef.guid defI
    , drBody = body
    , drType = typeS
    }

inferLoadedExpression ::
  Monad f =>
  Maybe Data.DefinitionIRef ->
  Load.ExpressionEntity (T m) ->
  T f
  (Bool,
   SugarContext,
   Data.Expression (ExprEntityStored m))
inferLoadedExpression mDefI exprL = do
  loaded <- uncurry (Infer.load loader) Infer.initial mDefI exprL
  let
    ((exprInferred, inferContext), conflictsMap) =
      runWriter $ Infer.infer actions loaded
    toExprEntity x =
      ExprEntityInferred
      { eesInferred = x
      , eesValueConflicts = conflicts Infer.tvVal x
      , eesTypeConflicts = conflicts Infer.tvType x
      }
    conflicts getRef x =
      getConflicts ((getRef . Infer.nRefs . Infer.iPoint) x)
      conflictsMap
  return
    ( Map.null $ unConflictMap conflictsMap
    , SugarContext inferContext, fmap toExprEntity exprInferred
    )
  where
    actions = Infer.InferActions reportError

convertLoadedExpression ::
  Monad m =>
  Maybe Data.DefinitionIRef ->
  Data.Expression (DataIRef.ExpressionProperty (T m)) ->
  T m (Expression m)
convertLoadedExpression mDefI exprL = do
  (_, sugarContext, exprStored) <- inferLoadedExpression mDefI exprL
  convertStoredExpression sugarContext exprStored

convertStoredExpression ::
  Monad m =>
  SugarContext -> Data.Expression (ExprEntityStored m) ->
  T m (Expression m)
convertStoredExpression sugarContext =
  runSugar sugarContext . convertExpressionI . fmap (Just . fmap Just)

removeTypes :: Expression m -> Expression m
removeTypes =
  (atRPayload . atPlInferredTypes . const) [] .
  (atRExpressionBody . fmap) removeTypes

eesInferredExprs ::
  (Infer.Inferred a -> b) ->
  (ExprEntityInferred a -> [b]) ->
  ExprEntityInferred a -> [b]
eesInferredExprs getVal eeConflicts ee =
  getVal (eesInferred ee) : eeConflicts ee

eesInferredTypes :: ExprEntityInferred a -> [Data.PureExpression]
eesInferredTypes = eesInferredExprs Infer.iType eesTypeConflicts

eesInferredValues :: ExprEntityInferred a -> [Data.PureExpression]
eesInferredValues = eesInferredExprs Infer.iValue eesValueConflicts
