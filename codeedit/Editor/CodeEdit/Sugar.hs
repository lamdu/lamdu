{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}

module Editor.CodeEdit.Sugar
  ( DefinitionRef(..)
  , DefinitionNewType(..)
  , Builtin(..)
  , Actions(..)
  , Expression(..), ExpressionRef(..)
  , Where(..), WhereItem(..)
  , Func(..), FuncParam(..), FuncParamActions(..)
  , Pi(..), Apply(..), Section(..), Hole(..)
  , LiteralInteger(..), Inferred(..)
  , GetVariable(..), gvGuid
  , HasParens(..)
  , convertDefinition
  , convertExpression, convertExpressionPure
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow (second, (&&&))
import Control.Lens ((^.))
import Control.Monad (join, guard, liftM, mzero)
import Control.Monad.ListT (ListT)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Functor.Identity (Identity(..))
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid (mempty)
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import System.Random (RandomGen)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.Binary.Utils as BinaryUtils
import qualified Data.List.Class as List
import qualified Data.Map as Map
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Anchors as Anchors
import qualified Editor.CodeEdit.Infix as Infix
import qualified Editor.Data as Data
import qualified Editor.Data.Ops as DataOps
import qualified Editor.Data.Typed as DataTyped
import qualified System.Random as Random

type T = Transaction ViewTag

data Actions m = Actions
  { addNextArg   :: T m Guid
  , giveAsArg    :: T m Guid
  , callWithArg  :: T m Guid
  , lambdaWrap   :: T m Guid
  , addWhereItem :: T m Guid
  , replace      :: T m Guid
  , cut          :: T m Guid
  , mNextArg     :: Maybe (ExpressionRef m)
  }

data HasParens = HaveParens | DontHaveParens

data ExpressionRef m = ExpressionRef
  { rExpression :: Expression m
  , rInferredTypes :: [ExpressionRef m]
  , rGuid :: Guid
  , rActions :: Maybe (Actions m)
  }

data WhereItem m = WhereItem
  { wiGuid :: Guid
  , wiTypeGuid :: Guid
  , wiMDelete :: Maybe (T m Guid)
  , wiValue :: ExpressionRef m
  }

data Where m = Where
  { wWheres :: [WhereItem m]
  , wBody :: ExpressionRef m
  }

data FuncParamActions m = FuncParamActions
  { fpaAddNextParam :: T m Guid
  , fpaDelete :: T m Guid
  }

data FuncParam m = FuncParam
  { fpGuid :: Guid
  , fpType :: ExpressionRef m
  , fpMActions :: Maybe (FuncParamActions m)
  }

-- Multi-param Lambda
data Func m = Func
  { fParams :: [FuncParam m]
  , fBody :: ExpressionRef m
  }

data Pi m = Pi
  { pParam :: FuncParam m
  , pResultType :: ExpressionRef m
  }

data Apply m = Apply
  { applyFunc :: ExpressionRef m
  , applyArg :: ExpressionRef m
  }

-- Infix Sections include: (+), (1+), (+1), (1+2). Last is really just
-- infix application, but considered an infix section too.
data Section m = Section
  { sectionLArg :: Maybe (ExpressionRef m)
  , sectionOp :: ExpressionRef m -- Always a GetVariable
  , sectionRArg :: Maybe (ExpressionRef m)
  , sectionInnerApplyGuid :: Maybe Guid
  }

data Hole m = Hole
  { holeScope :: [(Guid, Data.VariableRef)]
  , holePickResult :: Maybe (Data.PureGuidExpression -> T m Guid)
  , holePaste :: Maybe (T m Guid)
  , holeInferResults :: Data.PureGuidExpression -> ListT (T m) Data.PureGuidExpression
  }

data LiteralInteger m = LiteralInteger
  { liValue :: Integer
  , liSetValue :: Maybe (Integer -> T m ())
  }

data Inferred m = Inferred
  { iValue :: ExpressionRef m
  , iHole :: Hole m
  }

data GetVariable
  = GetParameter Guid | GetDefinition Data.DefinitionIRef

gvGuid :: GetVariable -> Guid
gvGuid (GetParameter g) = g
gvGuid (GetDefinition defI) = IRef.guid defI

data Expression m
  = ExpressionApply   { eHasParens :: HasParens, eApply :: Apply m }
  | ExpressionSection { eHasParens :: HasParens, eSection :: Section m }
  | ExpressionWhere   { eHasParens :: HasParens, eWhere :: Where m }
  | ExpressionFunc    { eHasParens :: HasParens, eFunc :: Func m }
  | ExpressionPi      { eHasParens :: HasParens, ePi :: Pi m }
  | ExpressionGetVariable { _getVariable :: GetVariable }
  | ExpressionHole { eHole :: Hole m }
  | ExpressionInferred { eInferred :: Inferred m }
  | ExpressionLiteralInteger { _eLit :: LiteralInteger m }
  | ExpressionBuiltin { eBuiltin :: Builtin m }

data Builtin m = Builtin
  { biName :: Data.FFIName
  , biSetFFIName :: Maybe (Data.FFIName -> T m ())
  }

data DefinitionNewType m = DefinitionNewType
  { dntNewType :: ExpressionRef m
  , dntAcceptNewType :: T m ()
  }

data DefinitionRef m = DefinitionRef
  { drGuid :: Guid
    -- TODO: This is the opposite order of the data model, reverse
    -- either of them:
  , drBody :: ExpressionRef m
  , drType :: ExpressionRef m
  , drIsTypeRedundant :: Bool
  , drMNewType :: Maybe (DefinitionNewType m)
  }

AtFieldTH.make ''Hole
AtFieldTH.make ''Where
AtFieldTH.make ''FuncParam
AtFieldTH.make ''Func
AtFieldTH.make ''ExpressionRef
AtFieldTH.make ''Apply
AtFieldTH.make ''Section
AtFieldTH.make ''Expression

AtFieldTH.make ''Actions
AtFieldTH.make ''FuncParamActions

data Loopable a = Loop | NonLoop a

data ExprEntityStored m = ExprEntityStored
  { eesProp :: Data.ExpressionIRefProperty (T m)
  , eeInferredTypes :: DataTyped.Ref -- TODO: name
  , eeInferredValues :: DataTyped.Ref
  }

data ExprEntity m = ExprEntity
  { eeGuid :: Guid
  , eeStored :: Maybe (ExprEntityStored m)
  , eeValue :: Loopable (Data.Expression (ExprEntity m))
  }

eeProp :: ExprEntity m -> Maybe (Data.ExpressionIRefProperty (T m))
eeProp = fmap eesProp . eeStored

eeFromITL :: DataTyped.LoopGuidExpression -> ExprEntity m
eeFromITL (DataTyped.Loop itGuid) =
  ExprEntity itGuid Nothing Loop
eeFromITL
  (DataTyped.NoLoop (Data.GuidExpression itGuid val)) =
    ExprEntity itGuid Nothing . NonLoop $
    fmap eeFromITL val

eeFromPure :: Data.PureGuidExpression -> ExprEntity m
eeFromPure (Data.PureGuidExpression (Data.GuidExpression g expr)) =
  ExprEntity g Nothing . NonLoop $ fmap eeFromPure expr

argument :: (a -> b) -> (b -> c) -> a -> c
argument = flip (.)

writeIRef
  :: Monad m => Data.ExpressionIRefProperty (T m)
  -> Data.Expression Data.ExpressionIRef
  -> Transaction t m ()
writeIRef = Data.writeExprIRef . Property.value

writeIRefVia
  :: Monad m
  => (a -> Data.ExpressionI)
  -> Data.ExpressionIRefProperty (T m)
  -> a -> Transaction t m ()
writeIRefVia f = (fmap . argument) f writeIRef

eeFromTypedExpression :: DataTyped.StoredExpression (T m) -> ExprEntity m
eeFromTypedExpression =
  runIdentity . Data.mapMExpression f
  where
    f e =
      ( return $ e ^. DataTyped.eeValue
      , return .
        ExprEntity (e ^. DataTyped.eeGuid)
        (Just
         (ExprEntityStored
          (e ^. DataTyped.eeRef)
          (e ^. DataTyped.eeInferredType)
          (e ^. DataTyped.eeInferredValue))) .
        NonLoop
      )

data SugarContext m = SugarContext
  { scScope :: DataTyped.Scope
  , scDef :: Maybe (DataTyped.StoredDefinition (T m))
  , scBuiltinsMap :: Anchors.BuiltinsMap
  }
AtFieldTH.make ''SugarContext

newtype Sugar m a = Sugar {
  unSugar :: ReaderT (SugarContext m) (T m) a
  } deriving (Monad)
AtFieldTH.make ''Sugar

runSugar :: Monad m => Maybe (DataTyped.StoredDefinition (T m)) -> Sugar m a -> T m a
runSugar def (Sugar action) = do
  builtinsMap <- Anchors.getP Anchors.builtinsMap
  runReaderT action SugarContext
    { scScope = Map.empty
    , scDef = def
    , scBuiltinsMap = builtinsMap
    }

putInScope :: Monad m => DataTyped.Ref -> Guid -> Sugar m a -> Sugar m a
putInScope typeRef x = atSugar . Reader.local . atScScope $ Map.insert x typeRef

readScope :: Monad m => Sugar m DataTyped.Scope
readScope = Sugar $ Reader.asks scScope

readDefinition :: Monad m => Sugar m (Maybe (DataTyped.StoredDefinition (T m)))
readDefinition = Sugar $ Reader.asks scDef

readBuiltinsMap :: Monad m => Sugar m Anchors.BuiltinsMap
readBuiltinsMap = Sugar $ Reader.asks scBuiltinsMap

liftTransaction :: Monad m => T m a -> Sugar m a
liftTransaction = Sugar . lift

type Convertor m = ExprEntity m -> Sugar m (ExpressionRef m)

mkCutter :: Monad m => Data.ExpressionIRef -> T m Guid -> T m Guid
mkCutter iref replaceWithHole = do
  Anchors.modP Anchors.clipboards (iref:)
  replaceWithHole

lambdaGuidToParamGuid :: Guid -> Guid
lambdaGuidToParamGuid = Guid.combine $ Guid.fromString "param"

mkActions :: Monad m => Data.ExpressionIRefProperty (T m) -> Actions m
mkActions stored =
  Actions
  { addNextArg = guidify $ DataOps.callWithArg stored
  , callWithArg = guidify $ DataOps.callWithArg stored
  , giveAsArg = guidify $ DataOps.giveAsArg stored
  , lambdaWrap = paramGuidify $ DataOps.lambdaWrap stored
  , addWhereItem = paramGuidify $ DataOps.redexWrap stored
  , replace = doReplace
  , cut = mkCutter (Property.value stored) doReplace
  , mNextArg = Nothing
  }
  where
    paramGuidify = liftM lambdaGuidToParamGuid . guidify
    guidify = liftM Data.exprIRefGuid
    doReplace = guidify $ DataOps.replaceWithHole stored

derefRef
  :: (Monad m, RandomGen g)
  => Sugar m (g -> DataTyped.Ref -> [DataTyped.LoopGuidExpression])
derefRef = do
  builtinsMap <- readBuiltinsMap
  mDefinition <- readDefinition
  let f def gen = DataTyped.derefRef gen builtinsMap $ def ^. DataTyped.deFinalTypeContext
  return $ maybe ((const . const) []) f mDefinition

mkExpressionRef
  :: Monad m
  => ExprEntity m
  -> Expression m -> Sugar m (ExpressionRef m)
mkExpressionRef ee expr = do
  deref <- derefRef
  let types = maybe [] (deref gen . eeInferredTypes) $ eeStored ee
  inferredTypesRefs <-
    mapM (convertExpressionI . eeFromITL) types
  return
    ExpressionRef
    { rExpression = expr
    , rInferredTypes = inferredTypesRefs
    , rGuid = eeGuid ee
    , rActions = fmap mkActions $ eeProp ee
    }
  where
    gen =
      Random.mkStdGen . (*2) . BinaryUtils.decodeS . Guid.bs $
      eeGuid ee

mkDelete
  :: Monad m
  => Data.ExpressionIRefProperty (T m)
  -> Data.ExpressionIRefProperty (T m)
  -> T m Guid
mkDelete parentP replacerP = do
  Property.set parentP replacerI
  return $ Data.exprIRefGuid replacerI
  where
    replacerI = Property.value replacerP

mkFuncParamActions
  :: Monad m
  => Data.ExpressionIRefProperty (T m)
  -> Data.ExpressionIRefProperty (T m)
  -> Actions m
  -> FuncParamActions m
mkFuncParamActions parentP replacerP bodyActions = FuncParamActions
  { fpaDelete = mkDelete parentP replacerP
  , fpaAddNextParam = lambdaWrap bodyActions
  }

convertLambdaParam
  :: Monad m
  => Data.Lambda (ExprEntity m)
  -> ExpressionRef m
  -> ExprEntity m -> Sugar m (FuncParam m)
convertLambdaParam (Data.Lambda paramTypeI bodyI) bodyRef exprI = do
  typeExpr <- convertExpressionI paramTypeI
  return FuncParam
    { fpGuid = lambdaGuidToParamGuid $ eeGuid exprI
    , fpType = typeExpr
    , fpMActions =
      mkFuncParamActions <$>
      eeProp exprI <*>
      eeProp bodyI <*>
      rActions bodyRef
    }

convertLambdaBody
  :: Monad m
  => Data.Lambda (ExprEntity m)
  -> Convertor m
convertLambdaBody (Data.Lambda paramTypeI bodyI) exprI =
  enhanceScope $ convertExpressionI bodyI
  where
    enhanceScope =
      case eeStored paramTypeI of
      Nothing -> id
      Just paramTypeStored ->
        putInScope (eeInferredValues paramTypeStored) $ eeGuid exprI

convertLambda
  :: Monad m
  => Data.Lambda (ExprEntity m)
  -> ExprEntity m -> Sugar m (FuncParam m, ExpressionRef m)
convertLambda lambda exprI = do
  sBody <- convertLambdaBody lambda exprI
  param <- convertLambdaParam lambda sBody exprI
  return (param, sBody)

convertFunc
  :: Monad m
  => Data.Lambda (ExprEntity m)
  -> Convertor m
convertFunc lambda exprI = do
  (param, sBody) <- convertLambda lambda exprI
  mkExpressionRef exprI .
    ExpressionFunc DontHaveParens $
    case rExpression sBody of
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
  mkExpressionRef exprI $ ExpressionPi DontHaveParens
    Pi
    { pParam = atFpType addApplyChildParens param
    , pResultType = sBody
    }

convertWhere
  :: Monad m
  => ExpressionRef m
  -> ExprEntity m
  -> Data.Lambda (ExprEntity m)
  -> Convertor m
convertWhere valueRef lambdaI lambda@(Data.Lambda typeI bodyI) applyI = do
  sBody <- convertLambdaBody lambda lambdaI
  mkExpressionRef applyI .
    ExpressionWhere DontHaveParens . atWWheres (item :) $
    case rExpression sBody of
      ExpressionWhere _ x -> x
      _ -> Where [] sBody
  where
    item = WhereItem
      { wiGuid = lambdaGuidToParamGuid (eeGuid lambdaI)
      , wiTypeGuid = eeGuid typeI
      , wiMDelete = mkDelete <$> eeProp applyI <*> eeProp bodyI
      , wiValue = valueRef
      }

addParens :: Expression m -> Expression m
addParens (ExpressionInferred (Inferred val hole)) =
  ExpressionInferred $ Inferred (atRExpression addParens val) hole
addParens x = (atEHasParens . const) HaveParens x

addApplyChildParens :: ExpressionRef m -> ExpressionRef m
addApplyChildParens =
  atRExpression f
  where
    f x@(ExpressionApply _ _) = x
    f x = addParens x

infixOp :: Monad m => ExprEntity m -> T m (Maybe Data.VariableRef)
infixOp ExprEntity { eeValue = NonLoop x } = Infix.infixOp x
infixOp _ = return Nothing

convertApply
  :: Monad m
  => Data.Apply (ExprEntity m)
  -> Convertor m
convertApply apply@(Data.Apply funcI argI) exprI =
  case eeValue funcI of
    NonLoop (Data.ExpressionLambda lambda@(
      Data.Lambda (ExprEntity { eeValue = NonLoop Data.ExpressionHole }) _)) -> do
      valueRef <- convertExpressionI argI
      -- TODO: Should we pass the lambda with the hole in its type,
      -- and not just the body?
      convertWhere valueRef funcI lambda exprI
    -- InfixR or ordinary prefix:
    NonLoop (Data.ExpressionApply funcApply@(Data.Apply funcFuncI _)) -> do
      mInfixOp <- liftTransaction $ infixOp funcFuncI
      case mInfixOp of
        Just op -> convertApplyInfixFull funcApply op apply exprI
        Nothing -> prefixApply
    -- InfixL or ordinary prefix:
    _ -> do
      mInfixOp <- liftTransaction $ infixOp funcI
      case mInfixOp of
        Just op -> convertApplyInfixL op apply exprI
        Nothing -> prefixApply
  where
    prefixApply = convertApplyPrefix apply exprI

setAddArg :: Monad m => ExprEntity m -> ExpressionRef m -> ExpressionRef m
setAddArg exprI =
  maybe id f $ eeProp exprI
  where
    f stored =
      atRActions . fmap . atAddNextArg . const .
      liftM Data.exprIRefGuid $ DataOps.callWithArg stored

atFunctionType :: ExpressionRef m -> ExpressionRef m
atFunctionType exprRef =
  case rExpression exprRef of
    ExpressionHole {} -> exprRef -- Keep types on holes
    _ -> atRInferredTypes removeIfOnlyPis exprRef
  where
    removeIfOnlyPis xs
      | all (isPi . rExpression) xs = []
      | otherwise = xs
    isPi (ExpressionPi {}) = True
    isPi _ = False

mkExpressionGetVariable :: Data.VariableRef -> Expression m
mkExpressionGetVariable = ExpressionGetVariable . mkGetVariable
  where
    mkGetVariable (Data.ParameterRef lambdaGuid) =
      GetParameter $ lambdaGuidToParamGuid lambdaGuid
    mkGetVariable (Data.DefinitionRef defI) =
      GetDefinition defI

convertApplyInfixFull
  :: Monad m
  => Data.Apply (ExprEntity m)
  -> Data.VariableRef
  -> Data.Apply (ExprEntity m)
  -> Convertor m
convertApplyInfixFull
  (Data.Apply funcFuncI funcArgI) op (Data.Apply funcI argI) exprI
  = do
    rArgRef <- convertExpressionI argI
    lArgRef <- convertExpressionI funcArgI
    opRef <- mkExpressionRef funcFuncI $ mkExpressionGetVariable op
    let
      newLArgRef = addApplyChildParens lArgRef
      newRArgRef = addApplyChildParens rArgRef
      newOpRef = atFunctionType $ setAddArg exprI opRef
    mkExpressionRef exprI . ExpressionSection DontHaveParens .
      Section (Just newLArgRef) newOpRef (Just newRArgRef) . Just $
      eeGuid funcI

convertApplyInfixL
  :: Monad m
  => Data.VariableRef
  -> Data.Apply (ExprEntity m)
  -> Convertor m
convertApplyInfixL op (Data.Apply opI argI) exprI = do
  argRef <- convertExpressionI argI
  let newArgRef = addApplyChildParens argRef
  opRef <- mkExpressionRef opI $ mkExpressionGetVariable op
  let
    newOpRef =
      atFunctionType .
      setAddArg exprI $
      opRef
  mkExpressionRef exprI . ExpressionSection HaveParens $
    Section (Just newArgRef) newOpRef Nothing Nothing

convertApplyPrefix
  :: Monad m
  => Data.Apply (ExprEntity m)
  -> Convertor m
convertApplyPrefix (Data.Apply funcI argI) exprI = do
  argRef <- convertExpressionI argI
  funcRef <- convertExpressionI funcI
  let
    newArgRef =
      setAddArg exprI $
      atRExpression addParens argRef
    setNextArg = atRActions . fmap . atMNextArg . const $ Just newArgRef
    newFuncRef =
      setNextArg .
      addApplyChildParens .
      atFunctionType .
      (atRExpression . atEApply . atApplyArg) setNextArg .
      (atRExpression . atESection . atSectionOp) setNextArg $
      funcRef
  mkExpressionRef exprI . ExpressionApply DontHaveParens $
    Apply newFuncRef newArgRef

convertGetVariable :: Monad m => Data.VariableRef -> Convertor m
convertGetVariable varRef exprI = do
  isInfix <- liftTransaction $ Infix.isInfixVar varRef
  getVarExpr <-
    mkExpressionRef exprI $
    mkExpressionGetVariable varRef
  if isInfix
    then
      mkExpressionRef exprI .
      ExpressionSection HaveParens $
      Section Nothing ((atRInferredTypes . const) [] getVarExpr) Nothing Nothing
    else return getVarExpr

mkPaste :: Monad m => Data.ExpressionIRefProperty (T m) -> Sugar m (Maybe (T m Guid))
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
      return $ Data.exprIRefGuid clip

zeroGuid :: Guid
zeroGuid = Guid.fromString "applyZero"

pureHole :: Data.PureGuidExpression
pureHole =
  Data.PureGuidExpression $
  Data.GuidExpression zeroGuid Data.ExpressionHole

fromInferred
  :: [DataTyped.LoopGuidExpression] -> Maybe Data.PureGuidExpression
fromInferred [] = Just pureHole
fromInferred [x] = DataTyped.pureGuidFromLoop x
fromInferred _ = Nothing

countPis :: Data.PureGuidExpression -> Int
countPis (Data.PureGuidExpression (Data.GuidExpression _ expr)) =
  case expr of
  Data.ExpressionPi (Data.Lambda _ resultType) -> 1 + countPis resultType
  _ -> 0

expandHoles
  :: (DataTyped.Ref -> Maybe Data.PureGuidExpression)
  -> DataTyped.Expression s -> Data.PureGuidExpression
expandHoles deref =
  runIdentity . Data.mapMExpression f
  where
    f expr =
      ( Identity $ expr ^. DataTyped.eeValue
      , \newVal ->
        Identity $ case newVal of
        Data.ExpressionHole -> fromMaybe pureHole . deref $ expr ^. DataTyped.eeInferredValue
        _ -> Data.PureGuidExpression $ Data.GuidExpression (expr ^. DataTyped.eeGuid) newVal
      )

applyForms
  :: Data.PureGuidExpression
  -> Data.PureGuidExpression -> [Data.PureGuidExpression]
applyForms _ e@(
  Data.PureGuidExpression (Data.GuidExpression _ (
  Data.ExpressionLambda _))) =
  [e]
applyForms exprType expr =
  take (1 + countPis exprType) $ iterate addApply expr
  where
    addApply =
      Data.PureGuidExpression . Data.GuidExpression zeroGuid .
      (`Data.makeApply` pureHole)

inferResults ::
  (Monad m, Monad f) =>
  Anchors.BuiltinsMap ->
  DataTyped.Scope ->
  Maybe (DataTyped.StoredDefinition (T f)) ->
  ExprEntityStored f ->
  Data.PureGuidExpression ->
  ListT (T m) Data.PureGuidExpression
inferResults builtinsMap scope mDef holeStored expr = List.joinL $ do
  mExprType <-
    liftM join . runMaybeT .
    liftM (uncurry ($) . second (Lens.view DataTyped.eeInferredType)) .
    runInfer $ DataTyped.pureInferExpressionWithinContext scope mDef expr
  return $ case mExprType of
    Nothing -> mempty
    Just exprType ->
      List.catMaybes . List.mapL mkResult . List.fromList $ applyForms exprType expr
  where
    mkResult applyExpr = runMaybeT $ do
      (derefIt, typedExpr) <- runInfer $ do
        typedExpr <-
          DataTyped.pureInferExpressionWithinContext
          scope mDef applyExpr
        DataTyped.unify (eeInferredTypes holeStored) $
          typedExpr ^. DataTyped.eeInferredType
        DataTyped.unify (eeInferredValues holeStored) $
          typedExpr ^. DataTyped.eeInferredValue
        return typedExpr
      _ <- MaybeT . return . derefIt $ typedExpr ^. DataTyped.eeInferredType
      return $ expandHoles derefIt typedExpr
    typeContext =
      maybe DataTyped.emptyTypeContext (Lens.view DataTyped.deTypeContext)
      mDef
    inferActions =
      Lens.set DataTyped.onConflict mzero $
      DataTyped.inferActions lift
    refPair =
      Lens.view DataTyped.deInferredType &&&
      Lens.view DataTyped.deRecursiveInferredType
    runInfer action = do
      (newTypeContext, x) <-
        DataTyped.resumeInfer inferActions typeContext action
      finalTypeContext <-
        DataTyped.finalizeTypeContext (fmap refPair mDef)
        inferActions newTypeContext
      let
        derefIt =
          fromInferred .
          DataTyped.derefRef (Random.mkStdGen 0) builtinsMap
          finalTypeContext
      return (derefIt, x)

convertHole :: Monad m => Convertor m
convertHole exprI = do
  mPaste <- maybe (return Nothing) mkPaste $ eeProp exprI
  scope <- readScope
  deref <- derefRef
  builtinsMap <- readBuiltinsMap
  mDef <- readDefinition
  let
    maybeInferredValues =
      map DataTyped.pureGuidFromLoop .
      maybe [] (deref gen . eeInferredValues) $ eeStored exprI
    gen =
      Random.mkStdGen . (+1) . (*2) . BinaryUtils.decodeS $ Guid.bs eGuid
    hole = Hole
      { holeScope =
        map (lambdaGuidToParamGuid &&& Data.ParameterRef) $
        Map.keys scope
      , holePickResult = fmap pickResult $ eeProp exprI
      , holePaste = mPaste
      , holeInferResults =
        (maybe . const) mempty
        (inferResults builtinsMap scope mDef) $ eeStored exprI
      }
  mkExpressionRef exprI =<<
    case maybeInferredValues of
    [Just x] ->
      liftM (ExpressionInferred . (`Inferred` hole)) .
      convertExpressionI $ eeFromPure x
    _ -> return $ ExpressionHole hole
  where
    eGuid = eeGuid exprI
    pickResult irefP result = do
      ~() <- Data.writeIRefExpressionFromPure (Property.value irefP) result
      return eGuid

convertLiteralInteger :: Monad m => Integer -> Convertor m
convertLiteralInteger i exprI =
  mkExpressionRef exprI . ExpressionLiteralInteger $
  LiteralInteger
  { liValue = i
  , liSetValue = fmap (writeIRefVia Data.ExpressionLiteralInteger) $ eeProp exprI
  }

convertBuiltin :: Monad m => Data.Builtin (ExprEntity m) -> Convertor m
convertBuiltin (Data.Builtin name t) exprI =
  mkExpressionRef exprI . ExpressionBuiltin $
  Builtin
  { biName = name
  , biSetFFIName = do
      tProp <- eeProp t
      fmap (writeIRefVia (Data.ExpressionBuiltin . (`Data.Builtin` Property.value tProp))) $ eeProp exprI
  }

fakeBuiltin :: [String] -> String -> Expression m
fakeBuiltin path name =
  ExpressionBuiltin
  Builtin
  { biName = Data.FFIName path name
  , biSetFFIName = Nothing
  }

convertSet :: Monad m => Convertor m
convertSet exprI =
  mkExpressionRef exprI $ fakeBuiltin ["Core"] "Set"

convertLoop :: Monad m => Convertor m
convertLoop ee = mkExpressionRef ee $ fakeBuiltin ["Loopy"] "Loop"

convertExpressionI :: Monad m => ExprEntity m -> Sugar m (ExpressionRef m)
convertExpressionI ee =
  ($ ee) $
  case eeValue ee of
    NonLoop x -> convert x
    Loop -> convertLoop
  where
    convert (Data.ExpressionLambda x) = convertFunc x
    convert (Data.ExpressionPi x) = convertPi x
    convert (Data.ExpressionApply x) = convertApply x
    convert (Data.ExpressionGetVariable x) = convertGetVariable x
    convert (Data.ExpressionLiteralInteger x) = convertLiteralInteger x
    convert (Data.ExpressionBuiltin x) = convertBuiltin x
    convert Data.ExpressionSet = convertSet
    convert Data.ExpressionHole = convertHole

-- Check no holes
isCompleteType :: Data.PureGuidExpression -> Bool
isCompleteType =
  isJust . Data.mapMExpression f
  where
    f (Data.PureGuidExpression (Data.GuidExpression g e)) =
      ( case e of
        Data.ExpressionHole -> Nothing
        _ -> Just e
      , Just . Data.PureGuidExpression . Data.GuidExpression g
      )

convertDefinitionI
  :: Monad m
  => DataTyped.StoredDefinition (T m)
  -> Sugar m (DefinitionRef m)
convertDefinitionI (DataTyped.StoredDefinition defI defInferredType _ (Data.Definition bodyI typeI) _ _) = do
  bodyS <- convertExpressionI bodyEntity
  typeS <- convertExpressionI $ eeFromTypedExpression typeI
  deref <- derefRef
  mInferredTypePure <- runMaybeT $ do
    inferredType <- toMaybeT . fromInferred $ deref gen defInferredType
    guard $ isCompleteType inferredType
    return inferredType
  defNewType <- runMaybeT $ do
    inferredTypePure <- toMaybeT mInferredTypePure
    let defType = DataTyped.toPureExpression typeI
    guard . not $ DataTyped.alphaEq inferredTypePure defType
    inferredTypeS <- lift . convertExpressionI $ eeFromPure inferredTypePure
    return DefinitionNewType
      { dntNewType = inferredTypeS
      , dntAcceptNewType =
        Transaction.writeIRef defI .
        (Data.Definition . Property.value . Lens.view DataTyped.eeRef) bodyI =<<
        Data.newIRefExpressionFromPure inferredTypePure
      }
  return DefinitionRef
    { drGuid = defGuid
    , drBody = bodyS
    , drType = typeS
    , drIsTypeRedundant = isJust mInferredTypePure
    , drMNewType = defNewType
    }
  where
    gen =
      Random.mkStdGen . BinaryUtils.decodeS . Guid.bs $
      IRef.guid defI
    bodyEntity = eeFromTypedExpression bodyI
    toMaybeT = MaybeT . return
    defGuid = IRef.guid defI

convertDefinition
  :: Monad m
  => DataTyped.StoredDefinition (T m) -> T m (DefinitionRef m)
convertDefinition def = runSugar (Just def) $ convertDefinitionI def

convertExpressionPure
  :: Monad m => Data.PureGuidExpression -> T m (ExpressionRef m)
convertExpressionPure = runSugar Nothing . convertExpressionI . eeFromPure

convertExpression
  :: Monad m
  => DataTyped.StoredExpression (T m) -> T m (ExpressionRef m)
convertExpression = runSugar Nothing . convertExpressionI . eeFromTypedExpression
