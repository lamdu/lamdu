{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}

module Editor.CodeEdit.Sugar
  ( DefinitionRef(..)
  , DefinitionNewType(..)
  , Builtin(..)
  , Entity(..), Actions(..)
  , Expression(..), ExpressionRef(..)
  , Where(..), WhereItem(..)
  , Func(..), FuncParam(..)
  , Pi(..), Apply(..), Section(..), Hole(..), LiteralInteger(..), Inferred(..)
  , HasParens(..)
  , convertDefinition
  , convertExpression, convertExpressionPure
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow (first)
import Control.Monad (guard, liftM, (<=<))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Functor.Identity (Identity(..))
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Data.UnionFind.IntMap (newUnionFind)
import Editor.Anchors (ViewTag)
import System.Random (RandomGen)
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.Binary.Utils as BinaryUtils
import qualified Data.List.Utils as ListUtils
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
  , mDelete      :: Maybe (T m Guid)
  -- invariant: cut includes the action of mDelete if one exists, or
  -- replace if one doesn't
  , mNextArg     :: Maybe (ExpressionRef m)
  }

data Entity m = Entity
  { guid     :: Guid
  , eActions :: Maybe (Actions m)
  }

data HasParens = HaveParens | DontHaveParens

data ExpressionRef m = ExpressionRef
  { rExpression :: Expression m
  , rInferredTypes :: [ExpressionRef m]
  , rEntity :: Entity m
  }

data WhereItem m = WhereItem
  { wiEntity :: Entity m
  , wiValue :: ExpressionRef m
  , wiType :: ExpressionRef m
  }

data Where m = Where
  { wWheres :: [WhereItem m]
  , wBody :: ExpressionRef m
  }

data FuncParam m = FuncParam
  { fpEntity :: Entity m
  , fpType :: ExpressionRef m
  , fpBody :: ExpressionRef m
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
  { holeScope :: [Data.VariableRef]
  , holePickResult :: Maybe (Data.PureGuidExpression -> T m Guid)
  , holePaste :: Maybe (T m Guid)
  , holeInferredValues :: [Data.PureGuidExpression]
  , holeDefinitionType :: Data.DefinitionIRef -> DataTyped.Infer (T m) DataTyped.TypeRef
  , holeCheckInfer :: Data.PureGuidExpression -> T m [Data.PureGuidExpression]
  }

data LiteralInteger m = LiteralInteger
  { liValue :: Integer
  , liSetValue :: Maybe (Integer -> T m ())
  }

data Inferred m = Inferred
  { iValue :: ExpressionRef m
  , iHole :: Hole m
  }

data Expression m
  = ExpressionApply   { eHasParens :: HasParens, eApply :: Apply m }
  | ExpressionSection { eHasParens :: HasParens, eSection :: Section m }
  | ExpressionWhere   { eHasParens :: HasParens, eWhere :: Where m }
  | ExpressionFunc    { eHasParens :: HasParens, eFunc :: Func m }
  | ExpressionPi      { eHasParens :: HasParens, ePi :: Pi m }
  | ExpressionGetVariable { _eGetVar :: Data.VariableRef }
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

AtFieldTH.make ''Entity
AtFieldTH.make ''Actions

data Loopable a = Loop | NonLoop a

data ExprEntityStored m = ExprEntityStored
  { eesProp :: Data.ExpressionIRefProperty (T m)
  , eeInferredTypes :: DataTyped.TypeRef -- TODO: name
  , eeInferredValues :: DataTyped.TypeRef
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
      ( return $ DataTyped.eeValue e
      , return .
        ExprEntity (DataTyped.eeGuid e)
        (Just
         (ExprEntityStored
          (DataTyped.eeRef e)
          (DataTyped.eeInferredType e)
          (DataTyped.eeInferredValue e))) .
        NonLoop
      )

data SugarContext m = SugarContext
  { scScope :: [(Data.VariableRef, DataTyped.TypeRef)]
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
    { scScope = []
    , scDef = def
    , scBuiltinsMap = builtinsMap
    }

putInScope :: Monad m => DataTyped.TypeRef -> Data.VariableRef -> Sugar m a -> Sugar m a
putInScope typeRef x = atSugar . Reader.local . atScScope $ ((x, typeRef) :)

readScope :: Monad m => Sugar m [(Data.VariableRef, DataTyped.TypeRef)]
readScope = Sugar $ Reader.asks scScope

readDefinition :: Monad m => Sugar m (Maybe (DataTyped.StoredDefinition (T m)))
readDefinition = Sugar $ Reader.asks scDef

readBuiltinsMap :: Monad m => Sugar m Anchors.BuiltinsMap
readBuiltinsMap = Sugar $ Reader.asks scBuiltinsMap

liftTransaction :: Monad m => T m a -> Sugar m a
liftTransaction = Sugar . lift

type Convertor m = ExprEntity m -> Sugar m (ExpressionRef m)

makeEntity :: Monad m => ExprEntity m -> Entity m
makeEntity ee = makeEntityGuid (eeGuid ee) ee

mkCutter :: Monad m => Data.ExpressionIRef -> T m Guid -> T m Guid
mkCutter iref replaceWithHole = do
  Anchors.modP Anchors.clipboards (iref:)
  replaceWithHole

mkActions :: Monad m => Data.ExpressionIRefProperty (T m) -> Actions m
mkActions stored =
  Actions
  { addNextArg = guidify $ DataOps.callWithArg stored
  , callWithArg = guidify $ DataOps.callWithArg stored
  , giveAsArg = guidify $ DataOps.giveAsArg stored
  , lambdaWrap = guidify $ DataOps.lambdaWrap stored
  , addWhereItem = guidify $ DataOps.redexWrap stored
  , replace = doReplace
    -- mDelete gets overridden by parent if it is an apply.
  , mDelete = Nothing
  , cut = mkCutter (Property.value stored) doReplace
  , mNextArg = Nothing
  }
  where
    guidify = liftM Data.exprIRefGuid
    doReplace = guidify $ DataOps.replaceWithHole stored

makeEntityGuid :: Monad m => Guid -> ExprEntity m -> Entity m
makeEntityGuid exprGuid exprI =
  Entity
  { guid = exprGuid
  , eActions = fmap mkActions $ eeProp exprI
  }

derefTypeRef
  :: (Monad m, RandomGen g)
  => Sugar m (g -> DataTyped.TypeRef -> [DataTyped.LoopGuidExpression])
derefTypeRef = do
  builtinsMap <- readBuiltinsMap
  mDefinition <- readDefinition
  let f def gen = DataTyped.derefTypeRef gen builtinsMap $ DataTyped.deTypeContext def
  return $ maybe ((const . const) []) f mDefinition

mkExpressionRef
  :: Monad m
  => ExprEntity m
  -> Expression m -> Sugar m (ExpressionRef m)
mkExpressionRef ee expr = do
  deref <- derefTypeRef
  let types = maybe [] (deref gen . eeInferredTypes) $ eeStored ee
  inferredTypesRefs <-
    mapM (convertExpressionI . eeFromITL) types
  return
    ExpressionRef
    { rExpression = expr
    , rInferredTypes = inferredTypesRefs
    , rEntity = makeEntity ee
    }
  where
    gen =
      Random.mkStdGen . (*2) . BinaryUtils.decodeS . Guid.bs $
      eeGuid ee

addStoredDeleteCutActions
  :: Monad m
  => Data.ExpressionIRefProperty (T m)
  -> Data.ExpressionIRefProperty (T m)
  -> Data.ExpressionIRefProperty (T m)
  -> Entity m -> Entity m
addStoredDeleteCutActions deletedP parentP replacerP =
  (atEActions . fmap)
  ((atCut . const) cutter .
   (atMDelete . const) (Just delete))
  where
    cutter = mkCutter (Property.value deletedP) delete
    delete = do
      Property.set parentP $ Property.value replacerP
      return . Data.exprIRefGuid $ Property.value replacerP

addDeleteCutActions
  :: Monad m
  => ExprEntity m -> ExprEntity m -> ExprEntity m
  -> Entity m -> Entity m
addDeleteCutActions deletedI parentI replacerI =
  fromMaybe id $
  addStoredDeleteCutActions <$> eeProp deletedI <*> eeProp parentI <*> eeProp replacerI

addDeleteCut
  :: Monad m
  => ExprEntity m -> ExprEntity m -> ExprEntity m
  -> ExpressionRef m -> ExpressionRef m
addDeleteCut deletedI exprI replacerI =
  atREntity $ addDeleteCutActions deletedI exprI replacerI

convertLambdaParam
  :: Monad m
  => Data.Lambda (ExprEntity m)
  -> ExpressionRef m
  -> ExprEntity m -> Sugar m (FuncParam m)
convertLambdaParam (Data.Lambda paramTypeI bodyI) bodyRef exprI = do
  typeExpr <- convertExpressionI paramTypeI
  return FuncParam
    { fpEntity =
        addDeleteCutActions exprI exprI bodyI $
        makeEntity exprI
    , fpType = typeExpr
    , fpBody = bodyRef
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
      Just stored ->
        putInScope (eeInferredValues stored) . Data.ParameterRef $
        eeGuid exprI

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
    ExpressionFunc DontHaveParens . atFParams (param :) $
    case rExpression sBody of
      ExpressionFunc _ x -> x
      _ -> Func [] sBody


convertPi
  :: Monad m
  => Data.Lambda (ExprEntity m)
  -> Convertor m
convertPi lambda@(Data.Lambda paramTypeI bodyI) exprI = do
  (param, sBody) <- convertLambda lambda exprI
  mkExpressionRef exprI $ ExpressionPi DontHaveParens
    Pi
    { pParam = atFpType addApplyChildParens param
    , pResultType = addDeleteCut bodyI exprI paramTypeI sBody
    }

convertWhere
  :: Monad m
  => ExpressionRef m
  -> ExprEntity m
  -> Data.Lambda (ExprEntity m)
  -> Convertor m
convertWhere valueRef lambdaI lambda@(Data.Lambda typeI bodyI) applyI = do
  typeRef <- convertExpressionI typeI
  sBody <- convertLambdaBody lambda lambdaI
  mkExpressionRef applyI .
    ExpressionWhere DontHaveParens . atWWheres (item typeRef :) $
    case rExpression sBody of
      ExpressionWhere _ x -> x
      _ -> Where [] sBody
  where
    item typeRef = WhereItem
      { wiEntity =
          addDeleteCutActions applyI applyI bodyI $
          makeEntityGuid (eeGuid lambdaI) applyI
      , wiValue = valueRef
      , wiType = typeRef
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
      atREntity . atEActions . fmap . atAddNextArg . const .
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
    opRef <- mkExpressionRef funcFuncI $ ExpressionGetVariable op
    let
      newLArgRef =
        addDeleteCut funcArgI funcI funcFuncI $ addApplyChildParens lArgRef
      newRArgRef = addDeleteCut argI exprI funcI $ addApplyChildParens rArgRef
      newOpRef =
        atFunctionType .
        addDeleteCut funcFuncI funcI funcArgI $
        setAddArg exprI opRef
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
  let newArgRef = addDeleteCut argI exprI opI $ addApplyChildParens argRef
  opRef <- mkExpressionRef opI $ ExpressionGetVariable op
  let
    newOpRef =
      atFunctionType .
      addDeleteCut opI exprI argI .
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
      addDeleteCut argI exprI funcI .
      setAddArg exprI $
      atRExpression addParens argRef
    setNextArg = atREntity . atEActions . fmap . atMNextArg . const $ Just newArgRef
    newFuncRef =
      addDeleteCut funcI exprI argI .
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
  name <- liftTransaction . Anchors.getP $ Anchors.variableNameRef varRef
  getVarExpr <-
    mkExpressionRef exprI $
    ExpressionGetVariable varRef
  if Infix.isInfixName name
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

fromInferred :: [DataTyped.LoopGuidExpression] -> Maybe Data.PureGuidExpression
fromInferred = DataTyped.pureGuidFromLoop <=< ListUtils.theOne

convertHole :: Monad m => Convertor m
convertHole exprI = do
  mPaste <- maybe (return Nothing) mkPaste $ eeProp exprI
  scope <- readScope
  deref <- derefTypeRef
  builtinsMap <- readBuiltinsMap
  mDef <- readDefinition
  let
    maybeInferredValues =
      map DataTyped.pureGuidFromLoop .
      maybe [] (deref gen . eeInferredValues) $ eeStored exprI
    gen =
      Random.mkStdGen . (+1) . (*2) . BinaryUtils.decodeS $ Guid.bs eGuid
    checkInfer holeType expr =
      liftM (maybe [] (const [expr]) . fromInferred) .
      DataTyped.derefResumedInfer (Random.mkStdGen 0) builtinsMap
      (maybe newUnionFind DataTyped.deTypeContext mDef) $ do
        typedExpr <-
          DataTyped.pureInferExpressionWithinContext
          ((map . first) Data.variableRefGuid scope) mDef expr
        DataTyped.unify holeType $
          DataTyped.eeInferredType typedExpr
        return holeType
    hole = Hole
      { holeScope = map fst scope
      , holePickResult = fmap pickResult $ eeProp exprI
      , holePaste = mPaste
      , holeInferredValues = catMaybes maybeInferredValues
      , holeDefinitionType = DataTyped.loadDefTypeWithinContext mDef
      , holeCheckInfer =
        (maybe . const . return) []
        (checkInfer . eeInferredTypes) $ eeStored exprI
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

convertMagic :: Monad m => Convertor m
convertMagic exprI =
  mkExpressionRef exprI $ fakeBuiltin ["Core"] "Magic"

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
    convert Data.ExpressionMagic = convertMagic
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
convertDefinitionI (DataTyped.StoredDefinition defI defInferredType (Data.Definition bodyI typeI) _) = do
  bodyS <- convertExpressionI bodyEntity
  typeS <- convertExpressionI $ eeFromTypedExpression typeI
  deref <- derefTypeRef
  mInferredTypePure <- runMaybeT $ do
    inferredType <- toMaybeT . fromInferred $ deref (Random.mkStdGen 0) defInferredType
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
        (Data.Definition . Property.value . DataTyped.eeRef) bodyI =<<
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
