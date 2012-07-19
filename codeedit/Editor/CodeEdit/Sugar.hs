{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}

module Editor.CodeEdit.Sugar
  ( DefinitionRef(..)
  , DefinitionNewType(..)
  , Builtin(..)
  , Entity(..), Actions(..)
  , Expression(..), ExpressionRef(..)
  , Where(..), WhereItem(..)
  , Func(..), FuncParam(..)
  , Pi(..), Apply(..), Section(..), Hole(..), LiteralInteger(..)
  , HasParens(..)
  , convertDefinition
  , convertExpression, convertExpressionPure
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (guard, liftM)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Functor.Identity (Identity(..))
import Data.Maybe (fromMaybe, isJust)
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Data.Store.IRef as IRef
import qualified Editor.Anchors as Anchors
import qualified Editor.CodeEdit.Infix as Infix
import qualified Editor.Data as Data
import qualified Editor.Data.Ops as DataOps
import qualified Editor.Data.Typed as DataTyped

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

type Scope = [Data.VariableRef]

data Hole m = Hole
  { holeScope :: Scope
  , holePickResult :: Maybe (Data.PureGuidExpression -> T m Guid)
  , holePaste :: Maybe (T m Guid)
  , holeInferredValues :: [ExpressionRef m]
  }

data LiteralInteger m = LiteralInteger
  { liValue :: Integer
  , liSetValue :: Maybe (Integer -> T m ())
  }

data Expression m
  = ExpressionApply   { eHasParens :: HasParens, eApply :: Apply m }
  | ExpressionSection { eHasParens :: HasParens, eSection :: Section m }
  | ExpressionWhere   { eHasParens :: HasParens, eWhere :: Where m }
  | ExpressionFunc    { eHasParens :: HasParens, eFunc :: Func m }
  | ExpressionPi      { eHasParens :: HasParens, ePi :: Pi m }
  | ExpressionGetVariable { _eGetVar :: Data.VariableRef }
  | ExpressionHole { eHole :: Hole m }
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

data ExprEntity m = ExprEntity
  { eeGuid :: Guid
  , eeStored :: Maybe (Data.ExpressionIRefProperty (T m))
  , eeInferredTypes :: [ExprEntity m]
  , eeInferredValues :: [ExprEntity m]
  , eeValue :: Loopable (Data.Expression (ExprEntity m))
  }

eeFromITL :: DataTyped.LoopGuidExpression -> ExprEntity m
eeFromITL (DataTyped.Loop itGuid) =
  ExprEntity itGuid Nothing [] [] Loop
eeFromITL
  (DataTyped.NoLoop (Data.GuidExpression itGuid val)) =
    ExprEntity itGuid Nothing [] [] . NonLoop $
    Data.mapExpression eeFromITL val

eeFromPure :: Data.PureGuidExpression -> ExprEntity m
eeFromPure (Data.PureGuidExpression (Data.GuidExpression g expr)) =
  ExprEntity g Nothing [] [] . NonLoop $ Data.mapExpression eeFromPure expr

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

eeFromTypedExpression
  :: DataTyped.TypedStoredExpression (T m) -> ExprEntity m
eeFromTypedExpression =
  runIdentity . Data.mapMExpression f
  where
    f e =
      ( return $ DataTyped.eeValue e
      , return .
        ExprEntity (DataTyped.storedGuid e)
        (Just (DataTyped.eeStored e))
        (map eeFromITL (DataTyped.eeInferredType e))
        (map eeFromITL (DataTyped.eeInferredValue e)) .
        NonLoop
      )

newtype Sugar m a = Sugar {
  unSugar :: ReaderT Scope (T m) a
  } deriving (Monad)

AtFieldTH.make ''Sugar

runSugar :: Sugar m a -> T m a
runSugar = (`runReaderT` []) . unSugar

putInScope :: Monad m => Data.VariableRef -> Sugar m a -> Sugar m a
putInScope x = (atSugar . Reader.local) (x:)

readScope :: Monad m => Sugar m Scope
readScope = Sugar Reader.ask

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
  , eActions = fmap mkActions $ eeStored exprI
  }

mkExpressionRef
  :: Monad m
  => ExprEntity m
  -> Expression m -> Sugar m (ExpressionRef m)
mkExpressionRef ee expr = do
  inferredTypesRefs <-
    mapM convertExpressionI $ eeInferredTypes ee
  return
    ExpressionRef
    { rExpression = expr
    , rInferredTypes = inferredTypesRefs
    , rEntity = makeEntity ee
    }

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
  addStoredDeleteCutActions <$> eeStored deletedI <*> eeStored parentI <*> eeStored replacerI

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

convertLambda
  :: Monad m
  => Data.Lambda (ExprEntity m)
  -> Convertor m
convertLambda lambda@(Data.Lambda _ bodyI) exprI = do
  sBody <-
    putInScope (Data.ParameterRef (eeGuid exprI)) $
    convertExpressionI bodyI
  param <- convertLambdaParam lambda sBody exprI
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
  sBody <-
    putInScope (Data.ParameterRef (eeGuid exprI)) $
    convertExpressionI bodyI
  param <- convertLambdaParam lambda sBody exprI
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
convertWhere valueRef lambdaI (Data.Lambda typeI bodyI) applyI = do
  typeRef <- convertExpressionI typeI
  sBody <-
    putInScope (Data.ParameterRef (eeGuid lambdaI)) $
    convertExpressionI bodyI
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

addApplyChildParens :: ExpressionRef m -> ExpressionRef m
addApplyChildParens =
  atRExpression f
  where
    f x@(ExpressionApply _ _) = x
    f x = (atEHasParens . const) HaveParens x

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
  maybe id f $ eeStored exprI
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
      addParens argRef
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
  where
    addParens = atRExpression . atEHasParens . const $ HaveParens

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

convertHole :: Monad m => Convertor m
convertHole exprI = do
  mPaste <- maybe (return Nothing) mkPaste $ eeStored exprI
  scope <- readScope
  inferredValues <- mapM convertExpressionI $ eeInferredValues exprI
  mkExpressionRef exprI $ ExpressionHole Hole
    { holeScope = scope
    , holePickResult = fmap pickResult $ eeStored exprI
    , holePaste = mPaste
    , holeInferredValues = inferredValues
    }
  where
    pickResult irefP result = do
      ~() <- Data.writeIRefExpressionFromPure (Property.value irefP) result
      return $ eeGuid exprI

convertLiteralInteger :: Monad m => Integer -> Convertor m
convertLiteralInteger i exprI =
  mkExpressionRef exprI . ExpressionLiteralInteger $
  LiteralInteger
  { liValue = i
  , liSetValue = fmap (writeIRefVia Data.ExpressionLiteralInteger) $ eeStored exprI
  }

convertBuiltin :: Monad m => Data.Builtin (ExprEntity m) -> Convertor m
convertBuiltin (Data.Builtin name t) exprI =
  mkExpressionRef exprI . ExpressionBuiltin $
  Builtin
  { biName = name
  , biSetFFIName = do
      tProp <- eeStored t
      fmap (writeIRefVia (Data.ExpressionBuiltin . (`Data.Builtin` Property.value tProp))) $ eeStored exprI
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
    convert (Data.ExpressionLambda x) = convertLambda x
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
  => DataTyped.TypedStoredDefinition (T m)
  -> Sugar m (DefinitionRef m)
convertDefinitionI (DataTyped.StoredDefinition defI defInferredType (Data.Definition bodyI typeI)) = do
  bodyS <- convertExpressionI bodyEntity
  typeS <- convertExpressionI $ eeFromTypedExpression typeI
  mInferredTypePure <- runMaybeT $ do
    inferredType <-
      toMaybeT $ DataTyped.pureGuidFromLoop =<< theOne defInferredType
    guard $ isCompleteType inferredType
    return inferredType
  defNewType <- runMaybeT $ do
    inferredTypePure <- toMaybeT mInferredTypePure
    let defType = DataTyped.pureExpressionFromStored typeI
    guard . not $ DataTyped.alphaEq inferredTypePure defType
    inferredTypeS <- lift . convertExpressionI $ eeFromPure inferredTypePure
    return DefinitionNewType
      { dntNewType = inferredTypeS
      , dntAcceptNewType =
        Transaction.writeIRef defI .
        (Data.Definition . Property.value . DataTyped.eeStored) bodyI =<<
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
    theOne [x] = Just x
    theOne _ = Nothing

convertDefinition
  :: Monad m
  => DataTyped.TypedStoredDefinition (T m) -> T m (DefinitionRef m)
convertDefinition = runSugar . convertDefinitionI

convertExpressionPure
  :: Monad m => Data.PureGuidExpression -> T m (ExpressionRef m)
convertExpressionPure = runSugar . convertExpressionI . eeFromPure

convertExpression
  :: Monad m
  => DataTyped.TypedStoredExpression (T m) -> T m (ExpressionRef m)
convertExpression = runSugar . convertExpressionI . eeFromTypedExpression
