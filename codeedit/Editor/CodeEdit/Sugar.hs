{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}

module Editor.CodeEdit.Sugar
  ( DefinitionRef(..)
  , Builtin(..)
  , Expression(..), Actions(..), ExpressionRef(..)
  , Where(..), WhereItem(..)
  , Func(..), FuncParam(..)
  , Pi(..), Apply(..), Section(..), Hole(..), LiteralInteger(..)
  , HasParens(..)
  , convertDefinition
  , convertExpression
  ) where

import Control.Applicative((<$>), (<*>))
import Control.Monad(liftM, (<=<))
import Control.Monad.Trans.Class(MonadTrans(..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Functor.Identity (Identity(..))
import Data.Store.Guid(Guid)
import Data.Store.Transaction(Transaction)
import Editor.Anchors(ViewTag)
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.Store.Property as Property
import qualified Editor.Anchors as Anchors
import qualified Editor.CodeEdit.Infix as Infix
import qualified Editor.Data as Data
import qualified Editor.Data.Ops as DataOps
import qualified Editor.Data.Typed as DataTyped

type T = Transaction ViewTag

type MAction m = Maybe (T m Guid)

data Actions m = Actions
  { guid         :: Guid
  , addNextArg   :: MAction m
  , giveAsArg    :: MAction m
  , callWithArg  :: MAction m
  , lambdaWrap   :: MAction m
  , addWhereItem :: MAction m
  , mReplace     :: MAction m
  , mDelete      :: MAction m
  , mCut         :: MAction m
  , mNextArg     :: Maybe (ExpressionRef m)
  }

-- TODO: Only Expression types that CAN be wrapped with () should be,
-- as prerequisite of sections which will not have HasParens in
-- them...
data HasParens = HaveParens | DontHaveParens

data ExpressionRef m = ExpressionRef
  { rExpression :: Expression m
  , rInferredTypes :: [ExpressionRef m]
  , rActions :: Actions m
  }

data WhereItem m = WhereItem
  { wiActions :: Actions m
  , wiValue :: ExpressionRef m
  , wiType :: ExpressionRef m
  }

data Where m = Where
  { wWheres :: [WhereItem m]
  , wBody :: ExpressionRef m
  }

data FuncParam m = FuncParam
  { fpActions :: Actions m
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
  , holePickResult :: Maybe (Data.ExpressionI -> T m Guid)
  , holeMFlipFuncArg :: Maybe (T m ())
  , holePaste :: Maybe (T m Guid)
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

data DefinitionRef m = DefinitionRef
  { drGuid :: Guid
    -- TODO: This is the opposite order of the data model, reverse
    -- either of them:
  , drDef :: ExpressionRef m
  , drType :: ExpressionRef m
  , drInferredTypes :: [ExpressionRef m]
  }

AtFieldTH.make ''Hole
AtFieldTH.make ''Where
AtFieldTH.make ''FuncParam
AtFieldTH.make ''Func
AtFieldTH.make ''ExpressionRef
AtFieldTH.make ''Actions
AtFieldTH.make ''Apply
AtFieldTH.make ''Section
AtFieldTH.make ''Expression

data ExprEntity m = ExprEntity
  { eeGuid :: Guid
  , eeStored :: Maybe (DataTyped.StoredExpression (T m))
  , eeInferredTypes :: [DataTyped.InferredType]
  , eeValue :: Data.Expression (ExprEntity m)
  }

eeFromITE :: DataTyped.InferredType -> ExprEntity m
eeFromITE = runIdentity . Data.mapMExpression (f . DataTyped.unInferredType)
  where
    f e =
      ( return $ DataTyped.iteValue e
      , return . ExprEntity (DataTyped.iteGuid e) Nothing []
      )
writeIRef
  :: Monad m
  => ExprEntity m
  -> Maybe (Data.ExpressionI -> Transaction t m ())
writeIRef = fmap Data.writeExprIRef . eeIRef

argument :: (a -> b) -> (b -> c) -> a -> c
argument = flip (.)

writeIRefVia
  :: Monad m
  => (a -> Data.ExpressionI)
  -> ExprEntity m
  -> Maybe (a -> Transaction t m ())
writeIRefVia f = (fmap . fmap . argument) f writeIRef

eeIRef :: ExprEntity m -> Maybe Data.ExpressionIRef
eeIRef = fmap DataTyped.esIRef . eeStored

eeReplace :: ExprEntity m -> Maybe (Data.ExpressionIRef -> T m ())
eeReplace = DataTyped.esReplace <=< eeStored

eeFromTypedExpression :: DataTyped.TypedExpressionEntity (T m) -> ExprEntity m
eeFromTypedExpression = runIdentity . Data.mapMExpression f
  where
    f e =
      ( return $ DataTyped.eeValue e
      , return . ExprEntity (DataTyped.eeGuid e) (Just (DataTyped.eeStored e)) (DataTyped.eeInferredType e)
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

addArg :: Monad m => ExprEntity m -> ExprEntity m -> MAction m
addArg whereI exprI =
  (fmap . liftM) Data.exprIRefGuid $
  DataOps.callWithArg <$>
  eeIRef exprI <*>
  eeReplace whereI

makeActions :: Monad m => ExprEntity m -> Actions m
makeActions exprI = makeActionsGuid (eeGuid exprI) exprI

mkCutter :: Monad m => Data.ExpressionIRef -> T m Guid -> T m Guid
mkCutter iref delete = do
  Property.pureModify Anchors.clipboards (iref:)
  delete

makeActionsGuid :: Monad m => Guid -> ExprEntity m -> Actions m
makeActionsGuid exprGuid exprI =
  Actions
  { guid = exprGuid
  , addNextArg = addArg exprI exprI
  , callWithArg = addArg exprI exprI
  , giveAsArg = withIRef DataOps.giveAsArg
  , lambdaWrap = withIRef DataOps.lambdaWrap
  , addWhereItem = withIRef DataOps.redexWrap
    -- Hole will remove mReplace because no point replacing hole with hole.
  , mReplace = replace
    -- mDelete gets overridden by parent if it is an apply.
  , mDelete = Nothing
  , mCut = mkCutter <$> eeIRef exprI <*> replace
  , mNextArg = Nothing
  }
  where
    replace =
      fmap (liftM Data.exprIRefGuid . DataOps.replaceWithHole) setExprI
    setExprI = eeReplace exprI
    withIRef f =
      (fmap . liftM) Data.exprIRefGuid $
      f <$> eeIRef exprI <*> setExprI

mkExpressionRef
  :: Monad m
  => ExprEntity m
  -> Expression m -> Sugar m (ExpressionRef m)
mkExpressionRef exprI expr = do
  typeRef <-
    mapM (convertExpressionI . eeFromITE) $
    eeInferredTypes exprI
  return
    ExpressionRef
    { rExpression = expr
    , rInferredTypes = typeRef
    , rActions = makeActions exprI
    }

convertLambdaParam
  :: Monad m
  => Data.Lambda (ExprEntity m)
  -> ExpressionRef m
  -> ExprEntity m -> Sugar m (FuncParam m)
convertLambdaParam (Data.Lambda paramTypeI bodyI) bodyRef exprI = do
  typeExpr <- convertExpressionI paramTypeI
  return FuncParam
    { fpActions =
        addDeleteAction exprI exprI bodyI $
        makeActions exprI
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

addDeleteAction
  :: Monad m
  => ExprEntity m -> ExprEntity m -> ExprEntity m
  -> Actions m -> Actions m
addDeleteAction deletedI exprI replacerI actions =
  (atMCut . const)    mCutter .
  (atMDelete . const) mDeleter $
  actions
  where
    mCutter = do
      _ <- mReplace actions -- mReplace as a guard here: if no replacer, no cutter either
      mkCutter <$> eeIRef deletedI <*> mDeleter
    mDeleter =
      mkDeleter <$> eeReplace exprI <*> eeIRef replacerI
    mkDeleter replacer val = do
      ~() <- replacer val
      return $ Data.exprIRefGuid val

addDelete
  :: Monad m
  => ExprEntity m -> ExprEntity m -> ExprEntity m
  -> ExpressionRef m -> ExpressionRef m
addDelete deletedI exprI replacerI = atRActions $ addDeleteAction deletedI exprI replacerI

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
    , pResultType = addDelete bodyI exprI paramTypeI sBody
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
      { wiActions =
          addDeleteAction applyI applyI bodyI $
          makeActionsGuid (eeGuid lambdaI) applyI
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
infixOp = maybe (return Nothing) Infix.infixOp . eeIRef

convertApply
  :: Monad m
  => Data.Apply (ExprEntity m)
  -> Convertor m
convertApply apply@(Data.Apply funcI argI) exprI =
  case eeValue funcI of
    Data.ExpressionLambda lambda -> do
      valueRef <- convertExpressionI argI
      convertWhere valueRef funcI lambda exprI
    -- InfixR or ordinary prefix:
    Data.ExpressionApply funcApply@(Data.Apply funcFuncI _) -> do
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

setAddArg :: Monad m => ExprEntity m -> ExprEntity m -> ExpressionRef m -> ExpressionRef m
setAddArg whereI exprI =
  atRActions . atAddNextArg . const $ addArg whereI exprI

removeUninterestingType :: ExpressionRef m -> ExpressionRef m
removeUninterestingType exprRef =
  case rExpression exprRef of
    ExpressionHole {} -> exprRef -- Keep types on holes
    _ -> (atRInferredTypes . const) [] exprRef

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
        addDelete funcArgI funcI funcFuncI $ addApplyChildParens lArgRef
      newRArgRef = addDelete argI exprI funcI $ addApplyChildParens rArgRef
      newOpRef =
        removeUninterestingType .
        addDelete funcFuncI funcI funcArgI $
        setAddArg exprI exprI opRef
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
  let newArgRef = addDelete argI exprI opI $ addApplyChildParens argRef
  opRef <- mkExpressionRef opI $ ExpressionGetVariable op
  let
    newOpRef =
      removeUninterestingType .
      addDelete opI exprI argI .
      setAddArg exprI exprI $
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
      addDelete argI exprI funcI .
      setAddArg exprI exprI .
      addFlipFuncArg $
      addParens argRef
    setNextArg = atRActions . atMNextArg . const . Just $ newArgRef
    newFuncRef =
      addDelete funcI exprI argI .
      setNextArg .
      addApplyChildParens .
      removeUninterestingType .
      (atRExpression . atEApply . atApplyArg) setNextArg .
      (atRExpression . atESection . atSectionOp) setNextArg $
      funcRef
  mkExpressionRef exprI . ExpressionApply DontHaveParens $
    Apply newFuncRef newArgRef
  where
    addFlipFuncArg = atRExpression . atEHole . atHoleMFlipFuncArg . const $ mSetFlippedApply
    mSetFlippedApply = writeIRef exprI <*> mFlippedApply
    mFlippedApply = Data.ExpressionApply <$> (Data.Apply <$> eeIRef argI <*> eeIRef funcI)
    addParens = atRExpression . atEHasParens . const $ HaveParens

convertGetVariable :: Monad m => Data.VariableRef -> Convertor m
convertGetVariable varRef exprI = do
  name <- liftTransaction . Property.get $ Anchors.variableNameRef varRef
  getVarExpr <-
    mkExpressionRef exprI $
    ExpressionGetVariable varRef
  if Infix.isInfixName name
    then
      mkExpressionRef exprI .
      ExpressionSection HaveParens $
      Section Nothing ((atRInferredTypes . const) [] getVarExpr) Nothing Nothing
    else return getVarExpr

convertHole :: Monad m => Convertor m
convertHole exprI = do
  clipboardContent <- liftTransaction $ Property.get Anchors.clipboards
  let
    mClipPop =
      case clipboardContent of
      [] -> Nothing
      (clip : clips) -> Just (clip, Property.set Anchors.clipboards clips)
  scope <- readScope
  (liftM . atRActions)
    ((atMReplace . const) Nothing .
     (atMCut . const) Nothing) .
    mkExpressionRef exprI . ExpressionHole $
    Hole
    { holeScope = scope
    , holePickResult = pickResult <$> writeIRef exprI
    , holeMFlipFuncArg = Nothing
    , holePaste = paste <$> eeReplace exprI <*> mClipPop
    }
  where
    paste replacer (clip, popClip) = do
      ~() <- popClip
      ~() <- replacer clip
      return $ Data.exprIRefGuid clip
    pickResult write result = do
      ~() <- write result
      return $ eeGuid exprI

convertLiteralInteger :: Monad m => Integer -> Convertor m
convertLiteralInteger i exprI =
  mkExpressionRef exprI . ExpressionLiteralInteger $
  LiteralInteger
  { liValue = i
  , liSetValue = writeIRefVia Data.ExpressionLiteralInteger exprI
  }

convertBuiltin :: Monad m => Data.FFIName -> Convertor m
convertBuiltin name exprI =
  mkExpressionRef exprI . ExpressionBuiltin $
  Builtin
  { biName = name
  , biSetFFIName = writeIRefVia Data.ExpressionBuiltin exprI
  }

convertMagic :: Monad m => Convertor m
convertMagic exprI =
  mkExpressionRef exprI . ExpressionBuiltin $
  Builtin
  { biName = Data.FFIName ["Core"] "Magic"
  , biSetFFIName = Nothing
  }

convertExpressionI :: Monad m => Convertor m
convertExpressionI exprI =
  convert (eeValue exprI) exprI
  where
    convert (Data.ExpressionLambda x) = convertLambda x
    convert (Data.ExpressionPi x) = convertPi x
    convert (Data.ExpressionApply x) = convertApply x
    convert (Data.ExpressionGetVariable x) = convertGetVariable x
    convert (Data.ExpressionLiteralInteger x) = convertLiteralInteger x
    convert (Data.ExpressionBuiltin x) = convertBuiltin x
    convert Data.ExpressionMagic = convertMagic
    convert Data.ExpressionHole = convertHole

convertDefinitionI
  :: Monad m
  => DataTyped.TypedDefinitionEntity (T m)
  -> Sugar m (DefinitionRef m)
convertDefinitionI defI =
  case DataTyped.deValue defI of
  Data.Definition typeI bodyI -> do
    defType <- convertExpressionI $ eeFromTypedExpression typeI
    defBody <-
      convertExpressionI $ eeFromTypedExpression bodyI
    return .
      DefinitionRef defGuid ((atRInferredTypes . const) [] defBody) defType $
      rInferredTypes defBody
  where
    defGuid = DataTyped.deGuid defI

convertDefinition
  :: Monad m
  => DataTyped.TypedDefinitionEntity (T m)
  -> T m (DefinitionRef m)
convertDefinition = runSugar . convertDefinitionI

convertExpression
  :: Monad m
  => DataTyped.TypedExpressionEntity (T m) -> T m (ExpressionRef m)
convertExpression = runSugar . convertExpressionI . eeFromTypedExpression
