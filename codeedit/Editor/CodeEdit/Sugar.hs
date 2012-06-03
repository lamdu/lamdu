{-# LANGUAGE TemplateHaskell #-}

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
import Control.Monad(liftM)
import Data.Functor.Identity (Identity(..))
import Data.Store.Guid(Guid)
import Data.Store.IRef(IRef)
import Data.Store.Transaction(Transaction)
import Editor.Anchors(ViewTag)
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Editor.Anchors as Anchors
import qualified Editor.CodeEdit.Infix as Infix
import qualified Editor.Data as Data
import qualified Editor.Data.Typed as DataTyped
import qualified Editor.Data.Ops as DataOps

type MAction m = Maybe (Transaction ViewTag m Guid)

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
  , rType :: [ExpressionRef m]
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
  , holePickResult :: Maybe (Data.Expression IRef -> Transaction ViewTag m Guid)
  , holeMFlipFuncArg :: Maybe (Transaction ViewTag m ())
  , holePaste :: Maybe (Transaction ViewTag m Guid)
  }

data LiteralInteger m = LiteralInteger
  { liValue :: Integer
  , liSetValue :: Maybe (Integer -> Transaction ViewTag m ())
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
  , biSetFFIName :: Maybe (Data.FFIName -> Transaction ViewTag m ())
  }

data DefinitionRef m = DefinitionRef
  { drGuid :: Guid
    -- TODO: This is the opposite order of the data model, reverse
    -- either of them:
  , drDef :: ExpressionRef m
  , drType :: ExpressionRef m
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

type EntityExpr m = DataTyped.EntityT m Data.Expression
type Entity m = DataTyped.Entity (Transaction ViewTag m)

type Convertor m = Scope -> EntityExpr m -> Transaction ViewTag m (ExpressionRef m)

addArg :: Monad m => EntityExpr m -> EntityExpr m -> MAction m
addArg whereI exprI =
  (fmap . liftM) IRef.guid $ DataOps.callWithArg <$> DataTyped.entityIRef exprI <*> DataTyped.entityReplace whereI

makeActions :: Monad m => EntityExpr m -> Actions m
makeActions exprI = makeActionsGuid (DataTyped.entityGuid exprI) exprI

mkCutter :: Monad m => IRef (Data.Expression IRef) -> Transaction ViewTag m Guid -> Transaction ViewTag m Guid
mkCutter iref delete = do
  Property.pureModify Anchors.clipboards (iref:)
  delete

makeActionsGuid :: Monad m => Guid -> EntityExpr m -> Actions m
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
  , mCut = mkCutter <$> DataTyped.entityIRef exprI <*> replace
  , mNextArg = Nothing
  }
  where
    replace = fmap (liftM IRef.guid . DataOps.replaceWithHole) setExprI
    setExprI = DataTyped.entityReplace exprI
    withIRef f =
      (fmap . liftM) IRef.guid $
      f <$> DataTyped.entityIRef exprI <*> setExprI

mkExpressionRef
  :: Monad m
  => Scope -> EntityExpr m
  -> Expression m -> Transaction ViewTag m (ExpressionRef m)
mkExpressionRef scope exprI expr = do
  typeRef <- mapM (convertScopedExpression scope) $ DataTyped.entityType exprI
  return
    ExpressionRef
    { rExpression = expr
    , rType = typeRef
    , rActions = makeActions exprI
    }

convertLambdaParam
  :: Monad m
  => Data.Lambda (Entity m)
  -> ExpressionRef m
  -> Scope -> EntityExpr m -> Transaction ViewTag m (FuncParam m)
convertLambdaParam (Data.Lambda paramTypeI bodyI) bodyRef scope exprI = do
  typeExpr <- convertScopedExpression scope paramTypeI
  return FuncParam
    { fpActions =
        addDeleteAction exprI exprI bodyI $
        makeActions exprI
    , fpType = typeExpr
    , fpBody = bodyRef
    }

convertLambda
  :: Monad m
  => Data.Lambda (Entity m)
  -> Convertor m
convertLambda lambda@(Data.Lambda _ bodyI) scope exprI = do
  sBody <-
    convertScopedExpression (Data.ParameterRef (DataTyped.entityGuid exprI) : scope) bodyI
  param <- convertLambdaParam lambda sBody scope exprI
  mkExpressionRef scope exprI .
    ExpressionFunc DontHaveParens . atFParams (param :) $
    case rExpression sBody of
      ExpressionFunc _ x -> x
      _ -> Func [] sBody

addDeleteAction
  :: Monad m
  => EntityExpr m -> EntityExpr m -> EntityExpr m
  -> Actions m -> Actions m
addDeleteAction deletedI exprI replacerI actions =
  (atMCut . const)    mCutter .
  (atMDelete . const) mDeleter $
  actions
  where
    mCutter = do
      _ <- mReplace actions -- mReplace as a guard here: if no replacer, no cutter either
      mkCutter <$> DataTyped.entityIRef deletedI <*> mDeleter
    mDeleter = mkDeleter <$> DataTyped.entityReplace exprI <*> DataTyped.entityIRef replacerI
    mkDeleter replacer val = do
      ~() <- replacer val
      return $ IRef.guid val

addDelete
  :: Monad m
  => EntityExpr m -> EntityExpr m -> EntityExpr m
  -> ExpressionRef m -> ExpressionRef m
addDelete deletedI exprI replacerI = atRActions $ addDeleteAction deletedI exprI replacerI

convertPi
  :: Monad m
  => Data.Lambda (Entity m)
  -> Convertor m
convertPi lambda@(Data.Lambda paramTypeI bodyI) scope exprI = do
  sBody <- convertScopedExpression (Data.ParameterRef (DataTyped.entityGuid exprI) : scope) bodyI
  param <- convertLambdaParam lambda sBody scope exprI
  mkExpressionRef scope exprI $ ExpressionPi DontHaveParens
    Pi
    { pParam = atFpType addApplyChildParens param
    , pResultType = addDelete bodyI exprI paramTypeI sBody
    }

convertWhere
  :: Monad m
  => ExpressionRef m
  -> EntityExpr m
  -> Data.Lambda (Entity m)
  -> Convertor m
convertWhere valueRef lambdaI (Data.Lambda typeI bodyI) scope applyI = do
  typeRef <- convertScopedExpression scope typeI
  sBody <- convertScopedExpression (Data.ParameterRef (DataTyped.entityGuid lambdaI) : scope) bodyI
  mkExpressionRef scope applyI . ExpressionWhere DontHaveParens . atWWheres (item typeRef :) $
    case rExpression sBody of
      ExpressionWhere _ x -> x
      _ -> Where [] sBody
  where
    item typeRef = WhereItem
      { wiActions =
          addDeleteAction applyI applyI bodyI $
          makeActionsGuid (DataTyped.entityGuid lambdaI) applyI
      , wiValue = valueRef
      , wiType = typeRef
      }

addApplyChildParens :: ExpressionRef m -> ExpressionRef m
addApplyChildParens =
  atRExpression f
  where
    f x@(ExpressionApply _ _) = x
    f x = (atEHasParens . const) HaveParens x

infixOp :: Monad m => EntityExpr m -> Transaction ViewTag m (Maybe Data.VariableRef)
infixOp = maybe (return Nothing) Infix.infixOp . DataTyped.entityIRef

convertApply
  :: Monad m
  => Data.Apply (Entity m)
  -> Convertor m
convertApply apply@(Data.Apply funcI argI) scope exprI =
  case DataTyped.entityValue funcI of
    Data.ExpressionLambda lambda -> do
      valueRef <- convertScopedExpression scope argI
      convertWhere valueRef funcI lambda scope exprI
    -- InfixR or ordinary prefix:
    Data.ExpressionApply funcApply@(Data.Apply funcFuncI _) -> do
      mInfixOp <- infixOp funcFuncI
      case mInfixOp of
        Just op -> convertApplyInfixFull funcApply op apply scope exprI
        Nothing -> prefixApply
    -- InfixL or ordinary prefix:
    _ -> do
      mInfixOp <- infixOp funcI
      case mInfixOp of
        Just op -> convertApplyInfixL op apply scope exprI
        Nothing -> prefixApply
  where
    prefixApply = convertApplyPrefix apply scope exprI

setAddArg :: Monad m => EntityExpr m -> EntityExpr m -> ExpressionRef m -> ExpressionRef m
setAddArg whereI exprI =
  atRActions . atAddNextArg . const $ addArg whereI exprI

removeUninterestingType :: ExpressionRef m -> ExpressionRef m
removeUninterestingType = atRType f
  where
    f [_] = []
    f xs = xs

convertApplyInfixFull
  :: Monad m
  => Data.Apply (Entity m)
  -> Data.VariableRef
  -> Data.Apply (Entity m)
  -> Convertor m
convertApplyInfixFull (Data.Apply funcFuncI funcArgI) op (Data.Apply funcI argI) scope exprI = do
  rArgRef <- convertScopedExpression scope argI
  lArgRef <- convertScopedExpression scope funcArgI
  opRef <- mkExpressionRef scope funcFuncI $ ExpressionGetVariable op
  let
    newLArgRef = addDelete funcArgI funcI funcFuncI $ addApplyChildParens lArgRef
    newRArgRef = addDelete argI exprI funcI $ addApplyChildParens rArgRef
    newOpRef =
      removeUninterestingType .
      addDelete funcFuncI funcI funcArgI $
      setAddArg exprI exprI opRef
  mkExpressionRef scope exprI . ExpressionSection DontHaveParens .
    Section (Just newLArgRef) newOpRef (Just newRArgRef) . Just $ DataTyped.entityGuid funcI

convertApplyInfixL
  :: Monad m
  => Data.VariableRef
  -> Data.Apply (Entity m)
  -> Convertor m
convertApplyInfixL op (Data.Apply opI argI) scope exprI = do
  argRef <- convertScopedExpression scope argI
  let newArgRef = addDelete argI exprI opI $ addApplyChildParens argRef
  opRef <- mkExpressionRef scope opI $ ExpressionGetVariable op
  let
    newOpRef =
      removeUninterestingType .
      addDelete opI exprI argI .
      setAddArg exprI exprI $
      opRef
  mkExpressionRef scope exprI . ExpressionSection HaveParens $
    Section (Just newArgRef) newOpRef Nothing Nothing

convertApplyPrefix
  :: Monad m
  => Data.Apply (Entity m)
  -> Convertor m
convertApplyPrefix (Data.Apply funcI argI) scope exprI = do
  argRef <- convertScopedExpression scope argI
  funcRef <- convertScopedExpression scope funcI
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
  mkExpressionRef scope exprI . ExpressionApply DontHaveParens $
    Apply newFuncRef newArgRef
  where
    addFlipFuncArg = atRExpression . atEHole . atHoleMFlipFuncArg . const $ mSetFlippedApply
    mSetFlippedApply = DataTyped.writeIRef exprI <*> mFlippedApply
    mFlippedApply = Data.ExpressionApply <$> (Data.Apply <$> DataTyped.entityIRef argI <*> DataTyped.entityIRef funcI)
    addParens = atRExpression . atEHasParens . const $ HaveParens

convertGetVariable :: Monad m => Data.VariableRef -> Convertor m
convertGetVariable varRef scope exprI = do
  name <- Property.get $ Anchors.variableNameRef varRef
  getVarExpr <-
    mkExpressionRef scope exprI $
    ExpressionGetVariable varRef
  if Infix.isInfixName name
    then
      mkExpressionRef scope exprI .
      ExpressionSection HaveParens $
      Section Nothing ((atRType . const) [] getVarExpr) Nothing Nothing
    else return getVarExpr

convertHole :: Monad m => Convertor m
convertHole scope exprI = do
  clipboardContent <- Property.get Anchors.clipboards
  let
    mClipPop =
      case clipboardContent of
      [] -> Nothing
      (clip : clips) -> Just (clip, Property.set Anchors.clipboards clips)
  (liftM . atRActions)
    ((atMReplace . const) Nothing .
     (atMCut . const) Nothing) .
    mkExpressionRef scope exprI . ExpressionHole $
    Hole
    { holeScope = scope
    , holePickResult = pickResult <$> DataTyped.writeIRef exprI
    , holeMFlipFuncArg = Nothing
    , holePaste = paste <$> DataTyped.entityReplace exprI <*> mClipPop
    }
  where
    paste replacer (clip, popClip) = do
      ~() <- popClip
      ~() <- replacer clip
      return $ IRef.guid clip
    pickResult writeIRef result = do
      ~() <- writeIRef result
      return $ DataTyped.entityGuid exprI

convertLiteralInteger :: Monad m => Integer -> Convertor m
convertLiteralInteger i scope exprI =
  mkExpressionRef scope exprI . ExpressionLiteralInteger $
  LiteralInteger
  { liValue = i
  , liSetValue = DataTyped.writeIRefVia Data.ExpressionLiteralInteger exprI
  }

convertBuiltin :: Monad m => Data.FFIName -> Convertor m
convertBuiltin name scope exprI =
  mkExpressionRef scope exprI . ExpressionBuiltin $
  Builtin
  { biName = name
  , biSetFFIName = DataTyped.writeIRefVia Data.ExpressionBuiltin exprI
  }

convertMagic :: Monad m => Convertor m
convertMagic scope exprI =
  mkExpressionRef scope exprI . ExpressionBuiltin $
  Builtin
  { biName = Data.FFIName ["Core"] "Magic"
  , biSetFFIName = Nothing
  }

convertScopedExpression :: Monad m => Convertor m
convertScopedExpression scope exprI =
  convert (DataTyped.entityValue exprI) scope exprI
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
  => DataTyped.EntityT m Data.Definition
  -> Transaction ViewTag m (DefinitionRef m)
convertDefinitionI defI =
  case DataTyped.entityValue defI of
  Data.Definition typeI bodyI -> do
    defType <- convertScopedExpression [] typeI
    defBody <- convertScopedExpression [] bodyI
    return $ DefinitionRef defGuid defBody defType

  where
    defGuid = DataTyped.entityGuid defI

convertDefinition
  :: Monad m
  => DataTyped.EntityT m Data.Definition
  -> Transaction ViewTag m (DefinitionRef m)
convertDefinition =
  convertDefinitionI .
  DataTyped.atEntityValue
  (Data.atDefBody removeTypesOfTypes .
   Data.atDefType removeTypes)
  where
    removeTypesOfTypes =
      runIdentity . DataTyped.mapTypes (Identity . removeTypes)
    removeTypes =
      runIdentity . DataTyped.foldValues (Identity . (DataTyped.atEntityType . const) [])

convertExpression :: Monad m => EntityExpr m -> Transaction ViewTag m (ExpressionRef m)
convertExpression = convertScopedExpression []
