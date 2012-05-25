{-# LANGUAGE TemplateHaskell #-}

module Editor.CodeEdit.Sugar
  ( Definition(..), DefinitionRef(..), Builtin(..)
  , Expression(..), Actions(..), ExpressionRef(..)
  , Where(..), WhereItem(..)
  , Func(..), FuncParam(..)
  , Pi(..), Apply(..), Section(..), Hole(..), LiteralInteger(..)
  , HasParens(..)
  , loadConvertDefinition
  ) where

import Control.Applicative((<$>), (<*>))
import Control.Monad(liftM, (<=<))
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
import qualified Editor.DataLoad as DataLoad
import qualified Editor.DataOps as DataOps

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
  , mNextArg     :: Maybe (ExpressionRef m)
  }

-- TODO: Only Expression types that CAN be wrapped with () should be,
-- as prerequisite of sections which will not have HasParens in
-- them...
data HasParens = HaveParens | DontHaveParens

data ExpressionRef m = ExpressionRef
  { rExpression :: Expression m
  , rActions :: Actions m
  }

data WhereItem m = WhereItem
  { wiActions :: Actions m
  -- TODO: Show type as well ?
  , wiValue :: ExpressionRef m
  }

data Where m = Where
  { wWheres :: [WhereItem m]
  , wBody :: ExpressionRef m
  }

data FuncParam m = FuncParam
  { fpActions :: Actions m
  , fpType :: ExpressionRef m
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
  }

type Scope = [Data.VariableRef]

data Hole m = Hole
  { holeScope :: Scope
  , holePickResult :: Maybe (Data.Expression IRef -> Transaction ViewTag m Guid)
  , holeMFlipFuncArg :: Maybe (Transaction ViewTag m ())
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

data Builtin m = Builtin
  { biName :: Data.FFIName
  , biType :: ExpressionRef m
  }

data Definition m
  = DefinitionExpression (ExpressionRef m)
  | DefinitionBuiltin (Builtin m)

data DefinitionRef m = DefinitionRef
  { drGuid :: Guid
  , drDef :: Definition m
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

type EntityExpr m = DataLoad.EntityT m Data.Expression
type Entity m = DataLoad.Entity (Transaction ViewTag m)

type Convertor m = Scope -> EntityExpr m -> Transaction ViewTag m (ExpressionRef m)

addArg :: Monad m => EntityExpr m -> EntityExpr m -> MAction m
addArg whereI exprI =
  (fmap . liftM) IRef.guid $ DataOps.callWithArg <$> DataLoad.iref exprI <*> DataLoad.replacer whereI

makeActions :: Monad m => EntityExpr m -> Actions m
makeActions exprI = makeActionsGuid (DataLoad.guid exprI) exprI

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
  , mReplace =
    fmap (liftM IRef.guid . DataOps.replaceWithHole) setExprI
    -- mDelete gets overridden by parent if it is an apply.
  , mDelete = Nothing
  , mNextArg = Nothing
  }
  where
    setExprI = DataLoad.replacer exprI
    withIRef f =
      (fmap . liftM) IRef.guid $
      f <$> DataLoad.iref exprI <*> setExprI

mkExpressionRef :: Monad m => EntityExpr m -> Expression m -> Transaction ViewTag m (ExpressionRef m)
mkExpressionRef exprI expr =
  return
    ExpressionRef
    { rExpression = expr
    , rActions = makeActions exprI
    }

argument :: (a -> b) -> (b -> c) -> a -> c
argument = flip (.)

convertLambdaParam
  :: Monad m
  => Data.Lambda (Entity m) -> Scope
  -> EntityExpr m
  -> Transaction ViewTag m (FuncParam m)
convertLambdaParam (Data.Lambda paramTypeI bodyI) scope exprI = do
  typeExpr <- convertExpression scope paramTypeI
  return FuncParam
    { fpActions =
        addDeleteAction exprI bodyI $
        makeActions exprI
    , fpType = typeExpr
    }

convertLambda
  :: Monad m
  => Data.Lambda (Entity m)
  -> Convertor m
convertLambda lambda@(Data.Lambda _ bodyI) scope exprI = do
  param <- convertLambdaParam lambda scope exprI
  sBody <-
    convertExpression (Data.ParameterRef (DataLoad.guid exprI) : scope) bodyI
  mkExpressionRef exprI .
    ExpressionFunc DontHaveParens . atFParams (param :) $
    case rExpression sBody of
      ExpressionFunc _ x -> x
      _ -> Func [] sBody

addDeleteAction 
  :: Monad m 
  => EntityExpr m -> EntityExpr m 
  -> Actions m -> Actions m
addDeleteAction exprI replacerI =
  atMDelete . const $ deleter <$> DataLoad.replacer exprI <*> DataLoad.iref replacerI
  where
    deleter replacer val = do
      ~() <- replacer val
      return $ IRef.guid val

addDelete 
  :: Monad m
  => EntityExpr m -> EntityExpr m 
  -> ExpressionRef m -> ExpressionRef m
addDelete exprI replacerI = atRActions $ addDeleteAction exprI replacerI

convertPi
  :: Monad m
  => Data.Lambda (Entity m)
  -> Convertor m
convertPi lambda@(Data.Lambda paramTypeI bodyI) scope exprI = do
  param <- convertLambdaParam lambda scope exprI
  sBody <- convertExpression (Data.ParameterRef (DataLoad.guid exprI) : scope) bodyI
  mkExpressionRef exprI $ ExpressionPi DontHaveParens
    Pi
    { pParam = atFpType addApplyChildParens param
    , pResultType = addDelete exprI paramTypeI sBody
    }

convertWhere
  :: Monad m
  => ExpressionRef m
  -> EntityExpr m
  -> Data.Lambda (Entity m)
  -> Convertor m
convertWhere valueRef lambdaI (Data.Lambda _ bodyI) scope applyI = do
  sBody <- convertExpression (Data.ParameterRef (DataLoad.guid lambdaI) : scope) bodyI
  mkExpressionRef applyI . ExpressionWhere DontHaveParens . atWWheres (item :) $
    case rExpression sBody of
      ExpressionWhere _ x -> x
      _ -> Where [] sBody
  where
    item = WhereItem
      { wiActions =
          addDeleteAction applyI bodyI $
          makeActionsGuid (DataLoad.guid lambdaI) applyI
      , wiValue = valueRef
      }

addApplyChildParens :: ExpressionRef m -> ExpressionRef m
addApplyChildParens =
  atRExpression f
  where
    f x@(ExpressionApply _ _) = x
    f x = (atEHasParens . const) HaveParens x

infixOp :: Monad m => EntityExpr m -> Transaction ViewTag m (Maybe Data.VariableRef)
infixOp = maybe (return Nothing) Infix.infixOp . DataLoad.iref

convertApply
  :: Monad m
  => Data.Apply (Entity m)
  -> Convertor m
convertApply apply@(Data.Apply funcI argI) scope exprI =
  case DataLoad.entityValue funcI of
    Data.ExpressionLambda lambda -> do
      valueRef <- convertExpression scope argI
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

convertApplyInfixFull
  :: Monad m
  => Data.Apply (Entity m)
  -> Data.VariableRef
  -> Data.Apply (Entity m)
  -> Convertor m
convertApplyInfixFull (Data.Apply funcFuncI funcArgI) op (Data.Apply funcI argI) scope exprI = do
  rArgRef <- convertExpression scope argI
  lArgRef <- convertExpression scope funcArgI
  opRef <- mkExpressionRef funcFuncI $ ExpressionGetVariable op
  let
    newLArgRef = addDelete funcI funcFuncI $ addApplyChildParens lArgRef
    newRArgRef = addDelete exprI funcI $ addApplyChildParens rArgRef
    newOpRef = addDelete funcI funcArgI $ setAddArg exprI exprI opRef
  mkExpressionRef exprI . ExpressionSection DontHaveParens $
    Section (Just newLArgRef) newOpRef (Just newRArgRef)

convertApplyInfixL
  :: Monad m
  => Data.VariableRef
  -> Data.Apply (Entity m)
  -> Convertor m
convertApplyInfixL op (Data.Apply opI argI) scope exprI = do
  argRef <- convertExpression scope argI
  let newArgRef = addDelete exprI opI $ addApplyChildParens argRef
  opRef <- mkExpressionRef opI $ ExpressionGetVariable op
  let
    newOpRef =
      addDelete exprI argI .
      setAddArg exprI exprI $
      opRef
  mkExpressionRef exprI . ExpressionSection HaveParens $
    Section (Just newArgRef) newOpRef Nothing

convertApplyPrefix
  :: Monad m
  => Data.Apply (Entity m)
  -> Convertor m
convertApplyPrefix (Data.Apply funcI argI) scope exprI = do
  argRef <- convertExpression scope argI
  funcRef <- convertExpression scope funcI
  let
    newArgRef =
      addDelete exprI funcI .
      setAddArg exprI exprI .
      addFlipFuncArg $
      addParens argRef
    setNextArg = atRActions . atMNextArg . const . Just $ newArgRef
    newFuncRef =
      addDelete exprI argI .
      setNextArg .
      addApplyChildParens .
      (atRExpression . atEApply . atApplyArg) setNextArg .
      (atRExpression . atESection . atSectionOp) setNextArg $
      funcRef
  mkExpressionRef exprI . ExpressionApply DontHaveParens $
    Apply newFuncRef newArgRef
  where
    addFlipFuncArg = atRExpression . atEHole . atHoleMFlipFuncArg . const $ mSetFlippedApply
    mSetFlippedApply = DataLoad.writeIRef exprI <*> mFlippedApply
    mFlippedApply = Data.ExpressionApply <$> (Data.Apply <$> DataLoad.iref argI <*> DataLoad.iref funcI)
    addParens = atRExpression . atEHasParens . const $ HaveParens

convertGetVariable 
  :: Monad m 
  => Data.VariableRef -> EntityExpr m 
  -> Transaction ViewTag m (ExpressionRef m)
convertGetVariable varRef exprI = do
  name <- Property.get $ Anchors.variableNameRef varRef
  getVarExpr <-
    mkExpressionRef exprI $
    ExpressionGetVariable varRef
  if Infix.isInfixName name
    then
      mkExpressionRef exprI $
      ExpressionSection HaveParens (Section Nothing getVarExpr Nothing)
    else return getVarExpr

convertHole :: Monad m => Convertor m
convertHole scope exprI =
  (liftM . atRActions . atMReplace . const) Nothing .
  mkExpressionRef exprI . ExpressionHole $
  Hole
  { holeScope = scope
  , holePickResult = pickResult <$> DataLoad.writeIRef exprI
  , holeMFlipFuncArg = Nothing
  }
  where
    pickResult writeIRef result = do
      ~() <- writeIRef result
      return $ DataLoad.guid exprI

convertLiteralInteger :: Monad m => Integer -> EntityExpr m -> Transaction ViewTag m (ExpressionRef m)
convertLiteralInteger i exprI =
  mkExpressionRef exprI . ExpressionLiteralInteger $
  LiteralInteger
  { liValue = i
  , liSetValue = (fmap . argument) Data.ExpressionLiteralInteger $ DataLoad.writeIRef exprI
  }

convertExpression :: Monad m => Convertor m
convertExpression scope exprI = case DataLoad.entityValue exprI of
  Data.ExpressionLambda x -> convertLambda x scope exprI
  Data.ExpressionPi x -> convertPi x scope exprI
  Data.ExpressionApply x -> convertApply x scope exprI
  Data.ExpressionGetVariable x -> convertGetVariable x exprI
  Data.ExpressionHole -> convertHole scope exprI
  Data.ExpressionLiteralInteger x -> convertLiteralInteger x exprI

convertDefinition
  :: Monad m
  => DataLoad.EntityT m Data.Definition
  -> Transaction ViewTag m (DefinitionRef m)
convertDefinition defI =
  liftM (DefinitionRef (DataLoad.guid defI)) $
    case DataLoad.entityValue defI of
    Data.DefinitionExpression exprI ->
      liftM DefinitionExpression $ convertExpression [] exprI
    Data.DefinitionBuiltin (Data.Builtin ffiName typeI) ->
      liftM (DefinitionBuiltin . Builtin ffiName) $ convertExpression [] typeI

loadConvertDefinition
  :: Monad m
  => IRef (Data.Definition IRef)
  -> Transaction ViewTag m (DefinitionRef m)
loadConvertDefinition = convertDefinition <=< DataLoad.loadDefinition
