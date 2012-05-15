{-# LANGUAGE TemplateHaskell #-}

module Editor.CodeEdit.Sugar
  ( Expression(..), Actions(..), ExpressionRef(..)
  , Where(..), WhereItem(..)
  , Func(..), FuncParam(..)
  , Pi(..), Apply(..), Section(..), Hole(..), LiteralInteger(..)
  , HasParens(..)
  , convertExpression
  ) where

import Control.Monad(liftM)
import Data.Store.Guid(Guid)
import Data.Store.IRef(IRef)
import Data.Store.Transaction(Transaction)
import Editor.Anchors(ViewTag)
import Editor.DataOps(ExpressionSetter)
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Anchors as Anchors
import qualified Editor.CodeEdit.Infix as Infix
import qualified Editor.Data as Data
import qualified Editor.DataOps as DataOps

data Actions m = Actions
  { guid        :: Guid
  , addNextArg  :: Transaction ViewTag m Guid
  , giveAsArg   :: Transaction ViewTag m Guid
  , callWithArg :: Transaction ViewTag m Guid
  , lambdaWrap  :: Transaction ViewTag m Guid
  , mReplace    :: Maybe (Transaction ViewTag m Guid)
  , mDelete     :: Maybe (Transaction ViewTag m Guid)
  , mNextArg    :: Maybe (ExpressionRef m)
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
  , holePickResult :: Data.Expression -> Transaction ViewTag m Guid
  , holeMFlipFuncArg :: Maybe (Transaction ViewTag m ())
  }

data LiteralInteger m = LiteralInteger
  { liValue :: Integer
  , liSetValue :: Integer -> Transaction ViewTag m ()
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

AtFieldTH.make ''Hole
AtFieldTH.make ''Where
AtFieldTH.make ''Func
AtFieldTH.make ''ExpressionRef
AtFieldTH.make ''Actions
AtFieldTH.make ''Apply
AtFieldTH.make ''Section
AtFieldTH.make ''Expression

type Convertor m = IRef Data.Expression -> ExpressionSetter m -> Transaction ViewTag m (ExpressionRef m)

addArg :: Monad m => IRef Data.Expression -> ExpressionSetter m -> Transaction ViewTag m Guid
addArg exprI setExprI = liftM IRef.guid $ DataOps.callWithArg exprI setExprI

makeActions :: Monad m => IRef Data.Expression -> ExpressionSetter m -> Actions m
makeActions exprI setExprI =
  Actions
  { guid = IRef.guid exprI
  , addNextArg = addArg exprI setExprI
  , callWithArg = addArg exprI setExprI
  , giveAsArg = liftM IRef.guid $ DataOps.giveAsArg exprI setExprI
  , lambdaWrap = liftM IRef.guid $ DataOps.lambdaWrap exprI setExprI
    -- Hole will remove mReplace because no point replacing hole with hole.
  , mReplace = Just . liftM IRef.guid $ DataOps.replaceWithHole setExprI
    -- mDelete gets overridden by parent if it is an apply.
  , mDelete = Nothing
  , mNextArg = Nothing
  }

mkExpressionRef :: Monad m => IRef Data.Expression -> ExpressionSetter m -> Expression m -> Transaction ViewTag m (ExpressionRef m)
mkExpressionRef exprI setExprI expr =
  return
    ExpressionRef
    { rExpression = expr
    , rActions = makeActions exprI setExprI
    }

convertLambdaParam
  :: Monad m
  => (Data.Lambda -> Data.Expression)
  -> Data.Lambda -> Scope
  -> IRef Data.Expression -> ExpressionSetter m
  -> Transaction ViewTag m (FuncParam m)
convertLambdaParam con (Data.Lambda paramTypeI bodyI) scope exprI setExprI = do
  typeExpr <- convertNode scope paramTypeI typeSetter
  return $ FuncParam
    { fpActions =
        (atMDelete . const . Just) deleteArg $
        makeActions exprI setExprI
    , fpType = typeExpr
    }
  where
    typeSetter = Transaction.writeIRef exprI . con . (`Data.Lambda` bodyI)
    deleteArg = do
      setExprI bodyI
      return $ IRef.guid bodyI

convertLambda :: Monad m => Data.Lambda -> Scope -> Convertor m
convertLambda lambda@(Data.Lambda _ bodyI) scope exprI setExprI = do
  param <- convertLambdaParam Data.ExpressionLambda lambda scope exprI setExprI
  sBody <- convertNode (Data.ParameterRef exprI : scope) bodyI bodySetter
  mkExpressionRef exprI setExprI .
    ExpressionFunc DontHaveParens . atFParams (param :) $
    case rExpression sBody of
      ExpressionFunc _ x -> x
      _ -> Func [] sBody
  where
    bodySetter = DataOps.lambdaBodySetter Data.ExpressionLambda exprI lambda

convertPi :: Monad m => Data.Lambda -> Scope -> Convertor m
convertPi lambda@(Data.Lambda _ bodyI) scope exprI setExprI = do
  param <- convertLambdaParam Data.ExpressionPi lambda scope exprI setExprI
  sBody <- convertNode (Data.ParameterRef exprI : scope) bodyI bodySetter
  mkExpressionRef exprI setExprI $ ExpressionPi DontHaveParens
    Pi
    { pParam = param
    , pResultType = sBody
    }
  where
    bodySetter = DataOps.lambdaBodySetter Data.ExpressionPi exprI lambda

convertWhere
  :: Monad m
  => ExpressionRef m
  -> IRef Data.Expression
  -> Data.Lambda
  -> Scope
  -> Convertor m
convertWhere valueRef lambdaI lambda@(Data.Lambda _ bodyI) scope applyI setApplyI = do
  sBody <- convertNode (Data.ParameterRef lambdaI : scope) bodyI bodySetter
  mkExpressionRef applyI setApplyI . ExpressionWhere DontHaveParens . atWWheres (item :) $
    case rExpression sBody of
      ExpressionWhere _ x -> x
      _ -> Where [] sBody
  where
    bodySetter = DataOps.lambdaBodySetter Data.ExpressionLambda lambdaI lambda
    deleteItem = do
      setApplyI bodyI
      return $ IRef.guid bodyI
    item = WhereItem
      { wiActions =
          (atMDelete . const . Just) deleteItem $
          makeActions lambdaI setApplyI
      , wiValue = valueRef
      }

addApplyChildParens :: ExpressionRef m -> ExpressionRef m
addApplyChildParens =
  atRExpression f
  where
    f x@(ExpressionApply _ _) = x
    f x = (atEHasParens . const) HaveParens x

convertApply :: Monad m => Data.Apply -> Scope -> Convertor m
convertApply apply@(Data.Apply funcI argI) scope exprI setExprI = do
  func <- Transaction.readIRef funcI
  let
    prefixApply = convertApplyPrefix apply scope exprI setExprI
    setArgI = DataOps.applyArgSetter exprI apply
  case func of
    Data.ExpressionLambda lambda -> do
      valueRef <- convertNode scope argI setArgI
      convertWhere valueRef funcI lambda scope exprI setExprI
    -- InfixR or ordinary prefix:
    Data.ExpressionApply funcApply@(Data.Apply funcFuncI _) -> do
      mInfixOp <- Infix.infixOp funcFuncI
      case mInfixOp of
        Just op -> convertApplyInfixFull funcApply op apply scope exprI setExprI
        Nothing -> prefixApply
    -- InfixL or ordinary prefix:
    _ -> do
      mInfixOp <- Infix.infixOp funcI
      case mInfixOp of
        Just op -> convertApplyInfixL op apply scope exprI setExprI
        Nothing -> prefixApply

setAddArg :: Monad m => IRef Data.Expression -> ExpressionSetter m -> ExpressionRef m -> ExpressionRef m
setAddArg exprI setExprI = atRActions . atAddNextArg . const $ addArg exprI setExprI

addDelete :: Monad m => ExpressionSetter m -> IRef Data.Expression -> ExpressionRef m -> ExpressionRef m
addDelete parentSetter replacer =
  atRActions . atMDelete . const . Just $ do
    parentSetter replacer
    return $ IRef.guid replacer

convertApplyInfixFull
  :: Monad m
  => Data.Apply
  -> Data.VariableRef
  -> Data.Apply
  -> Scope
  -> Convertor m
convertApplyInfixFull funcApply@(Data.Apply funcFuncI funcArgI) op apply@(Data.Apply funcI argI) scope exprI setExprI = do
  rArgRef <- convertNode scope argI $ DataOps.applyArgSetter exprI apply
  lArgRef <- convertNode scope funcArgI $ DataOps.applyArgSetter funcI funcApply
  opRef <-
    mkExpressionRef funcFuncI (DataOps.applyFuncSetter funcI funcApply) $
    ExpressionGetVariable op
  let
    newLArgRef =
      addDelete funcSetter funcFuncI .
      addApplyChildParens $
      lArgRef
    newOpRef =
      addDelete funcSetter funcArgI .
      setAddArg exprI setExprI $
      opRef
    newRArgRef =
      addDelete setExprI funcI .
      addApplyChildParens $
      rArgRef
  mkExpressionRef exprI setExprI . ExpressionSection DontHaveParens $
    Section (Just newLArgRef) newOpRef (Just newRArgRef)
  where
    funcSetter = DataOps.applyFuncSetter exprI apply

convertApplyInfixL
  :: Monad m
  => Data.VariableRef
  -> Data.Apply
  -> Scope
  -> Convertor m
convertApplyInfixL op apply@(Data.Apply opI argI) scope exprI setExprI = do
  argRef <- convertNode scope argI $ DataOps.applyArgSetter exprI apply
  let
    newArgRef =
      addDelete setExprI opI .
      addApplyChildParens $
      argRef
  opRef <-
    mkExpressionRef opI (DataOps.applyFuncSetter exprI apply) $
    ExpressionGetVariable op
  let
    newOpRef =
      addDelete setExprI argI .
      setAddArg exprI setExprI $
      opRef
  mkExpressionRef exprI setExprI . ExpressionSection HaveParens $
    Section (Just newArgRef) newOpRef Nothing

convertApplyPrefix
  :: Monad m
  => Data.Apply
  -> Scope
  -> Convertor m
convertApplyPrefix apply@(Data.Apply funcI argI) scope exprI setExprI = do
  argRef <- convertNode scope argI $ DataOps.applyArgSetter exprI apply
  funcRef <- convertNode scope funcI $ DataOps.applyFuncSetter exprI apply
  let
    newArgRef =
      addDelete setExprI funcI .
      setAddArg exprI setExprI .
      addFlipFuncArg .
      addParens $ argRef
    setNextArg = atRActions . atMNextArg . const . Just $ newArgRef
    newFuncRef =
      addDelete setExprI argI .
      setNextArg .
      addApplyChildParens .
      (atRExpression . atEApply . atApplyArg) setNextArg $
      funcRef
  mkExpressionRef exprI setExprI . ExpressionApply DontHaveParens $
    Apply newFuncRef newArgRef
  where
    addFlipFuncArg =
      atRExpression . atEHole . atHoleMFlipFuncArg . const . Just .
      Transaction.writeIRef exprI . Data.ExpressionApply $
      Data.Apply argI funcI
    addParens = atRExpression . atEHasParens . const $ HaveParens

convertGetVariable :: Monad m => Data.VariableRef -> Convertor m
convertGetVariable varRef exprI setExprI = do
  name <- Property.get $ Anchors.variableNameRef varRef
  getVarExpr <-
    mkExpressionRef exprI setExprI $
    ExpressionGetVariable varRef
  if Infix.isInfixName name
    then
      mkExpressionRef exprI setExprI $
      ExpressionSection HaveParens (Section Nothing getVarExpr Nothing)
    else return getVarExpr

convertHole :: Monad m => Scope -> Convertor m
convertHole scope exprI setExprI =
  (liftM . atRActions . atMReplace . const) Nothing .
  mkExpressionRef exprI setExprI . ExpressionHole $
  Hole
  { holeScope = scope
  , holePickResult = pickResult
  , holeMFlipFuncArg = Nothing
  }
  where
    pickResult result = do
      Transaction.writeIRef exprI result
      return $ IRef.guid exprI

convertLiteralInteger :: Monad m => Integer -> Convertor m
convertLiteralInteger i exprI setExprI =
  mkExpressionRef exprI setExprI . ExpressionLiteralInteger $
  LiteralInteger
  { liValue = i
  , liSetValue = Transaction.writeIRef exprI . Data.ExpressionLiteralInteger
  }

convertNode :: Monad m => Scope -> Convertor m
convertNode scope exprI setExprI = do
  expr <- Transaction.readIRef exprI
  let
    conv =
      case expr of
      Data.ExpressionLambda x -> convertLambda x scope
      Data.ExpressionPi x -> convertPi x scope
      Data.ExpressionApply x -> convertApply x scope
      Data.ExpressionGetVariable x -> convertGetVariable x
      Data.ExpressionHole -> convertHole scope
      Data.ExpressionLiteralInteger x -> convertLiteralInteger x
  conv exprI setExprI

convertExpression :: Monad m => Convertor m
convertExpression = convertNode []
