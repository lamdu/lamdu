{-# LANGUAGE TemplateHaskell #-}

module Editor.CodeEdit.Sugar
  ( Expression(..), ExpressionActions(..), ExpressionRef(..)
  , Where(..), WhereItem(..)
  , Func(..), FuncParam(..)
  , Apply(..), Hole(..), GetVariable(..)
  , HasParens(..)
  , convertExpression
  ) where

import Control.Monad(liftM)
import Data.Maybe(fromMaybe)
import Data.Store.Guid(Guid)
import Data.Store.IRef(IRef, guid)
import Data.Store.Transaction(Transaction)
import Editor.Anchors(ViewTag)
import Editor.DataOps(ExpressionPtr)
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Anchors as Anchors
import qualified Editor.CodeEdit.Infix as Infix
import qualified Editor.Data as Data
import qualified Editor.DataOps as DataOps

data ExpressionActions m = ExpressionActions
  { addNextArg :: Transaction ViewTag m Guid
  , lambdaWrap :: Transaction ViewTag m Guid
  , mReplace :: Maybe (Transaction ViewTag m Guid)
  , mDelete :: Maybe (Transaction ViewTag m Guid)
  , mNextArg :: Maybe (ExpressionRef m)
  }

-- TODO: Only Expression types that CAN be wrapped with () should be,
-- as prerequisite of sections which will not have HasParens in
-- them...
data HasParens = HaveParens | DontHaveParens

data ExpressionRef m = ExpressionRef
  { rExpressionPtr :: ExpressionPtr m
  , rHasParens :: HasParens
  , rExpression :: Expression m
  , rActions :: ExpressionActions m
  }

data WhereItem m = WhereItem
  { wiParamI :: IRef Data.Parameter
  -- TODO: Show type as well ?
  , wiValue :: ExpressionRef m
  -- Pointer to original apply provided to still give access to it.
  , wiApplyPtr :: ExpressionPtr m
  -- IRef to body of lambda provided to allow deleting the where item.
  , wiLambdaBodyI :: IRef Data.Expression
  }

data Where m = Where
  { wWheres :: [WhereItem m]
  , wBody :: ExpressionRef m
  }

data FuncParam m = FuncParam
  { fpParamI :: IRef Data.Parameter
  , fpType :: ExpressionRef m
  -- Pointer to original lambda expression provided to still give access to the lambda.
  , fpLambdaPtr :: ExpressionPtr m
  -- IRef to original body of lambda provided to allow deleting the param/lambda.
  , fpBodyI :: IRef Data.Expression
  }

-- Multi-param Lambda
data Func m = Func
  { fParams :: [FuncParam m]
  , fBody :: ExpressionRef m
  }

data ApplyType
  = ApplyPrefix
  | ApplyInfixL -- ^ Apply of infix op (Arg is larg to infix)
  | ApplyInfixR -- ^ Apply of apply of infix op (Arg is rarg to infix)

data Apply m = Apply
  { applyFunc :: ExpressionRef m
  , applyArg :: ExpressionRef m
  , applyType :: ApplyType
  }

data Hole m = Hole
  { holeState :: Data.HoleState
  , holeMFlipFuncArg :: Maybe (Transaction ViewTag m ())
  }

data VariableType = VariableNormal | VariableInfix

data GetVariable = GetVariable Data.VariableRef VariableType

data Expression m
  = ExpressionApply (Apply m)
  | ExpressionGetVariable GetVariable
  | ExpressionHole (Hole m)
  | ExpressionLiteralInteger Integer
  | ExpressionWhere (Where m)
  | ExpressionFunc (Func m)

AtFieldTH.make ''Hole
AtFieldTH.make ''Where
AtFieldTH.make ''Func
AtFieldTH.make ''ExpressionRef
AtFieldTH.make ''ExpressionActions
AtFieldTH.make ''Apply

type Convertor m = ExpressionPtr m -> Transaction ViewTag m (ExpressionRef m)

data IsReplacable = CanReplaceWithHole | NoReplace

mkExpressionRef :: Monad m => IsReplacable -> HasParens -> ExpressionPtr m -> Expression m -> ExpressionRef m
mkExpressionRef isReplaceable hasParens ptr expr =
  ExpressionRef
  { rExpression = expr
  , rHasParens = hasParens
  , rExpressionPtr = ptr
  , rActions =
      ExpressionActions
      { addNextArg = liftM guid $ DataOps.callWithArg ptr
      , lambdaWrap = liftM guid $ DataOps.lambdaWrap ptr
      , mReplace = replace
        -- mDelete gets overridden by parent if it is an apply.
      , mDelete = Nothing
      , mNextArg = Nothing
      }
  }
  where
    replace =
      case isReplaceable of
        CanReplaceWithHole -> Just . liftM guid $ DataOps.replaceWithHole ptr
        NoReplace -> Nothing


convertLambda :: Monad m => Data.Lambda -> Convertor m
convertLambda lambda ptr = do
  exprI <- Property.get ptr
  let
    typePtr = DataOps.lambdaParamTypeRef exprI lambda
    bodyPtr = DataOps.lambdaBodyRef exprI lambda
  typeExpr <- convertExpression typePtr
  sBody <- convertExpression bodyPtr
  let
    item = FuncParam
      { fpParamI = Data.tpParam (Data.lambdaParam lambda)
      , fpType = typeExpr
      , fpLambdaPtr = ptr
      , fpBodyI = Data.lambdaBody lambda
      }
  return . mkExpressionRef CanReplaceWithHole DontHaveParens ptr .
    ExpressionFunc . atFParams (item :) $
    case rExpression sBody of
      ExpressionFunc x -> x
      _ -> Func [] sBody

convertWhere
  :: Monad m
  => ExpressionRef m
  -> IRef Data.Expression
  -> Data.Lambda
  -> Convertor m
convertWhere value funcI lambda@(Data.Lambda (Data.TypedParam paramI _) bodyI) ptr = do
  let
    bodyPtr = DataOps.lambdaBodyRef funcI lambda
    item = WhereItem
      { wiParamI = paramI
      , wiValue = value
      , wiApplyPtr = ptr
      , wiLambdaBodyI = bodyI
      }
  sBody <- convertExpression bodyPtr
  return . mkExpressionRef CanReplaceWithHole DontHaveParens ptr . ExpressionWhere . atWWheres (item :) $
    case rExpression sBody of
      ExpressionWhere x -> x
      _ -> Where [] sBody

atExpressionApply :: (Apply m -> Apply m) -> Expression m -> Expression m
atExpressionApply f (ExpressionApply apply) = ExpressionApply $ f apply
atExpressionApply _ x = x

atExpressionHole :: (Hole m -> Hole m) -> Expression m -> Expression m
atExpressionHole f (ExpressionHole x) = ExpressionHole $ f x
atExpressionHole _ x = x

convertApply :: Monad m => Data.Apply -> Convertor m
convertApply apply ptr = do
  exprI <- Property.get ptr
  let funcI = Data.applyFunc apply
  func <- Transaction.readIRef funcI
  arg <- convertExpression $ DataOps.applyArgRef exprI apply
  case func of
    Data.ExpressionLambda lambda -> convertWhere arg funcI lambda ptr
    _ -> convertApplyNonLambda apply exprI ptr

applyTypeOfFunc :: Expression m -> ApplyType
applyTypeOfFunc (ExpressionGetVariable (GetVariable _ VariableInfix)) = ApplyInfixL
applyTypeOfFunc (ExpressionApply (Apply _ _ ApplyInfixL)) = ApplyInfixR
applyTypeOfFunc _ = ApplyPrefix

makeApplyArg
  :: Monad m
  => Bool
  -> (ExpressionRef m -> ExpressionRef m)
  -> IRef Data.Expression
  -> Data.Apply
  -> Transaction ViewTag m (ExpressionRef m)
makeApplyArg isInfix addArgHere exprI apply@(Data.Apply funcI argI) = do
  argExpr <- convertExpression $ DataOps.applyArgRef exprI apply
  isArgFullyAppliedInfix <-
    case rExpression argExpr of
    ExpressionApply (Apply argFunc _ _) -> Infix.isApplyOfInfixOp =<< Property.get (rExpressionPtr argFunc)
    _ -> return False
  let
    modifyArgParens
      | isInfix && not isArgFullyAppliedInfix =
        case rExpression argExpr of
        ExpressionApply{} -> id
        _ -> const . exprHasParens $ rExpression argExpr
      | otherwise = const . exprHasParens $ rExpression argExpr
    flipFuncAndArg =
      Transaction.writeIRef exprI . Data.ExpressionApply $
      Data.Apply argI funcI
    addArgAfterArg
      | isInfix = id
      | otherwise = addArgHere
  return $
    (atRExpression . atExpressionHole . atHoleMFlipFuncArg . const . Just) flipFuncAndArg .
    atRHasParens modifyArgParens .
    addArgAfterArg $
    argExpr

makeFuncArg
  :: Monad m
  => Bool
  -> (ExpressionRef m -> ExpressionRef m)
  -> IRef Data.Expression
  -> Data.Apply
  -> ExpressionRef m
  -> Transaction ViewTag m (ApplyType, ExpressionRef m)
makeFuncArg isInfix addArgHere exprI apply@(Data.Apply funcI _) argExpr = do
  isInfixFunc <- Infix.isInfixFunc funcI
  funcExpr <- convertExpression $ DataOps.applyFuncRef exprI apply
  needAddFuncParens <-
    case rExpression funcExpr of
    ExpressionGetVariable{} -> return False
    ExpressionApply (Apply funcFunc _ _) -> Infix.isApplyOfInfixOp =<< Property.get (rExpressionPtr funcFunc)
    _ -> return True
  let
    appType = applyTypeOfFunc $ rExpression funcExpr
    modifyFuncParens
      | needAddFuncParens = const . exprHasParens $ rExpression funcExpr
      | isInfix = const DontHaveParens
      | otherwise = id
    setNextArg = atRActions . atMNextArg . const $ Just argExpr
    setFuncArgNextArg =
      atRExpression . atExpressionApply . atApplyArg $ setNextArg
    addArgAfterFunc
      | isInfixFunc = addArgHere
      | otherwise = id
  return . ((,) appType) $
    setFuncArgNextArg . setNextArg .
    atRHasParens modifyFuncParens .
    addArgAfterFunc $ funcExpr

convertApplyNonLambda
  :: Monad m
  => Data.Apply
  -> IRef Data.Expression
  -> Convertor m
convertApplyNonLambda apply@(Data.Apply funcI argI) exprI ptr = do
  isInfixFunc <- Infix.isInfixFunc funcI
  isInfix <- liftM (isInfixFunc ||) (Infix.isApplyOfInfixOp funcI)
  let
    deleteNode siblingI whereToGo =
      atRActions . atMDelete . const . Just $ do
        _ <- DataOps.replace ptr siblingI
        liftM guid whereToGo
    whereToGoAfterDeleteArg =
      liftM (fromMaybe funcI) (Infix.infixFuncOfRArg funcI)
    addArgHere = atRActions . atAddNextArg . const . liftM guid $ DataOps.callWithArg ptr
    mParensType
      | isInfixFunc = HaveParens
      | otherwise = DontHaveParens
  argExpr <- makeApplyArg isInfix addArgHere exprI apply
  (appType, funcExpr) <- makeFuncArg isInfix addArgHere exprI apply argExpr
  return . mkExpressionRef NoReplace mParensType ptr . ExpressionApply $
    Apply
      (deleteNode argI (return argI) funcExpr)
      (deleteNode funcI whereToGoAfterDeleteArg argExpr) appType

exprHasParens :: Expression m -> HasParens
exprHasParens ExpressionHole{} = DontHaveParens
exprHasParens ExpressionLiteralInteger{} = DontHaveParens
exprHasParens ExpressionGetVariable{} = HaveParens
exprHasParens ExpressionApply{} = HaveParens
exprHasParens ExpressionFunc{} = HaveParens
exprHasParens ExpressionWhere{} = HaveParens

convertGetVariable :: Monad m => Data.VariableRef -> Convertor m
convertGetVariable varRef ptr = do
  name <- Property.get $ Anchors.variableNameRef varRef
  let
    (parens, infixType)
      | Infix.isInfixName name = (HaveParens, VariableInfix)
      | otherwise = (DontHaveParens, VariableNormal)
  return . mkExpressionRef CanReplaceWithHole parens ptr $ ExpressionGetVariable (GetVariable varRef infixType)

convertHole :: Monad m => Data.HoleState -> Convertor m
convertHole state ptr =
  return . mkExpressionRef NoReplace DontHaveParens ptr . ExpressionHole $ Hole state Nothing

convertLiteralInteger :: Monad m => Integer -> Convertor m
convertLiteralInteger i ptr =
  return . mkExpressionRef CanReplaceWithHole DontHaveParens ptr $ ExpressionLiteralInteger i

convertExpression :: Monad m => Convertor m
convertExpression ptr = do
  exprI <- Property.get ptr
  expr <- Transaction.readIRef exprI
  let
    conv =
      case expr of
      Data.ExpressionLambda x -> convertLambda x
      Data.ExpressionApply x -> convertApply x
      Data.ExpressionGetVariable x -> convertGetVariable x
      Data.ExpressionHole x -> convertHole x
      Data.ExpressionLiteralInteger x -> convertLiteralInteger x
  conv ptr
