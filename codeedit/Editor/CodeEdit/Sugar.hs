{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Editor.CodeEdit.Sugar
  ( Expression(..), ExpressionRef(..), ExpressionActions(..)
  , Where(..), atWWheres, atWBody
  , WhereItem(..)
  , Func(..), atFParams, atFBody
  , FuncParam(..)
  , Apply(..)
  , ParensType(..)
  , convertExpression
  ) where

import Control.Monad(liftM)
import Data.Store.Guid(Guid)
import Data.Store.IRef(IRef, guid)
import Data.Store.Property(Property(Property))
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

data ParensType = TextParens | SquareParens

data ExpressionActions m = ExpressionActions
  { addNextArg :: m Guid
  , lambdaWrap :: m Guid
  , mReplace :: Maybe (m Guid)
  }

data ExpressionRef m = ExpressionRef
  { rExpressionPtr :: ExpressionPtr m
  , rMParensType :: Maybe ParensType
  , rExpression :: Expression m
  , rActions :: ExpressionActions (Transaction ViewTag m)
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

data Apply m = Apply
  { applyFunc :: ExpressionRef m
  , applyArg :: ExpressionRef m
  }

data Expression m
  = ExpressionApply (Apply m)
  | ExpressionGetVariable Data.VariableRef
  | ExpressionHole Data.HoleState
  | ExpressionLiteralInteger Integer
  | ExpressionWhere (Where m)
  | ExpressionFunc (Func m)

AtFieldTH.make ''Where
AtFieldTH.make ''Func

data ApplyRole = ApplyFunc | ApplyArg

data ParentInfo m
  = ExpressionTop
  | ApplyChild
    { _aRole :: ApplyRole
    , _aIsInfix :: Bool
    , _aParentPtr :: ExpressionPtr m
    }

data ConvertParams m = ConvertParams
  { cpPtr :: ExpressionPtr m
  , cpParentInfo :: ParentInfo m
  }

type Convertor m = ConvertParams m -> Transaction ViewTag m (ExpressionRef m)

mkExpressionRef :: Monad m => Bool -> Maybe ParensType -> ConvertParams m -> Expression m -> ExpressionRef m
mkExpressionRef isReplaceable mParensType cp expr =
  ExpressionRef
  { rExpression = expr
  , rMParensType = mParensType
  , rExpressionPtr = cpPtr cp
  , rActions =
      ExpressionActions
      { addNextArg = liftM guid $ DataOps.callWithArg addNextArgPos
      , lambdaWrap = liftM guid . DataOps.lambdaWrap $ cpPtr cp
      , mReplace =
          if isReplaceable
          then Just . liftM guid . DataOps.replaceWithHole $ cpPtr cp
          else Nothing
      }
  }
  where
    addNextArgPos =
      case cpParentInfo cp of
      ExpressionTop -> cpPtr cp
      ApplyChild ApplyFunc False _ -> cpPtr cp
      ApplyChild ApplyArg True _ -> cpPtr cp
      ApplyChild _ _ ptr -> ptr

convertLambda :: Monad m => Data.Lambda -> Convertor m
convertLambda lambda cp = do
  exprI <- Property.get $ cpPtr cp
  let
    typePtr = DataOps.lambdaParamTypeRef exprI lambda
    bodyPtr = DataOps.lambdaBodyRef exprI lambda
  typeExpr <- convertNode $ ConvertParams typePtr ExpressionTop
  sBody <- convertNode $ ConvertParams bodyPtr ExpressionTop
  let
    item = FuncParam
      { fpParamI = Data.tpParam (Data.lambdaParam lambda)
      , fpType = typeExpr
      , fpLambdaPtr = cpPtr cp
      , fpBodyI = Data.lambdaBody lambda
      }
    mParenType =
      case cpParentInfo cp of
      ApplyChild ApplyArg _ _ -> Just TextParens
      ApplyChild ApplyFunc _ _ -> error "redex should not be handled by convertLambda"
      ExpressionTop -> Nothing
  return . mkExpressionRef True mParenType cp . ExpressionFunc . atFParams (item :) $ case rExpression sBody of
    ExpressionFunc x -> x
    _ -> Func [] sBody

convertWhere
  :: Monad m
  => ExpressionPtr m
  -> IRef Data.Expression
  -> Data.Lambda
  -> Convertor m
convertWhere argPtr funcI lambda@(Data.Lambda (Data.TypedParam paramI _) bodyI) cp = do
  value <- convertNode $ ConvertParams argPtr ExpressionTop
  let
    bodyPtr = DataOps.lambdaBodyRef funcI lambda
    item = WhereItem
      { wiParamI = paramI
      , wiValue = value
      , wiApplyPtr = cpPtr cp
      , wiLambdaBodyI = bodyI
      }
    mParenType =
      case cpParentInfo cp of
      ExpressionTop -> Nothing
      _ -> Just SquareParens
  sBody <- convertNode $ ConvertParams bodyPtr ExpressionTop
  return . mkExpressionRef True mParenType cp . ExpressionWhere . atWWheres (item :) $ case rExpression sBody of
    ExpressionWhere x -> x
    _ -> Where [] sBody

convertApply :: Monad m => Data.Apply -> Convertor m
convertApply (Data.Apply funcI argI) cp = do
  exprI <- Property.get $ cpPtr cp
  let
    argPtr =
      Property (return argI) $
      Transaction.writeIRef exprI .
      Data.ExpressionApply . Data.Apply funcI
  func <- Transaction.readIRef funcI
  case func of
    Data.ExpressionLambda lambda ->
      convertWhere argPtr funcI lambda cp
    _ -> do
      isInfixFunc <- Infix.isInfixFunc funcI
      isFullInfix <- Infix.isApplyOfInfixOp funcI
      let
        funcPtr =
          Property (return funcI) $
          Transaction.writeIRef exprI .
          Data.ExpressionApply . (`Data.Apply` argI)
        isInfix = isInfixFunc || isFullInfix
        hasParens =
          case cpParentInfo cp of
          ExpressionTop -> isInfixFunc
          ApplyChild ApplyArg False _ -> True
          ApplyChild ApplyArg True _ -> isInfix
          ApplyChild ApplyFunc _ _ -> isFullInfix
        mParenType = if hasParens then Just TextParens else Nothing
      funcExpr <- convertNode . ConvertParams funcPtr . ApplyChild ApplyFunc isInfixFunc $ cpPtr cp
      argExpr <- convertNode . ConvertParams argPtr . ApplyChild ApplyArg isInfix $ cpPtr cp
      return . mkExpressionRef True mParenType cp . ExpressionApply $ Apply funcExpr argExpr

convertGetVariable :: Monad m => Data.VariableRef -> Convertor m
convertGetVariable varRef cp = do
  mParenType <-
    case cpParentInfo cp of
    ApplyChild ApplyFunc _ _ -> return Nothing
    _ -> do
      name <- Property.get $ Anchors.variableNameRef varRef
      return $ if Infix.isInfixName name then Just TextParens else Nothing
  return . mkExpressionRef True mParenType cp $ ExpressionGetVariable varRef

convertHole :: Monad m => Data.HoleState -> Convertor m
convertHole state cp =
  return . mkExpressionRef False Nothing cp $ ExpressionHole state

convertLiteralInteger :: Monad m => Integer -> Convertor m
convertLiteralInteger i cp =
  return . mkExpressionRef True Nothing cp $ ExpressionLiteralInteger i

convertNode :: Monad m => Convertor m
convertNode cp = do
  exprI <- Property.get $ cpPtr cp
  expr <- Transaction.readIRef exprI
  let
    conv =
      case expr of
      Data.ExpressionLambda x -> convertLambda x
      Data.ExpressionApply x -> convertApply x
      Data.ExpressionGetVariable x -> convertGetVariable x
      Data.ExpressionHole x -> convertHole x
      Data.ExpressionLiteralInteger x -> convertLiteralInteger x
  conv cp

convertExpression :: Monad m => ExpressionPtr m -> Transaction ViewTag m (ExpressionRef m)
convertExpression ptr =
  convertNode
  ConvertParams { cpParentInfo = ExpressionTop, cpPtr = ptr }
