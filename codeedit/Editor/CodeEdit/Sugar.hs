{-# LANGUAGE TemplateHaskell #-}

module Editor.CodeEdit.Sugar
  ( Expression(..), ExpressionActions(..), ExpressionRef(..)
  , Where(..), WhereItem(..)
  , Func(..), FuncParam(..)
  , Apply(..), Hole(..)
  , ParensType(..)
  , convertExpression
  ) where

import Control.Monad(liftM)
import Data.Maybe(fromMaybe)
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
  { addNextArg :: Transaction ViewTag m Guid
  , lambdaWrap :: Transaction ViewTag m Guid
  , mReplace :: Maybe (Transaction ViewTag m Guid)
  , mDelete :: Maybe (Transaction ViewTag m Guid)
  , mNextArg :: Maybe (ExpressionRef m)
  }

data ExpressionRef m = ExpressionRef
  { rExpressionPtr :: ExpressionPtr m
  , rMParensType :: Maybe ParensType
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

data Apply m = Apply
  { applyFunc :: ExpressionRef m
  , applyArg :: ExpressionRef m
  }

data Hole m = Hole
  { holeState :: Data.HoleState
  , holeMFlipInfix :: Maybe (Transaction ViewTag m ())
  }

data Expression m
  = ExpressionApply (Apply m)
  | ExpressionGetVariable Data.VariableRef
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

mkExpressionRef :: Monad m => Bool -> Maybe ParensType -> ExpressionPtr m -> Expression m -> ExpressionRef m
mkExpressionRef isReplaceable mParensType ptr expr =
  ExpressionRef
  { rExpression = expr
  , rMParensType = mParensType
  , rExpressionPtr = ptr
  , rActions =
      ExpressionActions
      { addNextArg = liftM guid $ DataOps.callWithArg ptr
      , lambdaWrap = liftM guid $ DataOps.lambdaWrap ptr
      , mReplace =
          if isReplaceable
          then Just . liftM guid $ DataOps.replaceWithHole ptr
          else Nothing
        -- mDelete gets overridden by parent if it is an apply.
      , mDelete = Nothing
      , mNextArg = Nothing
      }
  }

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
  return . mkExpressionRef True Nothing ptr . ExpressionFunc . atFParams (item :) $ case rExpression sBody of
    ExpressionFunc x -> x
    _ -> Func [] sBody

convertWhere
  :: Monad m
  => ExpressionPtr m
  -> IRef Data.Expression
  -> Data.Lambda
  -> Convertor m
convertWhere argPtr funcI lambda@(Data.Lambda (Data.TypedParam paramI _) bodyI) ptr = do
  value <- convertExpression argPtr
  let
    bodyPtr = DataOps.lambdaBodyRef funcI lambda
    item = WhereItem
      { wiParamI = paramI
      , wiValue = value
      , wiApplyPtr = ptr
      , wiLambdaBodyI = bodyI
      }
  sBody <- convertExpression bodyPtr
  return . mkExpressionRef True Nothing ptr . ExpressionWhere . atWWheres (item :) $ case rExpression sBody of
    ExpressionWhere x -> x
    _ -> Where [] sBody

atExpressionApply :: (Apply m -> Apply m) -> Expression m -> Expression m
atExpressionApply f (ExpressionApply apply) = ExpressionApply $ f apply
atExpressionApply _ x = x

atExpressionHole :: (Hole m -> Hole m) -> Expression m -> Expression m
atExpressionHole f (ExpressionHole x) = ExpressionHole $ f x
atExpressionHole _ x = x

convertApply :: Monad m => Data.Apply -> Convertor m
convertApply (Data.Apply funcI argI) ptr = do
  exprI <- Property.get ptr
  let
    argPtr =
      Property (return argI) $
      Transaction.writeIRef exprI .
      Data.ExpressionApply . Data.Apply funcI
  func <- Transaction.readIRef funcI
  case func of
    Data.ExpressionLambda lambda ->
      convertWhere argPtr funcI lambda ptr
    _ -> do
      isInfixFunc <- Infix.isInfixFunc funcI
      isFullInfix <- Infix.isApplyOfInfixOp funcI
      let
        funcPtr =
          Property (return funcI) $
          Transaction.writeIRef exprI .
          Data.ExpressionApply . (`Data.Apply` argI)
        isInfix = isInfixFunc || isFullInfix
        deleteNode siblingI whereToGo =
          atRActions . atMDelete . const . Just $ do
            _ <- DataOps.replace ptr siblingI
            liftM guid whereToGo
        whereToGoAfterDeleteArg =
          liftM (fromMaybe funcI) (Infix.infixFuncOfRArg funcI)
        addArgHere = atRActions . atAddNextArg . const . liftM guid $ DataOps.callWithArg ptr
        addArgAfterFunc
          | isInfixFunc = addArgHere
          | otherwise = id
        addArgAfterArg
          | isInfix = id
          | otherwise = addArgHere
      funcExpr <- convertExpression funcPtr
      argExpr <- convertExpression argPtr
      needAddFuncParens <-
        case rExpression funcExpr of
        ExpressionGetVariable{} -> return False
        ExpressionApply (Apply funcFunc _) -> Infix.isApplyOfInfixOp =<< Property.get (rExpressionPtr funcFunc)
        _ -> return True
      let
        modifyFuncParens
          | needAddFuncParens = const . exprParensType $ rExpression funcExpr
          | isInfix = const Nothing
          | otherwise = id
        modifyArgParens
          | isInfix =
            case rExpression argExpr of
            ExpressionApply{} -> id
            _ -> const . exprParensType $ rExpression argExpr
          | otherwise = const . exprParensType $ rExpression argExpr
        mParensType
          | isInfixFunc = Just TextParens
          | otherwise = Nothing
        flipInfixFuncAndArg =
          Transaction.writeIRef exprI . Data.ExpressionApply $
          Data.Apply argI funcI
        newArgExpr =
          (atRExpression . atExpressionHole . atHoleMFlipInfix . const . Just) flipInfixFuncAndArg .
          atRMParensType modifyArgParens .
          addArgAfterArg .
          deleteNode funcI whereToGoAfterDeleteArg $
          argExpr
        setNextArg = atRActions . atMNextArg . const $ Just newArgExpr
        setFuncArgNextArg =
          atRExpression . atExpressionApply . atApplyArg $ setNextArg
      return . mkExpressionRef True mParensType ptr . ExpressionApply $
        Apply
        ((setFuncArgNextArg . setNextArg .
          atRMParensType modifyFuncParens .
          addArgAfterFunc .
          deleteNode argI (return argI)
         ) funcExpr)
        newArgExpr
  where
    exprParensType ExpressionHole{} = Nothing
    exprParensType ExpressionLiteralInteger{} = Nothing
    exprParensType ExpressionGetVariable{} = Just TextParens
    exprParensType ExpressionApply{} = Just TextParens
    exprParensType ExpressionFunc{} = Just TextParens
    exprParensType ExpressionWhere{} = Just SquareParens

convertGetVariable :: Monad m => Data.VariableRef -> Convertor m
convertGetVariable varRef ptr = do
  name <- Property.get $ Anchors.variableNameRef varRef
  let
    parens
      | Infix.isInfixName name = Just TextParens
      | otherwise = Nothing
  return . mkExpressionRef True parens ptr $ ExpressionGetVariable varRef

convertHole :: Monad m => Data.HoleState -> Convertor m
convertHole state ptr =
  return . mkExpressionRef False Nothing ptr . ExpressionHole $ Hole state Nothing

convertLiteralInteger :: Monad m => Integer -> Convertor m
convertLiteralInteger i ptr =
  return . mkExpressionRef True Nothing ptr $ ExpressionLiteralInteger i

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
