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
import Data.Store.Guid(Guid)
import Data.Store.IRef(IRef)
import Data.Store.Transaction(Transaction)
import Editor.Anchors(ViewTag)
import Editor.DataOps(ExpressionPtr)
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.Store.IRef as IRef
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
  = ExpressionApply { eHasParens :: HasParens, eApply :: Apply m }
  | ExpressionWhere { eHasParens :: HasParens, eWhere :: Where m }
  | ExpressionFunc  { eHasParens :: HasParens, eFunc :: Func m }
  | ExpressionGetVariable { _eGetVar :: GetVariable }
  | ExpressionHole { _eHole :: Hole m }
  | ExpressionLiteralInteger { _eLit :: Integer }

AtFieldTH.make ''Hole
AtFieldTH.make ''Where
AtFieldTH.make ''Func
AtFieldTH.make ''ExpressionRef
AtFieldTH.make ''ExpressionActions
AtFieldTH.make ''Apply
AtFieldTH.make ''Expression

type Convertor m = ExpressionPtr m -> Transaction ViewTag m (ExpressionRef m)

addArg :: Monad m => ExpressionPtr m -> Transaction ViewTag m Guid
addArg = liftM IRef.guid . DataOps.callWithArg

mkExpressionRef :: Monad m => ExpressionPtr m -> Expression m -> ExpressionRef m
mkExpressionRef ptr expr =
  ExpressionRef
  { rExpression = expr
  , rExpressionPtr = ptr
  , rActions =
      ExpressionActions
      { addNextArg = addArg ptr
      , lambdaWrap = liftM IRef.guid $ DataOps.lambdaWrap ptr
        -- Hole will remove mReplace because no point replacing hole with hole.
      , mReplace = Just . liftM IRef.guid $ DataOps.replaceWithHole ptr
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
  return . mkExpressionRef ptr .
    ExpressionFunc DontHaveParens . atFParams (item :) $
    case rExpression sBody of
      ExpressionFunc _ x -> x
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
  return . mkExpressionRef ptr . ExpressionWhere DontHaveParens . atWWheres (item :) $
    case rExpression sBody of
      ExpressionWhere _ x -> x
      _ -> Where [] sBody

atExpressionApply :: (Apply m -> Apply m) -> Expression m -> Expression m
atExpressionApply f e@ExpressionApply{} = e { eApply = f (eApply e) }
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
applyTypeOfFunc (ExpressionApply _ (Apply _ _ ApplyInfixL)) = ApplyInfixR
applyTypeOfFunc _ = ApplyPrefix

makeApplyArg
  :: Monad m
  => Bool
  -> (Transaction ViewTag m Guid
      -> Transaction ViewTag m Guid)
  -> IRef Data.Expression
  -> Data.Apply
  -> Transaction ViewTag m (ExpressionRef m)
makeApplyArg prefixArgNeedsParens addArgAfterArg exprI apply@(Data.Apply funcI argI) = do
  argExpr <- convertExpression $ DataOps.applyArgRef exprI apply
  let
    modifyArgParens =
      case (prefixArgNeedsParens, rExpression argExpr) of
      (False, ExpressionApply _ (Apply _ _ ApplyPrefix)) -> id
      _ -> const HaveParens
  let
    flipFuncAndArg =
      Transaction.writeIRef exprI . Data.ExpressionApply $
      Data.Apply argI funcI
  return $
    (atRExpression . atExpressionHole . atHoleMFlipFuncArg . const . Just) flipFuncAndArg .
    (atRExpression . atEHasParens) modifyArgParens .
    (atRActions . atAddNextArg) addArgAfterArg $
    argExpr

makeApplyFunc
  :: Monad m
  => (Transaction ViewTag m Guid
      -> Transaction ViewTag m Guid)
  -> IRef Data.Expression
  -> Data.Apply
  -> ExpressionRef m
  -> Transaction ViewTag m (ApplyType, ExpressionRef m)
makeApplyFunc addArgAfterFunc exprI apply argExpr = do
  funcExpr <- convertExpression $ DataOps.applyFuncRef exprI apply
  let
    modifyFuncParens =
      case rExpression funcExpr of
      ExpressionApply _ (Apply _ _ ApplyPrefix) -> id
      ExpressionApply _ (Apply _ _ ApplyInfixL) -> const DontHaveParens
      _ -> const HaveParens
    appType = applyTypeOfFunc $ rExpression funcExpr
    setNextArg = atRActions . atMNextArg . const $ Just argExpr
    setFuncArgNextArg =
      atRExpression . atExpressionApply . atApplyArg $ setNextArg
  return . ((,) appType) $
    setFuncArgNextArg . setNextArg .
    (atRExpression . atEHasParens) modifyFuncParens .
    (atRActions . atAddNextArg) addArgAfterFunc $ funcExpr

convertApplyNonLambda
  :: Monad m
  => Data.Apply
  -> IRef Data.Expression
  -> Convertor m
convertApplyNonLambda apply@(Data.Apply funcI _) exprI ptr = do
  isInfixL <- Infix.isInfixFunc funcI
  mInfixRFunc <- Infix.infixFuncOfRArg funcI
  let
    convertor =
      if isInfixL
      then convertApplyInfixL
      else
        case mInfixRFunc of
        Just infixRFunc -> convertApplyInfixR infixRFunc
        Nothing -> convertApplyPrefix
  convertor apply exprI ptr

convertApplyInfixR :: Monad m => IRef Data.Expression -> Data.Apply -> IRef Data.Expression -> Convertor m
convertApplyInfixR infixRFunc apply expr ptr =
  convertApplyI infixRFunc HaveParens False id id apply expr ptr

convertApplyInfixL :: Monad m => Data.Apply -> IRef Data.Expression -> Convertor m
convertApplyInfixL apply@(Data.Apply funcI _) expr ptr =
  convertApplyI funcI HaveParens False (const (addArg ptr)) id apply expr ptr

convertApplyPrefix :: Monad m => Data.Apply -> IRef Data.Expression -> Convertor m
convertApplyPrefix apply@(Data.Apply funcI _) expr ptr =
  convertApplyI funcI DontHaveParens True id (const (addArg ptr)) apply expr ptr

convertApplyI
  :: Monad m
  => IRef Data.Expression -> HasParens
  -> Bool
  -> (Transaction ViewTag m Guid -> Transaction ViewTag m Guid)
  -> (Transaction ViewTag m Guid -> Transaction ViewTag m Guid)
  -> Data.Apply -> IRef Data.Expression -> Convertor m
convertApplyI
  whereToGoAfterDeleteArg hasParens
  prefixArgNeedsParens addArgAfterFunc addArgAfterArg
  apply@(Data.Apply funcI argI) exprI ptr = do
  let
    deleteNode siblingI whereToGo =
      atRActions . atMDelete . const . Just $ do
        _ <- DataOps.replace ptr siblingI
        return $ IRef.guid whereToGo
  argExpr <- makeApplyArg prefixArgNeedsParens addArgAfterArg exprI apply
  (appType, funcExpr) <- makeApplyFunc addArgAfterFunc exprI apply argExpr
  return . mkExpressionRef ptr . ExpressionApply hasParens $
    Apply
      (deleteNode argI argI funcExpr)
      (deleteNode funcI whereToGoAfterDeleteArg argExpr)
      appType

convertGetVariable :: Monad m => Data.VariableRef -> Convertor m
convertGetVariable varRef ptr = do
  name <- Property.get $ Anchors.variableNameRef varRef
  let
    (_parens, infixType)
      | Infix.isInfixName name = (HaveParens, VariableInfix)
      | otherwise = (DontHaveParens, VariableNormal)
  return . mkExpressionRef ptr $
    ExpressionGetVariable {-TODO: parens-} (GetVariable varRef infixType)

convertHole :: Monad m => Data.HoleState -> Convertor m
convertHole state ptr =
  return .
  (atRActions . atMReplace . const) Nothing .
  mkExpressionRef ptr . ExpressionHole $ Hole state Nothing

convertLiteralInteger :: Monad m => Integer -> Convertor m
convertLiteralInteger i ptr =
  return . mkExpressionRef ptr $ ExpressionLiteralInteger i

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
