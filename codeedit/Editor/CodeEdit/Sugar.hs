{-# LANGUAGE TemplateHaskell #-}

module Editor.CodeEdit.Sugar
  ( Expression(..), Actions(..), ExpressionRef(..)
  , Where(..), WhereItem(..)
  , Func(..), FuncParam(..)
  , Apply(..), Section(..), Hole(..)
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
  { rExpressionPtr :: ExpressionPtr m
  , rExpression :: Expression m
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

data Hole m = Hole
  { holeState :: Data.HoleState
  , holeMFlipFuncArg :: Maybe (Transaction ViewTag m ())
  }

data Expression m
  = ExpressionApply   { eHasParens :: HasParens, eApply :: Apply m }
  | ExpressionSection { eHasParens :: HasParens, eSection :: Section m }
  | ExpressionWhere   { eHasParens :: HasParens, eWhere :: Where m }
  | ExpressionFunc    { eHasParens :: HasParens, eFunc :: Func m }
  | ExpressionGetVariable { _eGetVar :: Data.VariableRef }
  | ExpressionHole { eHole :: Hole m }
  | ExpressionLiteralInteger { _eLit :: Integer }

AtFieldTH.make ''Hole
AtFieldTH.make ''Where
AtFieldTH.make ''Func
AtFieldTH.make ''ExpressionRef
AtFieldTH.make ''Actions
AtFieldTH.make ''Apply
AtFieldTH.make ''Section
AtFieldTH.make ''Expression

type Convertor m = ExpressionPtr m -> Transaction ViewTag m (ExpressionRef m)

addArg :: Monad m => ExpressionPtr m -> Transaction ViewTag m Guid
addArg = liftM IRef.guid . DataOps.callWithArg

makeActions :: Monad m => Guid -> ExpressionPtr m -> Actions m
makeActions g ptr =
  Actions
  { guid = g
  , addNextArg = addArg ptr
  , callWithArg = addArg ptr
  , giveAsArg = liftM IRef.guid $ DataOps.giveAsArg ptr
  , lambdaWrap = liftM IRef.guid $ DataOps.lambdaWrap ptr
    -- Hole will remove mReplace because no point replacing hole with hole.
  , mReplace = Just . liftM IRef.guid $ DataOps.replaceWithHole ptr
    -- mDelete gets overridden by parent if it is an apply.
  , mDelete = Nothing
  , mNextArg = Nothing
  }

mkExpressionRef :: Monad m => ExpressionPtr m -> Expression m -> Transaction ViewTag m (ExpressionRef m)
mkExpressionRef ptr expr = do
  iref <- Property.get ptr
  return
    ExpressionRef
    { rExpression = expr
    , rExpressionPtr = ptr
    , rActions = makeActions (IRef.guid iref) ptr
    }

convertLambda :: Monad m => Data.Lambda -> Convertor m
convertLambda lambda ptr = do
  exprI <- Property.get ptr
  let
    typePtr = DataOps.lambdaParamTypeRef exprI lambda
    bodyPtr = DataOps.lambdaBodyRef exprI lambda
  typeExpr <- convertExpression typePtr
  sBody <- convertExpression bodyPtr
  bodyI <- Property.get bodyPtr
  let
    deleteArg = do
      Property.set ptr bodyI
      return . guid $ rActions sBody
    item = FuncParam
      { fpActions =
          (atMDelete . const . Just) deleteArg $
          makeActions (IRef.guid exprI) ptr
      , fpType = typeExpr
      }
  mkExpressionRef ptr .
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
convertWhere valueRef funcI lambda@(Data.Lambda _ bodyI) ptr = do
  let
    bodyPtr = DataOps.lambdaBodyRef funcI lambda
    deleteItem = do
      Property.set ptr bodyI
      return $ IRef.guid bodyI
    item = WhereItem
      { wiActions =
          (atMDelete . const . Just) deleteItem $
          makeActions (IRef.guid funcI) ptr
      , wiValue = valueRef
      }
  sBody <- convertExpression bodyPtr
  mkExpressionRef ptr . ExpressionWhere DontHaveParens . atWWheres (item :) $
    case rExpression sBody of
      ExpressionWhere _ x -> x
      _ -> Where [] sBody

addParensAtSection :: ExpressionRef m -> ExpressionRef m
addParensAtSection =
  atRExpression f
  where
    f (ExpressionSection _ section) = ExpressionSection HaveParens section
    f x = x

convertApply :: Monad m => Data.Apply -> Convertor m
convertApply apply@(Data.Apply funcI _) ptr = do
  exprI <- Property.get ptr
  func <- Transaction.readIRef funcI
  argRef <- convertExpression $ DataOps.applyArgRef exprI apply
  let prefixApply = convertApplyPrefix exprI apply argRef ptr
  case func of
    Data.ExpressionLambda lambda -> convertWhere argRef funcI lambda ptr
    -- InfixR or ordinary prefix:
    Data.ExpressionApply funcApply@(Data.Apply funcFuncI _) -> do
      mInfixOp <- Infix.infixOp funcFuncI
      case mInfixOp of
        Just op -> convertApplyInfixFull funcApply op exprI apply argRef ptr
        Nothing -> prefixApply
    -- InfixL or ordinary prefix:
    _ -> do
      mInfixOp <- Infix.infixOp funcI
      case mInfixOp of
        Just op -> convertApplyInfixL op exprI apply argRef ptr
        Nothing -> prefixApply

setAddArg :: Monad m => ExpressionPtr m -> ExpressionRef m -> ExpressionRef m
setAddArg = atRActions . atAddNextArg . const . addArg

convertApplyInfixFull
  :: Monad m
  => Data.Apply
  -> Data.VariableRef
  -> IRef Data.Expression
  -> Data.Apply
  -> ExpressionRef m
  -> Convertor m
convertApplyInfixFull funcApply@(Data.Apply funcFuncI funcArgI) op exprI apply@(Data.Apply funcI _) rArgRef ptr = do
  lArgRef <- convertExpression $ DataOps.applyArgRef funcI funcApply
  opRef <-
    mkExpressionRef (DataOps.applyFuncRef funcI funcApply) $
    ExpressionGetVariable op
  let
    newLArgRef =
      addDelete funcFuncI funcPtr .
      addParensAtSection $
      lArgRef
    newOpRef =
      addDelete funcArgI funcPtr .
      setAddArg ptr $
      opRef
    newRArgRef =
      addDelete funcI ptr .
      addParensAtSection $
      rArgRef
  mkExpressionRef ptr . ExpressionSection DontHaveParens $
    Section (Just newLArgRef) newOpRef (Just newRArgRef)
  where
    funcPtr = DataOps.applyFuncRef exprI apply

convertApplyInfixL
  :: Monad m
  => Data.VariableRef
  -> IRef Data.Expression
  -> Data.Apply
  -> ExpressionRef m
  -> Convertor m
convertApplyInfixL op exprI apply@(Data.Apply opI argI) argRef ptr = do
  opRef <-
    mkExpressionRef (DataOps.applyFuncRef exprI apply) $
    ExpressionGetVariable op
  let
    newOpRef =
      addDelete argI ptr .
      setAddArg ptr $
      opRef
  mkExpressionRef ptr . ExpressionSection HaveParens $
    Section (Just newArgRef) newOpRef Nothing
  where
    newArgRef =
      addDelete opI ptr .
      addParensAtSection $
      argRef

convertApplyPrefix
  :: Monad m
  => IRef Data.Expression
  -> Data.Apply
  -> ExpressionRef m
  -> Convertor m
convertApplyPrefix exprI apply@(Data.Apply funcI argI) argRef ptr = do
  funcRef <- convertExpression $ DataOps.applyFuncRef exprI apply
  let
    newFuncRef =
      addDelete argI ptr .
      setNextArg .
      addParensAtSection .
      (atRExpression . atEApply . atApplyArg) setNextArg $
      funcRef
  mkExpressionRef ptr . ExpressionApply DontHaveParens $
    Apply newFuncRef newArgRef
  where
    newArgRef =
      addDelete funcI ptr .
      setAddArg ptr .
      addFlipFuncArg .
      addParens $ argRef
    setNextArg = atRActions . atMNextArg . const . Just $ newArgRef
    addFlipFuncArg =
      atRExpression . atEHole . atHoleMFlipFuncArg . const . Just .
      Transaction.writeIRef exprI . Data.ExpressionApply $
      Data.Apply argI funcI
    addParens = atRExpression . atEHasParens . const $ HaveParens

addDelete :: Monad m => IRef Data.Expression -> ExpressionPtr m -> ExpressionRef m -> ExpressionRef m
addDelete replacer parentPtr =
  atRActions . atMDelete . const . Just $ do
    Property.set parentPtr replacer
    return $ IRef.guid replacer

convertGetVariable :: Monad m => Data.VariableRef -> Convertor m
convertGetVariable varRef ptr = do
  name <- Property.get $ Anchors.variableNameRef varRef
  getVarExpr <-
    mkExpressionRef ptr $
    ExpressionGetVariable varRef
  if Infix.isInfixName name
    then
      mkExpressionRef ptr $
      ExpressionSection HaveParens (Section Nothing getVarExpr Nothing)
    else return getVarExpr

convertHole :: Monad m => Data.HoleState -> Convertor m
convertHole state ptr =
  (liftM . atRActions . atMReplace . const) Nothing .
  mkExpressionRef ptr . ExpressionHole $ Hole state Nothing

convertLiteralInteger :: Monad m => Integer -> Convertor m
convertLiteralInteger i ptr =
  mkExpressionRef ptr $ ExpressionLiteralInteger i

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
