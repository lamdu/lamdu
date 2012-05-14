{-# LANGUAGE TemplateHaskell #-}

module Editor.CodeEdit.Sugar
  ( Expression(..), Actions(..), ExpressionRef(..)
  , Where(..), WhereItem(..)
  , Func(..), FuncParam(..)
  , SimpleFuncType(..), BindingFuncType(..)
  , Apply(..), Section(..), Hole(..), LiteralInteger(..)
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

data SimpleFuncType m = SimpleFuncType
  { _sftParamType :: ExpressionRef m
  , _sftResultType :: ExpressionRef m
  , _sftTransformToBindingFuncType :: Transaction ViewTag m ()
  }

-- Deleting a BindingFuncType's parameter turns it into a SimpleFuncType
data BindingFuncType m = BindingFuncType
  { _bftParam :: FuncParam m
  , _bftResultType :: ExpressionRef m
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
  = ExpressionApply           { eHasParens :: HasParens, eApply :: Apply m }
  | ExpressionSection         { eHasParens :: HasParens, eSection :: Section m }
  | ExpressionWhere           { eHasParens :: HasParens, eWhere :: Where m }
  | ExpressionFunc            { eHasParens :: HasParens, eFunc :: Func m }
  | ExpressionSimpleFuncType  { eHasParens :: HasParens, eSFuncType :: SimpleFuncType m }
  | ExpressionBindingFuncType { eHasParens :: HasParens, eBFuncType :: BindingFuncType m }
  | ExpressionGetVariable { _eGetVar :: Data.VariableRef }
  | ExpressionHole { eHole :: Hole m }
  | ExpressionLiteralInteger { _eLit :: LiteralInteger m }
  | ExpressionPi

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

convertLambda :: Monad m => Data.Lambda -> Scope -> Convertor m
convertLambda lambda@(Data.Lambda paramTypeI bodyI) scope exprI setExprI = do
  let
    typeSetter = DataOps.lambdaParamTypeSetter exprI lambda
    bodySetter = DataOps.lambdaBodySetter exprI lambda
  typeExpr <- convertNode scope paramTypeI typeSetter
  sBody <- convertNode (Data.ParameterRef exprI : scope) bodyI bodySetter
  let
    deleteArg = do
      setExprI bodyI
      return . guid $ rActions sBody
    item = FuncParam
      { fpActions =
          (atMDelete . const . Just) deleteArg $
          makeActions exprI setExprI
      , fpType = typeExpr
      }
  mkExpressionRef exprI setExprI .
    ExpressionFunc DontHaveParens . atFParams (item :) $
    case rExpression sBody of
      ExpressionFunc _ x -> x
      _ -> Func [] sBody

convertWhere
  :: Monad m
  => ExpressionRef m
  -> IRef Data.Expression
  -> Data.Lambda
  -> Scope
  -> Convertor m
convertWhere valueRef lambdaI lambda@(Data.Lambda _ bodyI) scope applyI setApplyI = do
  let
    bodySetter = DataOps.lambdaBodySetter lambdaI lambda
    deleteItem = do
      setApplyI bodyI
      return $ IRef.guid bodyI
    item = WhereItem
      { wiActions =
          (atMDelete . const . Just) deleteItem $
          makeActions lambdaI setApplyI
      , wiValue = valueRef
      }
  sBody <- convertNode (Data.ParameterRef lambdaI : scope) bodyI bodySetter
  mkExpressionRef applyI setApplyI . ExpressionWhere DontHaveParens . atWWheres (item :) $
    case rExpression sBody of
      ExpressionWhere _ x -> x
      _ -> Where [] sBody

addParensAtSection :: ExpressionRef m -> ExpressionRef m
addParensAtSection =
  atRExpression f
  where
    f (ExpressionSection _ section) = ExpressionSection HaveParens section
    f x = x

convertApply :: Monad m => Data.Apply -> Scope -> Convertor m
convertApply apply@(Data.Apply funcI argI) scope exprI setExprI = do
  func <- Transaction.readIRef funcI
  argRef <- convertNode scope argI $ DataOps.applyArgSetter exprI apply
  let prefixApply = convertApplyPrefix apply argRef scope exprI setExprI
  case func of
    Data.ExpressionLambda lambda -> convertWhere argRef funcI lambda scope exprI setExprI
    -- InfixR or ordinary prefix:
    Data.ExpressionApply funcApply@(Data.Apply funcFuncI _) -> do
      mInfixOp <- Infix.infixOp funcFuncI
      case mInfixOp of
        Just op -> convertApplyInfixFull funcApply op apply argRef scope exprI setExprI
        Nothing -> prefixApply
    -- InfixL or ordinary prefix:
    _ -> do
      mInfixOp <- Infix.infixOp funcI
      case mInfixOp of
        Just op -> convertApplyInfixL op apply argRef exprI setExprI
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
  -> ExpressionRef m
  -> Scope
  -> Convertor m
convertApplyInfixFull funcApply@(Data.Apply funcFuncI funcArgI) op apply@(Data.Apply funcI _) rArgRef scope exprI setExprI = do
  lArgRef <- convertNode scope funcArgI $ DataOps.applyArgSetter funcI funcApply
  opRef <-
    mkExpressionRef funcFuncI (DataOps.applyFuncSetter funcI funcApply) $
    ExpressionGetVariable op
  let
    newLArgRef =
      addDelete funcSetter funcFuncI .
      addParensAtSection $
      lArgRef
    newOpRef =
      addDelete funcSetter funcArgI .
      setAddArg exprI setExprI $
      opRef
    newRArgRef =
      addDelete setExprI funcI .
      addParensAtSection $
      rArgRef
  mkExpressionRef exprI setExprI . ExpressionSection DontHaveParens $
    Section (Just newLArgRef) newOpRef (Just newRArgRef)
  where
    funcSetter = DataOps.applyFuncSetter exprI apply

convertApplyInfixL
  :: Monad m
  => Data.VariableRef
  -> Data.Apply
  -> ExpressionRef m
  -> Convertor m
convertApplyInfixL op apply@(Data.Apply opI argI) argRef exprI setExprI = do
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
  where
    newArgRef =
      addDelete setExprI opI .
      addParensAtSection $
      argRef

convertApplyPrefix
  :: Monad m
  => Data.Apply
  -> ExpressionRef m
  -> Scope
  -> Convertor m
convertApplyPrefix apply@(Data.Apply funcI argI) argRef scope exprI setExprI = do
  funcRef <- convertNode scope funcI $ DataOps.applyFuncSetter exprI apply
  let
    newFuncRef =
      addDelete setExprI argI .
      setNextArg .
      addParensAtSection .
      (atRExpression . atEApply . atApplyArg) setNextArg $
      funcRef
  mkExpressionRef exprI setExprI . ExpressionApply DontHaveParens $
    Apply newFuncRef newArgRef
  where
    newArgRef =
      addDelete setExprI funcI .
      setAddArg exprI setExprI .
      addFlipFuncArg .
      addParens $ argRef
    setNextArg = atRActions . atMNextArg . const . Just $ newArgRef
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

convertPi :: Monad m => Convertor m
convertPi exprI setExprI =
  mkExpressionRef exprI setExprI ExpressionPi

convertNode :: Monad m => Scope -> Convertor m
convertNode scope exprI setExprI = do
  expr <- Transaction.readIRef exprI
  let
    conv =
      case expr of
      Data.ExpressionLambda x -> convertLambda x scope
      Data.ExpressionApply x -> convertApply x scope
      Data.ExpressionGetVariable x -> convertGetVariable x
      Data.ExpressionHole -> convertHole scope
      Data.ExpressionLiteralInteger x -> convertLiteralInteger x
      Data.ExpressionPi -> convertPi
  conv exprI setExprI

convertExpression :: Monad m => Convertor m
convertExpression = convertNode []
