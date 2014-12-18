{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeFamilies, Rank2Types #-}
module Lamdu.Sugar.Convert.Expression
  ( convert
  , convertPositionalFuncParam
  , deleteParamRef
  , lambdaWrap
  , jumpToDefI
  ) where

import Control.Applicative (Applicative(..), (<$>), (<$))
import Control.Lens.Operators
import Control.Lens.Tuple
import Control.Monad (guard, void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either.Utils (runMatcherT, justToLeft)
import Control.MonadA (MonadA)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Data.Traversable (traverse)
import Lamdu.Expr.IRef (DefI)
import Lamdu.Expr.Val (Val(..))
import Lamdu.Sugar.Convert.Expression.Actions (addActions)
import Lamdu.Sugar.Convert.Monad (ConvertM)
import Lamdu.Sugar.Internal
import Lamdu.Sugar.Types
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.Store.Property as Property
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.UniqueId as UniqueId
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Sugar.Convert.Apply as ConvertApply
import qualified Lamdu.Sugar.Convert.Hole as ConvertHole
import qualified Lamdu.Sugar.Convert.List as ConvertList
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.Convert.Record as ConvertRecord
import qualified Lamdu.Sugar.Internal.EntityId as EntityId

type T = Transaction

onMatchingSubexprs ::
  MonadA m => (a -> m ()) -> (a -> Val () -> Bool) -> Val a -> m ()
onMatchingSubexprs action predicate =
  Lens.itraverseOf_ (ExprLens.subExprPayloads . Lens.ifiltered (flip predicate))
  (const action)

toHole :: MonadA m => ExprIRef.ValIProperty m -> T m ()
toHole = void . DataOps.setToHole

isGetParamOf :: V.Var -> Val a -> Bool
isGetParamOf = Lens.anyOf ExprLens.valVar . (==)

deleteParamRef ::
  MonadA m => V.Var -> Val (ExprIRef.ValIProperty m) -> T m ()
deleteParamRef = onMatchingSubexprs toHole . const . isGetParamOf

lambdaWrap :: MonadA m => ExprIRef.ValIProperty m -> T m EntityId
lambdaWrap stored =
  f <$> DataOps.lambdaWrap stored
  where
    f (newParam, _) = EntityId.ofLambdaParam newParam

mkPositionalFuncParamActions ::
  MonadA m => V.Var -> ExprIRef.ValIProperty m -> Val (ExprIRef.ValIProperty m) -> FuncParamActions m
mkPositionalFuncParamActions param lambdaProp body =
  FuncParamActions
  { _fpListItemActions =
    ListItemActions
    { _itemDelete = do
        deleteParamRef param body
        replaceWith lambdaProp $ body ^. V.payload
    , _itemAddNext = lambdaWrap $ body ^. V.payload
    }
  }

convertPositionalFuncParam ::
  (MonadA m, Monoid a) => V.Lam (Val (InputPayload m a)) ->
  InputPayload m a ->
  ConvertM m (FuncParam Guid m)
convertPositionalFuncParam (V.Lam param body) lamExprPl =
  pure FuncParam
  { _fpName = UniqueId.toGuid param
  , _fpVarKind = FuncParameter
  , _fpId = paramEntityId
  , _fpInferredType = paramType
  , _fpMActions =
    mkPositionalFuncParamActions param
    <$> lamExprPl ^. ipStored
    <*> traverse (^. ipStored) body
  , _fpHiddenIds = []
  }
  where
    paramEntityId = EntityId.ofLambdaParam param
    paramType =
      fromMaybe (error "Lambda value not inferred to a function type?!") $
      lamExprPl ^? ipInferred . Infer.plType . ExprLens._TFun . _1

convertLam ::
  (MonadA m, Monoid a) =>
  V.Lam (Val (InputPayload m a)) ->
  InputPayload m a -> ConvertM m (ExpressionU m a)
convertLam lam@(V.Lam paramVar lamBody) exprPl = do
  param <- convertPositionalFuncParam lam exprPl
  lamBodyS <- ConvertM.convertSubexpression lamBody
  protectedSetToVal <- ConvertM.typeProtectedSetToVal
  let
    setToInnerExprAction =
      maybe NoInnerExpr SetToInnerExpr $ do
        guard $ Lens.nullOf ExprLens.valHole lamBody
        lamBodyStored <- traverse (^. ipStored) lamBody
        stored <- exprPl ^. ipStored
        return $ do
          deleteParamRef paramVar lamBodyStored
          let lamBodyI = Property.value (lamBodyStored ^. V.payload)
          protectedSetToVal stored lamBodyI <&> EntityId.ofValI
  BodyLam
    Lam
    { _lParam =
        param
        & fpMActions .~ Nothing
    , _lResult = lamBodyS
    }
    & addActions exprPl
    <&> rPayload . plActions . Lens._Just . setToInnerExpr .~ setToInnerExprAction

convertVar ::
  MonadA m => V.Var ->
  InputPayload m a -> ConvertM m (ExpressionU m a)
convertVar param exprPl = do
  recordParamsMap <- (^. ConvertM.scRecordParamsInfos) <$> ConvertM.readContext
  let
    body =
      case Map.lookup param recordParamsMap of
      Just (ConvertM.RecordParamsInfo defName jumpTo) ->
        BodyGetParams GetParams
        { _gpDefName = defName
        , _gpJumpTo = jumpTo
        }
      Nothing ->
        BodyGetVar GetVar
        { _gvName = UniqueId.toGuid param
        , _gvJumpTo = pure $ EntityId.ofLambdaParam param
        , _gvVarType = GetParameter
        }
  addActions exprPl body

jumpToDefI ::
  MonadA m => Anchors.CodeProps m -> DefI m -> T m EntityId
jumpToDefI cp defI = EntityId.ofIRef defI <$ DataOps.newPane cp defI

convertVLiteralInteger ::
  MonadA m => Integer ->
  InputPayload m a -> ConvertM m (ExpressionU m a)
convertVLiteralInteger i exprPl = addActions exprPl $ BodyLiteralInteger i

convertGetField ::
  (MonadA m, Monoid a) =>
  V.GetField (Val (InputPayload m a)) ->
  InputPayload m a ->
  ConvertM m (ExpressionU m a)
convertGetField (V.GetField recExpr tag) exprPl = do
  tagParamInfos <- (^. ConvertM.scTagParamInfos) <$> ConvertM.readContext
  let
    mkGetVar jumpTo =
      pure GetVar
      { _gvName = UniqueId.toGuid tag
      , _gvJumpTo = pure jumpTo
      , _gvVarType = GetFieldParameter
      }
  mVar <- traverse mkGetVar $ do
    paramInfo <- Map.lookup tag tagParamInfos
    param <- recExpr ^? ExprLens.valVar
    guard $ param == ConvertM.tpiFromParameters paramInfo
    return $ ConvertM.tpiJumpTo paramInfo
  addActions exprPl =<<
    case mVar of
    Just var ->
      return $ BodyGetVar var
    Nothing ->
      traverse ConvertM.convertSubexpression
      GetField
      { _gfRecord = recExpr
      , _gfTag =
          TagG
          { _tagInstance = EntityId.ofGetFieldTag (exprPl ^. ipEntityId)
          , _tagVal = tag
          , _tagGName = UniqueId.toGuid tag
          }
      }
      <&> BodyGetField

convertGlobal ::
  MonadA m => V.GlobalId -> InputPayload m a -> ConvertM m (ExpressionU m a)
convertGlobal globalId exprPl =
  runMatcherT $ do
    justToLeft $ ConvertList.nil globalId exprPl
    lift $ do
      cp <- (^. ConvertM.scCodeAnchors) <$> ConvertM.readContext
      addActions exprPl .
        BodyGetVar $ GetVar
        { _gvName = UniqueId.toGuid defI
        , _gvJumpTo = jumpToDefI cp defI
        , _gvVarType = GetDefinition
        }
    where
      defI = ExprIRef.defI globalId

convert :: (MonadA m, Monoid a) => Val (InputPayload m a) -> ConvertM m (ExpressionU m a)
convert v =
  ($ v ^. V.payload) $
  case v ^. V.body of
  V.BAbs x -> convertLam x
  V.BApp x -> ConvertApply.convert x
  V.BRecExtend x -> ConvertRecord.convertExtend x
  V.BGetField x -> convertGetField x
  V.BLeaf (V.LVar x) -> convertVar x
  V.BLeaf (V.LGlobal x) -> convertGlobal x
  V.BLeaf (V.LLiteralInteger x) -> convertVLiteralInteger x
  V.BLeaf V.LHole -> ConvertHole.convert
  V.BLeaf V.LRecEmpty -> ConvertRecord.convertEmpty
