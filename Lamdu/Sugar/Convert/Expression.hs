{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeFamilies, Rank2Types #-}
module Lamdu.Sugar.Convert.Expression
  ( convert
  ) where

import           Control.Applicative (Applicative(..), (<$>), (<$))
import           Control.Lens.Operators
import           Control.Monad (guard)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Either.Utils (runMatcherT, justToLeft)
import           Control.MonadA (MonadA)
import qualified Data.Map as Map
import           Data.Monoid (Monoid(..))
import           Data.Store.Guid (Guid)
import           Data.Store.Transaction (Transaction)
import           Data.Traversable (traverse)
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import           Lamdu.Expr.IRef (DefI)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.UniqueId as UniqueId
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Sugar.Convert.Apply as ConvertApply
import qualified Lamdu.Sugar.Convert.Binder as ConvertBinder
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
import qualified Lamdu.Sugar.Convert.GetVar as ConvertGetVar
import qualified Lamdu.Sugar.Convert.Hole as ConvertHole
import qualified Lamdu.Sugar.Convert.Input as Input
import qualified Lamdu.Sugar.Convert.List as ConvertList
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.Convert.Record as ConvertRecord
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

type T = Transaction

jumpToDefI ::
  MonadA m => Anchors.CodeProps m -> DefI m -> T m EntityId
jumpToDefI cp defI = EntityId.ofIRef defI <$ DataOps.newPane cp defI

convertVLiteralInteger ::
  MonadA m => Integer ->
  Input.Payload m a -> ConvertM m (ExpressionU m a)
convertVLiteralInteger i exprPl = addActions exprPl $ BodyLiteralInteger i

convertGetFieldParam ::
  (MonadA m, MonadA n) =>
  V.GetField (Val a) ->
  ConvertM m (Maybe (GetVar Guid n))
convertGetFieldParam (V.GetField recExpr tag) =
  do
    tagParamInfos <- (^. ConvertM.scTagParamInfos) <$> ConvertM.readContext
    do
      paramInfo <- Map.lookup tag tagParamInfos
      param <- recExpr ^? ExprLens.valVar
      guard $ param == ConvertM.tpiFromParameters paramInfo
      Just $ GetVarNamed NamedVar
        { _nvName = UniqueId.toGuid tag
        , _nvJumpTo = pure (ConvertM.tpiJumpTo paramInfo)
        , _nvVarType = GetFieldParameter
        }
      & return

convertGetFieldNonParam ::
  (MonadA m, Monoid a) =>
  V.GetField (Val (Input.Payload m a)) -> EntityId ->
  ConvertM m (Body Guid m (ExpressionU m a))
convertGetFieldNonParam (V.GetField recExpr tag) entityId =
  GetField
  { _gfRecord = recExpr
  , _gfTag =
      TagG
      { _tagInstance = EntityId.ofGetFieldTag entityId
      , _tagVal = tag
      , _tagGName = UniqueId.toGuid tag
      }
  }
  & traverse ConvertM.convertSubexpression
  <&> BodyGetField

convertGetField ::
  (MonadA m, Monoid a) =>
  V.GetField (Val (Input.Payload m a)) ->
  Input.Payload m a ->
  ConvertM m (ExpressionU m a)
convertGetField getField exprPl =
  convertGetFieldParam getField
  >>= maybe (convertGetFieldNonParam getField entityId) (return . BodyGetVar)
  >>= addActions exprPl
  where
    entityId = exprPl ^. Input.entityId

convertGlobal ::
  MonadA m => V.GlobalId -> Input.Payload m a -> ConvertM m (ExpressionU m a)
convertGlobal globalId exprPl =
  runMatcherT $ do
    justToLeft $ ConvertList.nil globalId exprPl
    lift $ do
      cp <- (^. ConvertM.scCodeAnchors) <$> ConvertM.readContext
      addActions exprPl .
        BodyGetVar $ GetVarNamed NamedVar
        { _nvName = UniqueId.toGuid defI
        , _nvJumpTo = jumpToDefI cp defI
        , _nvVarType = GetDefinition
        }
    where
      defI = ExprIRef.defI globalId

convertGetVar ::
  MonadA m =>
  V.Var -> Input.Payload m a -> ConvertM m (ExpressionU m a)
convertGetVar param exprPl = do
  sugarContext <- ConvertM.readContext
  ConvertGetVar.convertVar sugarContext param
    (exprPl ^. Input.inferred . Infer.plType)
    & BodyGetVar
    & addActions exprPl

convert :: (MonadA m, Monoid a) => Val (Input.Payload m a) -> ConvertM m (ExpressionU m a)
convert v =
  ($ v ^. V.payload) $
  case v ^. V.body of
  V.BAbs x -> ConvertBinder.convertLam x
  V.BApp x -> ConvertApply.convert x
  V.BRecExtend x -> ConvertRecord.convertExtend x
  V.BGetField x -> convertGetField x
  V.BLeaf (V.LVar x) -> convertGetVar x
  V.BLeaf (V.LGlobal x) -> convertGlobal x
  V.BLeaf (V.LLiteralInteger x) -> convertVLiteralInteger x
  V.BLeaf V.LHole -> ConvertHole.convert
  V.BLeaf V.LRecEmpty -> ConvertRecord.convertEmpty
