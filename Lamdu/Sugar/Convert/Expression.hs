{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeFamilies, Rank2Types #-}
module Lamdu.Sugar.Convert.Expression
  ( convert
  , jumpToDefI
  ) where

import Control.Applicative (Applicative(..), (<$>), (<$))
import Control.Lens.Operators
import Control.Monad (guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either.Utils (runMatcherT, justToLeft)
import Control.MonadA (MonadA)
import Data.Monoid (Monoid(..))
import Data.Store.Transaction (Transaction)
import Data.Traversable (traverse, sequenceA)
import Lamdu.Expr.IRef (DefI)
import Lamdu.Expr.Val (Val(..))
import Lamdu.Sugar.Convert.Expression.Actions (addActions)
import Lamdu.Sugar.Convert.Monad (ConvertM)
import Lamdu.Sugar.Internal
import Lamdu.Sugar.Types
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.UniqueId as UniqueId
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Sugar.Convert.Apply as ConvertApply
import qualified Lamdu.Sugar.Convert.GetVar as ConvertGetVar
import qualified Lamdu.Sugar.Convert.Binder as ConvertBinder
import qualified Lamdu.Sugar.Convert.Hole as ConvertHole
import qualified Lamdu.Sugar.Convert.List as ConvertList
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.Convert.Record as ConvertRecord
import qualified Lamdu.Sugar.Internal.EntityId as EntityId

type T = Transaction

convertLam ::
  (MonadA m, Monoid a) =>
  V.Lam (Val (InputPayload m a)) ->
  InputPayload m a -> ConvertM m (ExpressionU m a)
convertLam lam@(V.Lam _ lamBody) exprPl =
  do
    binder <- ConvertBinder.convertLam lam exprPl
    mDeleteLam <-
      sequenceA $ ConvertBinder.makeDeleteLambda
      <$> (lam & (traverse . traverse) (^. ipStored))
      <*> exprPl ^. ipStored
    let
      setToInnerExprAction =
        maybe NoInnerExpr SetToInnerExpr $ do
          guard $ Lens.nullOf ExprLens.valHole lamBody
          mDeleteLam
    BodyLam binder
      & addActions exprPl
      <&> rPayload . plActions . Lens._Just . setToInnerExpr .~ setToInnerExprAction

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

convertGetVar ::
  MonadA m =>
  V.Var -> InputPayload m a -> ConvertM m (ExpressionU m a)
convertGetVar param exprPl = do
  sugarContext <- ConvertM.readContext
  addActions exprPl $ BodyGetVar $ ConvertGetVar.convertVar sugarContext param

convert :: (MonadA m, Monoid a) => Val (InputPayload m a) -> ConvertM m (ExpressionU m a)
convert v =
  ($ v ^. V.payload) $
  case v ^. V.body of
  V.BAbs x -> convertLam x
  V.BApp x -> ConvertApply.convert x
  V.BRecExtend x -> ConvertRecord.convertExtend x
  V.BGetField x -> convertGetField x
  V.BLeaf (V.LVar x) -> convertGetVar x
  V.BLeaf (V.LGlobal x) -> convertGlobal x
  V.BLeaf (V.LLiteralInteger x) -> convertVLiteralInteger x
  V.BLeaf V.LHole -> ConvertHole.convert
  V.BLeaf V.LRecEmpty -> ConvertRecord.convertEmpty
