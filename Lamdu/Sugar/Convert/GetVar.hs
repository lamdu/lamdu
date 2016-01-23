{-# LANGUAGE NoImplicitPrelude, PatternGuards #-}
module Lamdu.Sugar.Convert.GetVar
    ( convert
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.MonadA (MonadA)
import           Data.Store.Guid (Guid)
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.UniqueId as UniqueId
import qualified Lamdu.Expr.Val as V
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM, siTagParamInfos, tpiFromParameters)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

import           Prelude.Compat

usesAround :: Eq a => a -> [a] -> [a]
usesAround x xs =
    drop 1 after ++ before
    where
        (before, after) = break (== x) xs

convertH ::
    MonadA m => ConvertM.Context m -> V.Var -> Input.Payload m a -> GetVar Guid m
convertH sugarContext param exprPl
    | Just inline <- scopeInfo ^. ConvertM.siLetItems . Lens.at param =
      GetBinder BinderVar
      { _bvNameRef = paramNameRef
      , _bvForm = GetLet
      , _bvInline =
        inline & _CannotInlineDueToUses %~ usesAround (exprPl ^. Input.entityId)
      }

    | isGetParamRecord =
      GetParamsRecord ParamsRecordVar
      { _prvFieldNames =
          exprPl ^.. Input.inferredType . ExprLens._TRecord . ExprLens.compositeTags <&> UniqueId.toGuid
      }

    | otherwise =
      GetParam (Param paramNameRef GetParameter NormalBinder)

    where
        paramNameRef =
            NameRef
            { _nrName = UniqueId.toGuid param
            , _nrGotoDefinition = pure $ EntityId.ofLambdaParam param
            }
        isGetParamRecord =
            scopeInfo ^..
            siTagParamInfos . Lens.traversed . ConvertM._TagFieldParam
            & map tpiFromParameters
            & elem param
        scopeInfo = sugarContext ^. ConvertM.scScopeInfo

convert ::
    MonadA m =>
    V.Var -> Input.Payload m a -> ConvertM m (ExpressionU m a)
convert param exprPl =
    do
        sugarContext <- ConvertM.readContext
        convertH sugarContext param exprPl
            & BodyGetVar
            & addActions exprPl
