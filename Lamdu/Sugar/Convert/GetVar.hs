{-# LANGUAGE NoImplicitPrelude, PatternGuards #-}
module Lamdu.Sugar.Convert.GetVar
    ( convert
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Monad (guard)
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.Either.Utils (runMatcherT, justToLeft)
import           Control.Monad.Trans.Maybe (MaybeT)
import           Control.MonadA (MonadA)
import           Data.Maybe (fromMaybe)
import           Data.Maybe.Utils (maybeToMPlus)
import           Data.Store.Guid (Guid)
import           Lamdu.Builtins.Anchors (recurseVar)
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

convertRecurse ::
    MonadA m => V.Var -> MaybeT (ConvertM m) (GetVar Guid m)
convertRecurse param =
    do
        guard (param == recurseVar)
        defI <-
            ConvertM.readContext & lift
            <&> (^. ConvertM.scDefI)
            <&> fromMaybe (error "recurseVar used not in definition context?!")
        GetBinder BinderVar
            { _bvNameRef =
                NameRef
                { _nrName = UniqueId.toGuid defI
                , _nrGotoDefinition = pure $ EntityId.ofIRef defI
                }
            , _bvForm = GetDefinition
            , _bvInline = CannotInline
            } & return

usesAround :: Eq a => a -> [a] -> [a]
usesAround x xs =
    drop 1 after ++ before
    where
        (before, after) = break (== x) xs

paramNameRef :: MonadA m => V.Var -> NameRef Guid m
paramNameRef param =
    NameRef
    { _nrName = UniqueId.toGuid param
    , _nrGotoDefinition = pure $ EntityId.ofLambdaParam param
    }

convertGetBinder ::
    MonadA m => V.Var -> Input.Payload m a -> MaybeT (ConvertM m) (GetVar Guid m)
convertGetBinder param exprPl =
    do
        inline <-
            lift ConvertM.readContext
            <&> (^. ConvertM.scScopeInfo . ConvertM.siLetItems . Lens.at param)
            >>= maybeToMPlus
        GetBinder BinderVar
            { _bvNameRef = paramNameRef param
            , _bvForm = GetLet
            , _bvInline =
                inline
                & _CannotInlineDueToUses %~ usesAround (exprPl ^. Input.entityId)
            } & return

convertParamsRecord ::
    MonadA m => V.Var -> Input.Payload m a -> MaybeT (ConvertM m) (GetVar Guid m)
convertParamsRecord param exprPl =
    do
        lift ConvertM.readContext
            <&> (^.. ConvertM.scScopeInfo . siTagParamInfos . Lens.traversed
                . ConvertM._TagFieldParam)
            <&> map tpiFromParameters
            <&> elem param
            >>= guard
        GetParamsRecord ParamsRecordVar
            { _prvFieldNames =
                exprPl
                ^.. Input.inferredType . ExprLens._TRecord . ExprLens.compositeTags
                <&> UniqueId.toGuid
            } & return

convert ::
    MonadA m =>
    V.Var -> Input.Payload m a -> ConvertM m (ExpressionU m a)
convert param exprPl =
    do
        convertRecurse param & justToLeft
        convertGetBinder param exprPl & justToLeft
        convertParamsRecord param exprPl & justToLeft
        GetParam (Param (paramNameRef param) GetParameter NormalBinder) & return
    & runMatcherT
    <&> BodyGetVar
    >>= addActions exprPl
