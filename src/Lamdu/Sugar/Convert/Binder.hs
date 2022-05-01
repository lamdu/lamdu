{-# LANGUAGE TypeApplications #-}

module Lamdu.Sugar.Convert.Binder
    ( convertDefinitionBinder, convertLam
    , convertBinder
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Extended ((~~>))
import           Control.Monad.Once (OnceT)
import qualified Data.Map as Map
import qualified Data.Property as Property
import qualified Data.Set as Set
import           Hyper
import           Hyper.Type.Prune (Prune)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Data.Ops.Subexprs as SubExprs
import           Lamdu.Expr.IRef (DefI)
import qualified Lamdu.Expr.IRef as ExprIRef
import           Lamdu.Expr.UniqueId (ToUUID(..))
import qualified Lamdu.Sugar.Config as Config
import           Lamdu.Sugar.Convert.Binder.Params (convertLamParams, convertEmptyParams)
import           Lamdu.Sugar.Convert.Binder.Types (BinderKind(..))
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.LightLam (addLightLambdas)
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

lamParamToHole ::
    Monad m =>
    V.TypedLam V.Var (HCompose Prune T.Type) V.Term # Ann (Input.Payload m) -> T m ()
lamParamToHole (V.TypedLam param _paramTyp x) =
    SubExprs.getVarsToHole param (x & hflipped %~ hmap (const (^. Input.stored)))

convertBinder ::
    Monad m =>
    ConvertM.PositionInfo ->
    Ann (Input.Payload m) # V.Term ->
    ConvertM m (Annotated (ConvertPayload m) # Binder EvalPrep InternalName (OnceT (T m)) (T m))
convertBinder pos expr =
    do
        convertSub <- Lens.view id <&> ConvertM.scConvertSubexpression
        convertSub pos expr
    >>= convertBinderBody expr
    & local (ConvertM.scScopeInfo %~ addPos)
    where
        addPos x =
            x
            & ConvertM.siExtractPos ?~
                ConvertM.OuterScopeInfo
                { ConvertM._osiPos = expr ^. hAnn . Input.stored
                , ConvertM._osiScope = expr ^. hAnn . Input.inferScope
                }
            & ConvertM.siFloatPos .~ x ^. ConvertM.siExtractPos

convertBinderBody ::
    Monad m =>
    Ann (Input.Payload m) # V.Term ->
    Annotated (ConvertPayload m) # Term EvalPrep InternalName (OnceT (T m)) (T m) ->
    ConvertM m (Annotated (ConvertPayload m) # Binder EvalPrep InternalName (OnceT (T m)) (T m))
convertBinderBody rawExpr expr =
    do
        supportLet <- Lens.view (ConvertM.scConfig . Config.sugarsEnabled . Config.letExpression)
        case expr ^. hVal of
            BodySimpleApply (App (Ann lamPl (BodyLam lam)) argT) | supportLet ->
                do
                    postProcess <- ConvertM.postProcessAssert
                    let del =
                            do
                                traverse_ (`SubExprs.getVarsToHole` (rawExpr & hflipped %~ hmap (const (^. Input.stored)))) mVar
                                (lam ^. lamFunc . fBody . annotation . pStored
                                    & replaceWith topStored)
                                    <* postProcess
                    convertBinderBody rawExpr argT >>= toAssignment binderKind <&>
                        \argA ->
                        expr
                        & annValue .~
                            BinderLet Let
                            { _lValue = argA & annotation . pActions . delete . _Delete .~ del
                            , _lNames = lam ^. lamFunc . fParams & _LhsVar . vDelete .~ void del
                            , _lBody = lam ^. lamFunc . fBody
                            }
                        & annotation . pLambdas <>~ [toUUID (lamPl ^. Lens._Wrapped . pStored . ExprIRef.iref)]
                    where
                        mLam = rawExpr ^? hVal . V._BApp . V.appFunc . hVal . V._BLam
                        mVar = mLam ^? Lens._Just . V.tlIn
                        binderKind =
                            maybe BinderKindLambda -- <- Shouldn't happen?
                            (BinderKindLet . hmap (Proxy @(Recursively HFunctor) #> hflipped %~ hmap (const (^. Input.stored))))
                            mLam
            _ -> expr & annValue %~ BinderTerm & pure
    <&> annValue %~ Binder (DataOps.redexWrap topStored <&> EntityId.ofValI)
    where
        topStored = expr ^. annotation . pStored

makeFunction ::
    Monad m =>
    ConvertM.PositionInfo ->
    V.TypedLam V.Var (HCompose Prune T.Type) V.Term # Ann (Input.Payload m) ->
    Input.Payload m # V.Term ->
    ConvertM m (Function EvalPrep InternalName (OnceT (T m)) (T m) # Annotated (ConvertPayload m))
makeFunction pos lam exprPl =
    do
        params <-
            convertLamParams
            ( case pos of
                ConvertM.TopPos v -> BinderKindDef (ExprIRef.defI v)
                _ -> BinderKindLambda
            )
            lam exprPl
        let addParams ctx =
                ctx
                & ConvertM.siRecordParams <>~
                    ( case params of
                        LhsRecord r ->
                            (lam ^. V.tlIn) ~~>
                            Set.fromList (r ^.. SugarLens.taggedListItems >>= mkTags)
                        _ -> Map.empty
                    )
                & ConvertM.siNullParams <>~
                case params of
                LhsVar v | v ^. vIsNullParam -> Set.singleton (lam ^. V.tlIn)
                _ -> Set.empty
        assignmentBody <-
            convertBinder ConvertM.BinderPos (lam ^. V.tlOut)
            & local (ConvertM.scScopeInfo %~ addParams)
        pure
            Function
            { _fParams = params
            , _fChosenScopeProp = chosenScopeProp ^. Property.mkProperty & lift
            , _fBody = assignmentBody
            , _fBodyScopes = mempty
            }
    where
        chosenScopeProp = lam ^. V.tlIn & Anchors.assocScopeRef
        mkTags item =
            [] : (item ^.. tiValue . fSubFields . Lens._Just . traverse . _1 . tagVal <&> (:[]))
            <&> (t:)
            where
                t = item ^. tiTag . tagRefTag . tagVal

convertLam ::
    Monad m =>
    ConvertM.PositionInfo ->
    V.TypedLam V.Var (HCompose Prune T.Type) V.Term # Ann (Input.Payload m) ->
    Input.Payload m # V.Term ->
    ConvertM m (ExpressionU EvalPrep m)
convertLam pos lam exprPl =
    makeFunction pos lam exprPl
    >>= addActions (Ann exprPl (V.BLam lam)) . BodyLam . Lambda False UnlimitedFuncApply
    <&> hVal %~ hmap (const (annotation . pActions . mReplaceParent . Lens._Just %~ (lamParamToHole lam >>)))

toAssignment ::
    Monad m =>
    BinderKind m ->
    Annotated (ConvertPayload m) # Binder v name i (T m) ->
    ConvertM m (Annotated (ConvertPayload m) # Assignment v name i (T m))
toAssignment binderKind b =
    do
        enabled <- Lens.view (ConvertM.scConfig . Config.sugarsEnabled . Config.assignmentParameters)
        case b ^? hVal . bBody . _BinderTerm . _BodyLam of
            Just l | enabled -> b & annValue .~ BodyFunction (l ^. lamFunc) & pure
            _ ->
                convertEmptyParams binderKind (b ^. annotation . pUnsugared)
                <&> \addFirstParam -> b & annValue %~ BodyPlain . AssignPlain addFirstParam

convertDefinitionBinder ::
    Monad m =>
    DefI m -> Ann (Input.Payload m) # V.Term ->
    ConvertM m (Annotated (ConvertPayload m) # Assignment EvalPrep InternalName (OnceT (T m)) (T m))
convertDefinitionBinder defI t =
    addLightLambdas <*> (convertBinder (ConvertM.TopPos (ExprIRef.globalId defI)) t >>= toAssignment (BinderKindDef defI))
