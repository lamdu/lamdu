module Lamdu.Sugar.Convert.Binder
    ( convertDefinitionBinder, convertLam
    , convertBinder
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Once (OnceT)
import qualified Data.Map as Map
import           Data.Property (MkProperty')
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
import           Lamdu.Sugar.Convert.Binder.Params (ConventionalParams(..), convertLamParams, convertEmptyParams, cpParams, cpMLamParam)
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
    Ann (Input.Payload m) # V.Term ->
    ConvertM m (Annotated (ConvertPayload m) # Binder EvalPrep InternalName (OnceT (T m)) (T m))
convertBinder expr =
    do
        convertSub <- Lens.view id <&> \env -> ConvertM.scConvertSubexpression env
        convertSub ConvertM.BinderPos expr
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
                    convertBinderBody rawExpr argT >>= toAssignment BinderKindLambda rawExpr <&>
                        \argA ->
                        expr
                        & annValue .~
                            BinderLet Let
                            { _lValue = argA
                            , _lNames =
                                lam ^. lamFunc . fParams
                                & _ParamVar . vDelete .~
                                do
                                    traverse_ (`SubExprs.getVarsToHole` (rawExpr & hflipped %~ hmap (const (^. Input.stored)))) mVar
                                    lam ^. lamFunc . fBody . annotation . pStored
                                        & replaceWith topStored & void
                                    postProcess
                            , _lBody = lam ^. lamFunc . fBody
                            }
                        & annotation . pLambdas <>~ [toUUID (lamPl ^. Lens._Wrapped . pStored . ExprIRef.iref)]
                    where
                        mVar = rawExpr ^? hVal . V._BApp . V.appFunc . hVal . V._BLam . V.tlIn
            _ -> expr & annValue %~ BinderTerm & pure
    <&> annValue %~ Binder (DataOps.redexWrap topStored <&> EntityId.ofValI)
    where
        topStored = expr ^. annotation . pStored

makeFunction ::
    Monad m =>
    MkProperty' (T m) (Maybe BinderParamScopeId) ->
    ConventionalParams m -> Ann (Input.Payload m) # V.Term ->
    ConvertM m (Function EvalPrep InternalName (OnceT (T m)) (T m) # Annotated (ConvertPayload m))
makeFunction chosenScopeProp params funcBody =
    convertBinder funcBody
    <&> mkRes
    & local (ConvertM.scScopeInfo %~ addParams)
    where
        mkRes assignmentBody =
            Function
            { _fParams =
                -- TODO: avoid partiality here
                params ^?! cpParams . Lens._Just
            , _fChosenScopeProp = chosenScopeProp ^. Property.mkProperty & lift
            , _fBody = assignmentBody
            , _fBodyScopes = mempty
            }
        addParams ctx =
            ctx
            & ConvertM.siRecordParams <>~
                ( case params ^. cpParams of
                    Just (ParamsRecord r) ->
                        p
                        <&> (,) ?? Set.fromList (r ^.. SugarLens.taggedListItems . tiTag . tagRefTag . tagVal)
                        & Map.fromList
                    _ -> Map.empty
                )
            & ConvertM.siNullParams <>~
            case params ^. cpParams of
            Just (ParamVar v) | v ^. vIsNullParam -> Set.fromList p
            _ -> Set.empty
            where
                p = params ^.. cpMLamParam .Lens._Just . _2

convertLam ::
    Monad m =>
    V.TypedLam V.Var (HCompose Prune T.Type) V.Term # Ann (Input.Payload m) ->
    Input.Payload m # V.Term ->
    ConvertM m (ExpressionU EvalPrep m)
convertLam lam exprPl =
    do
        convParams <- convertLamParams lam exprPl
        func <-
            makeFunction
            (lam ^. V.tlIn & Anchors.assocScopeRef)
            convParams (lam ^. V.tlOut)
        Lambda False UnlimitedFuncApply func & BodyLam
            & addActions (Ann exprPl (V.BLam lam))
            <&> hVal %~
                hmap (const (annotation . pActions . mReplaceParent . Lens._Just %~ (lamParamToHole lam >>)))

-- Let-item or definition (form of <name> [params] = <body>)
convertAssignment ::
    Monad m =>
    BinderKind m -> V.Var ->
    Ann (Input.Payload m) # V.Term ->
    ConvertM m
    ( Maybe (MkProperty' (T m) PresentationMode)
    , Annotated (ConvertPayload m) # Assignment EvalPrep InternalName (OnceT (T m)) (T m)
    )
convertAssignment binderKind defVar expr =
    convertBinder expr
    >>= toAssignment binderKind expr <&>
    \r ->
    ( presMode <$ r ^? hVal . _BodyFunction . fParams . _ParamsRecord . tlItems . Lens._Just . tlTail . traverse
    , r
    )
    where
        presMode = Anchors.assocPresentationMode defVar

toAssignment ::
    Monad m =>
    BinderKind m -> Ann (Input.Payload m) # V.Term ->
    Annotated (ConvertPayload m) # Binder v name i (T m) ->
    ConvertM m (Annotated (ConvertPayload m) # Assignment v name i (T m))
toAssignment binderKind expr b =
    do
        enabled <- Lens.view (ConvertM.scConfig . Config.sugarsEnabled . Config.assignmentParameters)
        case b ^? hVal . bBody . _BinderTerm . _BodyLam of
            Just l | enabled && not (l ^. lamLightweight) -> b & annValue .~ BodyFunction (l ^. lamFunc) & pure
            _ -> convertEmptyParams binderKind expr <&> \addFirstParam -> b & annValue %~ BodyPlain . AssignPlain addFirstParam

convertDefinitionBinder ::
    Monad m =>
    DefI m -> Ann (Input.Payload m) # V.Term ->
    ConvertM m
    ( Maybe (MkProperty' (T m) PresentationMode)
    , Annotated (ConvertPayload m)
        # Assignment EvalPrep InternalName (OnceT (T m)) (T m)
    )
convertDefinitionBinder defI t =
    (addLightLambdas <&> (_2 %~)) <*> convertAssignment (BinderKindDef defI) (ExprIRef.globalId defI) t
