{-# LANGUAGE TypeApplications, DisambiguateRecordFields #-}
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
import           Lamdu.Expr.IRef (DefI, HRef)
import qualified Lamdu.Expr.IRef as ExprIRef
import           Lamdu.Expr.UniqueId (ToUUID(..))
import qualified Lamdu.Sugar.Config as Config
import           Lamdu.Sugar.Convert.Binder.Float (makeFloatLetToOuterScope)
import           Lamdu.Sugar.Convert.Binder.Inline (inlineLet)
import           Lamdu.Sugar.Convert.Binder.Params (ConventionalParams(..), convertLamParams, convertEmptyParams, cpParams, cpMLamParam, mkVarInfo)
import           Lamdu.Sugar.Convert.Binder.Redex (Redex(..))
import qualified Lamdu.Sugar.Convert.Binder.Redex as Redex
import           Lamdu.Sugar.Convert.Binder.Types (BinderKind(..))
import           Lamdu.Sugar.Convert.Expression.Actions (addActions, makeActions)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.LightLam (addLightLambdas)
import           Lamdu.Sugar.Convert.Monad (ConvertM(..), scScopeInfo, siLetItems)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.Convert.Tag as ConvertTag
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

makeInline ::
    Monad m =>
    HRef m # V.Term -> Redex # Input.Payload m -> EntityId -> VarInline (T m)
makeInline stored redex useId
    | Lens.has traverse otherUses = CannotInlineDueToUses (drop 1 after <> before)
    | otherwise =
        inlineLet stored (Redex.hmapRedex (const (^. Input.stored . ExprIRef.iref)) redex)
        & InlineVar
    where
        otherUses = filter (/= useId) uses
        uses = redex ^. Redex.paramRefs
        (before, after) = break (== useId) uses

convertLet ::
    Monad m =>
    Ann (Input.Payload m) # V.Term ->
    Redex # Input.Payload m ->
    ConvertM m (Annotated (ConvertPayload m ()) # BinderBody EvalPrep InternalName (OnceT (T m)) (T m))
convertLet src redex =
    do
        float <- makeFloatLetToOuterScope (pl ^. Input.stored . ExprIRef.setIref) redex
        vinfo <- mkVarInfo (argAnn ^. Input.inferredType)
        tag <- ConvertTag.taggedEntity (Just vinfo) param >>= ConvertM . lift
        (value, letBody, actions) <-
            (,,)
            <$> (convertAssignment binderKind param (redex ^. Redex.arg) <&> (^. _2))
            <*> ( convertBinder bod
                    & local (scScopeInfo . siLetItems <>~
                        Map.singleton param (makeInline stored redex))
                )
            <*> makeActions pl
            & localNewExtractDestPos pl
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        let fixValueNodeActions nodeActions =
                nodeActions
                & extract .~ float
                & mReplaceParent ?~
                    ( protectedSetToVal stored
                        (redex ^. Redex.arg . hAnn . Input.stored . ExprIRef.iref)
                        <&> EntityId.ofValI
                    )
        postProcess <- ConvertM.postProcessAssert
        let del =
                do
                    lamParamToHole (redex ^. Redex.lam)
                    redex ^. Redex.lam . V.tlOut . hAnn . Input.stored
                        & replaceWith stored & void
                <* postProcess
        pure Ann
            { _hVal =
                BinderLet Let
                { _lValue = value & annotation . pActions %~ fixValueNodeActions
                , _lDelete = del
                , _lName = tag
                , _lBody =
                    letBody
                    & annotation . pActions . mReplaceParent ?~
                        (letBody ^. annotation . pEntityId <$ del)
                    & annotation . pLambdas .~ [redex ^. Redex.lamPl . Input.stored . ExprIRef.iref & toUUID]
                , _lUsages = redex ^. Redex.paramRefs
                }
            , _hAnn =
                Const ConvertPayload
                { _pUnsugared = src
                , _pActions = actions
                , _pLambdas = []
                , _pUserData = ()
                , _pEntityId = src ^. hAnn . Input.entityId
                }
            }
    where
        pl = src ^. hAnn
        argAnn = redex ^. Redex.arg . hAnn
        stored = pl ^. Input.stored
        binderKind =
            redex ^. Redex.lam
            & hmap (Proxy @(Recursively HFunctor) #> hflipped %~ hmap (const (^. Input.stored)))
            & BinderKindLet
        V.TypedLam param _paramTyp bod = redex ^. Redex.lam

convertBinder ::
    Monad m =>
    Ann (Input.Payload m) # V.Term ->
    ConvertM m (Annotated (ConvertPayload m ()) # Binder EvalPrep InternalName (OnceT (T m)) (T m))
convertBinder expr =
    convertBinderBody expr
    <&> annValue %~ Binder (DataOps.redexWrap (expr ^. hAnn . Input.stored) <&> EntityId.ofValI)

convertBinderBody ::
    Monad m =>
    Ann (Input.Payload m) # V.Term ->
    ConvertM m (Annotated (ConvertPayload m ()) # BinderBody EvalPrep InternalName (OnceT (T m)) (T m))
convertBinderBody expr@(Ann pl body) =
    Lens.view (ConvertM.scConfig . Config.sugarsEnabled . Config.letExpression) >>=
    \case
    False -> convertExpr
    True ->
        case Redex.check body of
        Nothing -> convertExpr
        Just redex -> convertLet expr redex
    where
        convertExpr =
            do
                convertSub <- Lens.view id <&> \env -> ConvertM.scConvertSubexpression env
                convertSub ConvertM.BinderPos expr
            & localNewExtractDestPos pl
            <&> annValue %~ BinderTerm

localNewExtractDestPos ::
    Monad m => Input.Payload m # V.Term -> ConvertM m b -> ConvertM m b
localNewExtractDestPos x =
    ConvertM.scScopeInfo . ConvertM.siMOuter ?~
    ConvertM.OuterScopeInfo
    { _osiPos = x ^. Input.stored
    , _osiScope = x ^. Input.inferScope
    }
    & local

makeFunction ::
    Monad m =>
    MkProperty' (T m) (Maybe BinderParamScopeId) ->
    ConventionalParams m -> Ann (Input.Payload m) # V.Term ->
    ConvertM m (Function EvalPrep InternalName (OnceT (T m)) (T m) # Annotated (ConvertPayload m ()))
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
                    Just (RecordParams r) ->
                        p
                        <&> (,) ?? Set.fromList (r ^.. SugarLens.taggedListItems . tiTag . tagRefTag . tagVal)
                        & Map.fromList
                    _ -> Map.empty
                )
            & ConvertM.siNullParams <>~
            case params ^. cpParams of
            Just NullParam{} -> Set.fromList p
            _ -> Set.empty
            where
                p = params ^.. cpMLamParam .Lens._Just . _2

convertLam ::
    Monad m =>
    V.TypedLam V.Var (HCompose Prune T.Type) V.Term # Ann (Input.Payload m) ->
    Input.Payload m # V.Term ->
    ConvertM m (ExpressionU EvalPrep m ())
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
    , Annotated (ConvertPayload m ()) # Assignment EvalPrep InternalName (OnceT (T m)) (T m)
    )
convertAssignment binderKind defVar expr =
    convertBinder expr >>= toAssignment binderKind expr <&>
    \r ->
    ( presMode <$ r ^? hVal . _BodyFunction . fParams . _RecordParams . tlItems . Lens._Just . tlTail . traverse
    , r
    )
    where
        presMode = Anchors.assocPresentationMode defVar

toAssignment ::
    Monad m =>
    BinderKind m -> Ann (Input.Payload m) # V.Term ->
    Annotated (ConvertPayload m a) # Binder v name i (T m) ->
    ConvertM m (Annotated (ConvertPayload m a) # Assignment v name i (T m))
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
    , Annotated (ConvertPayload m ())
        # Assignment EvalPrep InternalName (OnceT (T m)) (T m)
    )
convertDefinitionBinder defI t =
    (addLightLambdas <&> (_2 %~)) <*> convertAssignment (BinderKindDef defI) (ExprIRef.globalId defI) t
