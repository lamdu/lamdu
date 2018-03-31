module Lamdu.Sugar.Convert.Expression.Actions
    ( addActions, makeAnnotation, makeActions
    ) where

import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.Property as Property
import qualified Lamdu.Calc.Val as V
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Convert.PostProcess (PostProcessResult(..), postProcessDef)
import           Lamdu.Sugar.Convert.Tag (convertTagSelection, AllowAnonTag(..))
import           Lamdu.Sugar.Convert.Type (convertType)
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

mkExtract ::
    Monad m => Input.Payload m a -> ConvertM m (T m ExtractDestination)
mkExtract exprPl =
    Lens.view id
    <&> \ctx ->
    case ctx ^. ConvertM.scScopeInfo . ConvertM.siMOuter of
    Nothing -> mkExtractToDef ctx exprPl <&> ExtractToDef
    Just outerScope ->
        mkExtractToLet (outerScope ^. ConvertM.osiPos) (exprPl ^. Input.stored)
        <&> ExtractToLet

mkExtractToDef ::
    Monad m => ConvertM.Context m -> Input.Payload m a -> T m EntityId
mkExtractToDef ctx exprPl =
    do
        newDefI <-
            Definition.Definition
            (Definition.BodyExpr (Definition.Expr valI deps)) scheme ()
            & DataOps.newPublicDefinitionWithPane cp
        GoodExpr <- postProcessDef newDefI
        let param = ExprIRef.globalId newDefI
        getVarI <- V.LVar param & V.BLeaf & ExprIRef.newValBody
        Property.set (exprPl ^. Input.stored) getVarI
        Infer.depsGlobalTypes . Lens.at param ?~ scheme
            & Property.pureModify (ctx ^. ConvertM.scFrozenDeps)
        -- Remove the extracted deps
        ctx ^. ConvertM.scPostProcessRoot & void
        EntityId.ofIRef newDefI & pure
    where
        valI = exprPl ^. Input.stored . Property.pVal
        deps = ctx ^. ConvertM.scFrozenDeps . Property.pVal
        cp = ctx ^. ConvertM.scCodeAnchors
        scheme =
            Infer.makeScheme (ctx ^. ConvertM.scInferContext)
            (exprPl ^. Input.inferredType)

mkExtractToLet ::
    Monad m => ExprIRef.ValIProperty m -> ExprIRef.ValIProperty m -> T m EntityId
mkExtractToLet outerScope stored =
    do
        (newParam, lamI) <-
            if Property.value stored == extractPosI
            then
                -- Give entire binder body a name (replace binder body
                -- with "(\x -> x) stored")
                DataOps.newIdentityLambda
            else
                -- Give some subexpr in binder body a name (replace
                -- binder body with "(\x -> binderBody) stored", and
                -- stored becomes "x")
                do
                    newParam <- ExprIRef.newVar
                    lamI <-
                        V.Lam newParam extractPosI & V.BLam
                        & ExprIRef.newValBody
                    getVarI <- V.LVar newParam & V.BLeaf & ExprIRef.newValBody
                    Property.set stored getVarI
                    pure (newParam, lamI)
        V.Apply lamI oldStored & V.BApp & ExprIRef.newValBody
            >>= Property.set outerScope
        EntityId.ofBinder newParam & pure
    where
        extractPosI = Property.value outerScope
        oldStored = Property.value stored

mkWrapInRecord ::
    Monad m => Input.Payload m a -> ConvertM m (TagSelection InternalName (T m) ())
mkWrapInRecord exprPl =
    do
        typeProtectedSetToVal <- ConvertM.typeProtectedSetToVal
        let recWrap tag =
                V.BLeaf V.LRecEmpty & ExprIRef.newValBody
                >>= ExprIRef.newValBody . V.BRecExtend . V.RecExtend tag (stored ^. Property.pVal)
                >>= typeProtectedSetToVal stored
                & void
        convertTagSelection nameWithoutContext mempty RequireTag tempMkEntityId recWrap
    where
        stored = exprPl ^. Input.stored
        -- TODO: The entity-ids created here don't match the resulting entity ids of the record.
        tempMkEntityId = EntityId.ofTaggedEntity (stored ^. Property.pVal)

makeActions :: Monad m => Input.Payload m a -> ConvertM m (NodeActions InternalName (T m))
makeActions exprPl =
    do
        ext <- mkExtract exprPl
        wrapInRec <- mkWrapInRecord exprPl
        postProcess <- ConvertM.postProcess
        pure NodeActions
            { _detach = DataOps.applyHoleTo stored <* postProcess <&> EntityId.ofValI & DetachAction
            , _mSetToHole = DataOps.setToHole stored <* postProcess <&> EntityId.ofValI & Just
            , _extract = ext
            , _mReplaceParent = Nothing
            , _wrapInRecord = wrapInRec
            }
    where
        stored = exprPl ^. Input.stored

setChildReplaceParentActions ::
    Monad m =>
    ConvertM m (
        ExprIRef.ValIProperty m ->
        Body name (T m) (Expression name (T m) (ConvertPayload m a)) ->
        Body name (T m) (Expression name (T m) (ConvertPayload m a))
    )
setChildReplaceParentActions =
    ConvertM.typeProtectedSetToVal
    <&>
    \protectedSetToVal stored body ->
    let setToExpr srcPl =
            plActions . mReplaceParent ?~
            (protectedSetToVal
                stored
                (srcPl ^. plData . pStored . Property.pVal)
                <&> EntityId.ofValI)
        fixFragmentReplaceParent child =
            -- Replace-parent with fragment sets directly to fragment expression
            case child ^. rBody of
            BodyFragment fragment -> child & rPayload %~ setToExpr (fragment ^. fExpr . rPayload)
            _ -> child
    in
    case body of
    BodyLam lam | Lens.has (lamBinder . bBody . bbContent . _BinderLet) lam -> body
    _ ->
        body
        & Lens.filtered (not . Lens.has (_BodyFragment . fAttach . _AttachTypeMismatch)) .
            traverse . rPayload %~ join setToExpr
        <&> fixFragmentReplaceParent
        -- Replace-parent of fragment expr without "attach" available -
        -- replaces parent of fragment rather than fragment itself (i.e: replaces grandparent).
        <&> rBody . _BodyFragment . Lens.filtered (Lens.has (fAttach . _AttachTypeMismatch)) .
            fExpr . rPayload %~ join setToExpr

addActions ::
    Monad m =>
    Input.Payload m a -> Body InternalName (T m) (ExpressionU m a) ->
    ConvertM m (ExpressionU m a)
addActions exprPl body =
    do
        actions <- makeActions exprPl
        ann <- makeAnnotation exprPl
        addReplaceParents <- setChildReplaceParentActions
        pure Expression
            { _rBody = addReplaceParents (exprPl ^. Input.stored) body
            , _rPayload =
                Payload
                { _plEntityId = exprPl ^. Input.entityId
                , _plAnnotation = ann
                , _plActions = actions
                , _plData =
                    ConvertPayload
                    { _pStored = exprPl ^. Input.stored
                    , _pUserData = exprPl ^. Input.userData
                    }
                }
            }

makeAnnotation :: Monad m => Input.Payload m a -> ConvertM m (Annotation InternalName)
makeAnnotation payload =
    do
        typS <- convertType (EntityId.ofTypeOf entityId) typ
        pure Annotation
            { _aInferredType = typS
            , _aMEvaluationResult =
                payload ^. Input.evalResults
                <&> (^. Input.eResults)
                <&> mk
            }
    where
        mk res
            | Map.null res = Nothing
            | otherwise = Just res
        entityId = payload ^. Input.entityId
        typ = payload ^. Input.inferredType
