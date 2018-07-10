module Lamdu.Sugar.Convert.Expression.Actions
    ( subexprPayloads, addActionsWith, addActions, makeAnnotation, makeActions, convertPayload
    ) where

import qualified Control.Lens.Extended as Lens
import qualified Data.Property as Property
import qualified Data.Set as Set
import qualified Lamdu.Cache as Cache
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val)
import           Lamdu.Calc.Val.Utils (culledSubexprPayloads)
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Sugar.Annotations as Ann
import qualified Lamdu.Sugar.Convert.Eval as ConvertEval
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.Convert.PostProcess as PostProcess
import           Lamdu.Sugar.Convert.Tag (convertTagSelection, AllowAnonTag(..))
import           Lamdu.Sugar.Convert.Type (convertType)
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Lens (overBodyChildren, bodyChildPayloads)
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

mkExtract ::
    Monad m => Input.Payload m a -> ConvertM m (T m ExtractDestination)
mkExtract exprPl =
    Lens.view (ConvertM.scScopeInfo . ConvertM.siMOuter)
    >>= \case
    Nothing -> mkExtractToDef exprPl <&> Lens.mapped %~ ExtractToDef
    Just outerScope ->
        mkExtractToLet (outerScope ^. ConvertM.osiPos) (exprPl ^. Input.stored)
        <&> ExtractToLet & pure

mkExtractToDef :: Monad m => Input.Payload m a -> ConvertM m (T m EntityId)
mkExtractToDef exprPl =
    (,,)
    <$> Lens.view id
    <*> ConvertM.postProcessAssert
    <*> ConvertM.cachedFunc Cache.infer
    <&>
    \(ctx, postProcess, infer) ->
    do
        let scheme =
                Infer.makeScheme (ctx ^. ConvertM.scInferContext)
                (exprPl ^. Input.inferredType)
        let deps = ctx ^. ConvertM.scFrozenDeps . Property.pVal
        newDefI <-
            Definition.Definition
            (Definition.BodyExpr (Definition.Expr valI deps)) scheme ()
            & DataOps.newPublicDefinitionWithPane (ctx ^. ConvertM.scCodeAnchors)
        PostProcess.GoodExpr <-
            PostProcess.def infer (ctx ^. ConvertM.scDebugMonitors) newDefI
        let param = ExprIRef.globalId newDefI
        getVarI <- V.LVar param & V.BLeaf & ExprIRef.newValBody
        (exprPl ^. Input.stored . Property.pSet) getVarI
        Infer.depsGlobalTypes . Lens.at param ?~ scheme
            & Property.pureModify (ctx ^. ConvertM.scFrozenDeps)
        postProcess
        EntityId.ofIRef newDefI & pure
    where
        valI = exprPl ^. Input.stored . Property.pVal

mkExtractToLet ::
    Monad m => ExprIRef.ValP m -> ExprIRef.ValP m -> T m EntityId
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
                -- binder body with "(\x -> assignmentBody) stored", and
                -- stored becomes "x")
                do
                    newParam <- ExprIRef.newVar
                    lamI <-
                        V.Lam newParam extractPosI & V.BLam
                        & ExprIRef.newValBody
                    getVarI <- V.LVar newParam & V.BLeaf & ExprIRef.newValBody
                    (stored ^. Property.pSet) getVarI
                    pure (newParam, lamI)
        V.Apply lamI oldStored & V.BApp & ExprIRef.newValBody
            >>= outerScope ^. Property.pSet
        EntityId.ofBinder newParam & pure
    where
        extractPosI = Property.value outerScope
        oldStored = Property.value stored

mkWrapInRecord ::
    Monad m =>
    Input.Payload m a -> ConvertM m (TagSelection InternalName (T m) (T m) ())
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

makeActions ::
    Monad m =>
    Input.Payload m a -> ConvertM m (NodeActions InternalName (T m) (T m))
makeActions exprPl =
    do
        ext <- mkExtract exprPl
        wrapInRec <- mkWrapInRecord exprPl
        postProcess <- ConvertM.postProcessAssert
        outerPos <-
            Lens.view (ConvertM.scScopeInfo . ConvertM.siMOuter)
            <&> (^? Lens._Just . ConvertM.osiPos)
        pure NodeActions
            { _detach = DataOps.applyHoleTo stored <* postProcess <&> EntityId.ofValI & DetachAction
            , _mSetToHole = DataOps.setToHole stored <* postProcess <&> EntityId.ofValI & Just
            , _extract = ext
            , _mReplaceParent = Nothing
            , _wrapInRecord = wrapInRec
            , _mNewLet = outerPos <&> DataOps.redexWrap <&> fmap EntityId.ofBinder
            }
    where
        stored = exprPl ^. Input.stored

setChildReplaceParentActions ::
    Monad m =>
    ConvertM m (
        ExprIRef.ValP m ->
        Body name (T m) (T m) (ConvertPayload m a) ->
        Body name (T m) (T m) (ConvertPayload m a)
    )
setChildReplaceParentActions =
    ConvertM.typeProtectedSetToVal
    <&>
    \protectedSetToVal stored bod ->
    let setToExpr srcPl =
            pActions . mReplaceParent ?~
            (protectedSetToVal
                stored
                (srcPl ^. pInput . Input.stored . Property.pVal)
                <&> EntityId.ofValI)
    in
    case bod of
    BodyLam lam | Lens.has (lamFunc . fBody . bContent . _BinderLet) lam -> bod
    _ ->
        bod
        & Lens.filtered (not . Lens.has (_BodyFragment . fHeal . _TypeMismatch))
        . bodyChildPayloads %~ join setToExpr
        -- Replace-parent with fragment sets directly to fragment expression
        & overBodyChildren id id id
            (Lens.filteredBy (_PNode . val . _BodyFragment . fExpr . _PNode . ann) <. _PNode . ann %@~ setToExpr)
        -- Replace-parent of fragment expr without "heal" available -
        -- replaces parent of fragment rather than fragment itself (i.e: replaces grandparent).
        & overBodyChildren id id id
            (_PNode . val . _BodyFragment . Lens.filtered (Lens.has (fHeal . _TypeMismatch)) .
                fExpr . _PNode . ann %~ join setToExpr)

subexprPayloads ::
    Foldable f =>
    f (Val (Input.Payload m a)) -> [ConvertPayload m a] -> [a]
subexprPayloads subexprs cullPoints =
    subexprs ^.. Lens.folded . Lens.to (culledSubexprPayloads toCull) . Lens.folded . Input.userData
    where
        -- | The direct child exprs of the sugar expr
        cullSet =
            cullPoints ^.. Lens.folded . pInput . Input.stored . Property.pVal
            <&> EntityId.ofValI
            & Set.fromList
        toCull pl = (pl ^. Input.entityId) `Set.member` cullSet

addActionsWith ::
    Monad m =>
    a -> Input.Payload m b ->
    Body InternalName (T m) (T m) (ConvertPayload m a) ->
    ConvertM m (ExpressionU m a)
addActionsWith userData exprPl bodyS =
    do
        actions <- makeActions exprPl
        addReplaceParents <- setChildReplaceParentActions
        PNode Node
            { _val = addReplaceParents (exprPl ^. Input.stored) bodyS
            , _ann =
                ConvertPayload
                { _pInput = exprPl & Input.userData .~ userData
                , _pActions = actions
                }
            } & pure

addActions ::
    (Monad m, Monoid a, Foldable f) =>
    f (Val (Input.Payload m a)) -> Input.Payload m a ->
    Body InternalName (T m) (T m) (ConvertPayload m a) ->
    ConvertM m (ExpressionU m a)
addActions subexprs exprPl bodyS =
    addActionsWith (mconcat (subexprPayloads subexprs childPayloads)) exprPl bodyS
    where
        childPayloads = bodyS ^.. bodyChildPayloads

makeTypeAnnotation :: Monad m => Input.Payload m a -> ConvertM m (Type InternalName)
makeTypeAnnotation payload =
    convertType (EntityId.ofTypeOf entityId) typ
    where
        entityId = payload ^. Input.entityId
        typ = payload ^. Input.inferredType

makeAnnotation ::
    Monad m =>
    Input.AnnotationMode -> Ann.ShowAnnotation -> Input.Payload m a ->
    ConvertM m (Annotation InternalName (T m))
makeAnnotation Input.None showAnn pl
    | showAnn ^. Ann.showExpanded = makeTypeAnnotation pl <&> AnnotationType
    | otherwise = pure AnnotationNone
makeAnnotation Input.Types showAnn pl
    | showAnn ^. Ann.showInTypeMode = makeTypeAnnotation pl <&> AnnotationType
    | otherwise = pure AnnotationNone
makeAnnotation Input.Evaluation showAnn pl
    | showAnn ^. Ann.showInEvalMode =
        guard (showAnn ^. Ann.showExpanded)
        & Lens._Just (const (makeTypeAnnotation pl))
        <&>
        ValAnnotation
        ( pl ^. Input.evalResults <&> (^. Input.eResults)
            & ConvertEval.results (EntityId.ofEvalOf (pl ^. Input.entityId))
        )
        <&> AnnotationVal
    | otherwise = pure AnnotationNone

convertPayload ::
    Monad m =>
    Input.AnnotationMode -> (Ann.ShowAnnotation, ConvertPayload m a) ->
    ConvertM m (Payload InternalName (T m) (T m) a)
convertPayload mode (showAnn, pl) =
    makeAnnotation mode showAnn (pl ^. pInput)
    <&>
    \x ->
    Payload
    { _plAnnotation = x
    , _plActions = pl ^. pActions
    , _plNeverShrinkAnnotation = showAnn ^. Ann.showExpanded
    , _plEntityId = pl ^. pInput . Input.entityId
    , _plData = pl ^. pInput . Input.userData
    }
