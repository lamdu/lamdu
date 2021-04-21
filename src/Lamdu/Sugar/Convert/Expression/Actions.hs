{-# LANGUAGE TypeFamilies, TypeApplications, ScopedTypeVariables #-}

module Lamdu.Sugar.Convert.Expression.Actions
    ( subexprPayloads, addActionsWith, addActions, makeActions
    , makeTypeAnnotation, convertPayloads
    ) where

import qualified Control.Lens.Extended as Lens
import           Control.Monad.Once (OnceT)
import           Control.Monad.Transaction (MonadTransaction)
import qualified Data.Map as Map
import qualified Data.Property as Property
import qualified Data.Set as Set
import           Data.Text.Encoding (encodeUtf8)
import           Hyper
import           Hyper.Type.AST.Nominal (ToNom(..), NominalDecl(..), NominalInst(..))
import           Hyper.Type.AST.Row (RowExtend(..))
import qualified Hyper.Type.AST.Scheme as S
import           Hyper.Type.Prune (Prune(..))
import           Hyper.Unify.Binding (UVar)
import           Hyper.Unify.Generalize (generalize)
import qualified Lamdu.Builtins.Anchors as Builtins
import qualified Lamdu.Builtins.PrimVal as PrimVal
import qualified Lamdu.Cache as Cache
import           Lamdu.Calc.Definition (depsGlobalTypes, depsNominals)
import           Lamdu.Calc.Infer (runPureInfer)
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import           Lamdu.Calc.Term.Utils (culledSubexprPayloads)
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM(..))
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.Convert.PostProcess as PostProcess
import qualified Lamdu.Sugar.Convert.Tag as ConvertTag
import           Lamdu.Sugar.Convert.Type (convertType)
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Lens (childPayloads)
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

mkExtract ::
    Monad m =>
    Input.Payload m a # V.Term -> ConvertM m (T m ExtractDestination)
mkExtract exprPl =
    Lens.view (ConvertM.scScopeInfo . ConvertM.siMOuter)
    >>= \case
    Nothing -> mkExtractToDef exprPl <&> Lens.mapped %~ ExtractToDef
    Just outerScope ->
        mkExtractToLet (outerScope ^. ConvertM.osiPos) (exprPl ^. Input.stored)
        <&> ExtractToLet & pure

mkExtractToDef :: Monad m => Input.Payload m a # V.Term -> ConvertM m (T m EntityId)
mkExtractToDef exprPl =
    (,,)
    <$> Lens.view id
    <*> ConvertM.postProcessAssert
    <*> ConvertM.cachedFunc Cache.infer
    <&>
    \(ctx, postProcess, infer) ->
    do
        let scheme =
                generalize (exprPl ^. Input.inferredTypeUVar)
                >>= S.saveScheme
                & runPureInfer @(V.Scope # UVar) V.emptyScope (ctx ^. ConvertM.scInferContext)
                & Lens._Left %~ (\x -> x :: Pure # T.TypeError)
                & (^?! Lens._Right . _1)
        let deps = ctx ^. ConvertM.scFrozenDeps . Property.pVal
        newDefI <-
            Definition.Definition
            (Definition.BodyExpr (Definition.Expr valI deps)) scheme ()
            & DataOps.newPublicDefinitionWithPane (ctx ^. Anchors.codeAnchors)
        PostProcess.def infer (ctx ^. ConvertM.scDebugMonitors) newDefI
            >>=
            \case
            PostProcess.GoodExpr -> pure ()
            _ -> error "Bug!"
        let param = ExprIRef.globalId newDefI
        getVarI <- V.LVar param & V.BLeaf & ExprIRef.newValI
        (exprPl ^. Input.stored . ExprIRef.setIref) getVarI
        depsGlobalTypes . Lens.at param ?~ scheme
            & Property.pureModify (ctx ^. ConvertM.scFrozenDeps)
        postProcess
        EntityId.ofIRef newDefI & pure
    where
        valI = exprPl ^. Input.stored . ExprIRef.iref

mkExtractToLet ::
    Monad m =>
    ExprIRef.HRef m # V.Term -> ExprIRef.HRef m # V.Term -> T m EntityId
mkExtractToLet outerScope stored =
    do
        lamI <-
            if stored ^. ExprIRef.iref == extractPosI
            then
                -- Give entire binder body a name (replace binder body
                -- with "(\x -> x) stored")
                DataOps.newIdentityLambda <&> snd
            else
                -- Give some subexpr in binder body a name (replace
                -- binder body with "(\x -> assignmentBody) stored", and
                -- stored becomes "x")
                do
                    newParam <- ExprIRef.newVar
                    paramType <- _HCompose # Pruned & ExprIRef.newValI
                    lamI <-
                        V.TypedLam newParam paramType extractPosI & V.BLam
                        & ExprIRef.newValI
                    getVarI <- V.LVar newParam & V.BLeaf & ExprIRef.newValI
                    (stored ^. ExprIRef.setIref) getVarI
                    pure lamI
        V.App lamI oldStored & V.BApp & ExprIRef.newValI
            >>= outerScope ^. ExprIRef.setIref
        EntityId.ofValI oldStored & pure
    where
        extractPosI = outerScope ^. ExprIRef.iref
        oldStored = stored ^. ExprIRef.iref

mkWrapInRecord ::
    Monad m =>
    Input.Payload m a # V.Term -> ConvertM m (TagChoice InternalName (OnceT (T m)) (T m) ())
mkWrapInRecord exprPl =
    do
        typeProtectedSetToVal <- ConvertM.typeProtectedSetToVal
        let recWrap tag =
                V.BLeaf V.LRecEmpty & ExprIRef.newValI
                >>= ExprIRef.newValI . V.BRecExtend . RowExtend tag (stored ^. ExprIRef.iref)
                >>= typeProtectedSetToVal stored
                & void
        ConvertTag.replace nameWithoutContext mempty ConvertTag.RequireTag
            tempMkEntityId recWrap
            >>= ConvertM . lift
    where
        stored = exprPl ^. Input.stored
        -- TODO: The entity-ids created here don't match the resulting entity ids of the record.
        tempMkEntityId = EntityId.ofTaggedEntity (stored ^. ExprIRef.iref)

makeSetToLiteral ::
    Monad m =>
    Input.Payload m a # V.Term -> ConvertM m (Literal Identity -> T m EntityId)
makeSetToLiteral exprPl =
    (,) <$> ConvertM.typeProtectedSetToVal <*> valFromLiteral
    <&>
    \(setToVal, valFromLit) lit ->
    let (x, update) = valFromLit lit
    in
    do
        update
        l <-
            x & hflipped %~ hmap (const (const (ExprIRef.WriteNew :*: Const ())))
            & hAnn . _1 .~ ExprIRef.ExistingRef (exprPl ^. Input.stored . ExprIRef.iref)
            & ExprIRef.writeRecursively
            <&> (^. hAnn . _1)
        _ <- setToVal (exprPl ^. Input.stored) l
        EntityId.ofValI l & pure

makeSetToEmptyRecord ::
    Monad m =>
    Input.Payload m a # V.Term -> ConvertM m (T m EntityId)
makeSetToEmptyRecord exprPl =
    ConvertM.typeProtectedSetToVal <&>
    \setToVal ->
    EntityId.ofValI i <$
    do
        V.BLeaf V.LRecEmpty & ExprIRef.writeValI i
        setToVal (exprPl ^. Input.stored) i
    where
        i = exprPl ^. Input.stored . ExprIRef.iref

makeActions ::
    Monad m =>
    Input.Payload m a # V.Term -> ConvertM m (NodeActions InternalName (OnceT (T m)) (T m))
makeActions exprPl =
    do
        ext <- mkExtract exprPl
        wrapInRec <- mkWrapInRecord exprPl
        postProcess <- ConvertM.postProcessAssert
        outerPos <-
            Lens.view (ConvertM.scScopeInfo . ConvertM.siMOuter)
            <&> (^? Lens._Just . ConvertM.osiPos)
        setToLit <- makeSetToLiteral exprPl
        setToRec <- makeSetToEmptyRecord exprPl
        apply <- makeApply exprPl
        pure NodeActions
            { _detach = DataOps.applyHoleTo stored <* postProcess <&> EntityId.ofValI & DetachAction
            , _delete = DataOps.setToHole stored <* postProcess <&> EntityId.ofValI & SetToHole
            , _setToLiteral = setToLit
            , _setToEmptyRecord = setToRec
            , _extract = ext
            , _mReplaceParent = Nothing
            , _wrapInRecord = wrapInRec
            , _mNewLet = outerPos <&> DataOps.redexWrap <&> fmap EntityId.ofValI
            , _mApply = apply
            }
    where
        stored = exprPl ^. Input.stored

makeApply :: Monad m => Input.Payload m a # V.Term -> ConvertM m (Maybe (T m EntityId))
makeApply pl =
    (,)
    <$> Lens.view ConvertM.scPostProcessRoot
    <*> ConvertM.postProcessAssert
    <&>
    \(checkOk, postProcess) ->
    do
        arg <- V.BLeaf V.LHole & ExprIRef.newValI
        V.App (pl ^. Input.stored . ExprIRef.iref) arg & V.BApp & ExprIRef.newValI
            >>= pl ^. Input.stored . ExprIRef.setIref
        pure arg
    & ConvertM.typeProtect checkOk
    >>= maybe (DataOps.applyHoleTo (pl ^. Input.stored) <* postProcess) pure
    <&> EntityId.ofValI
    & Just

fragmentAnnIndex ::
    (Applicative f, Lens.Indexable j p) =>
    p a (f a) -> Lens.Indexed (Term v name i o # Annotated j) a (f a)
fragmentAnnIndex = Lens.filteredByIndex (_BodyFragment . fExpr . annotation)

bodyIndex ::
    Lens.IndexedTraversal' (k # Ann a) (Ann a # k) (Ann a # k)
bodyIndex = Lens.filteredBy hVal

class FixReplaceParent expr where
    fixReplaceParent :: (a -> a -> a) -> Annotated a # expr -> Annotated a # expr

instance FixReplaceParent (Const a) where
    fixReplaceParent _ = id

instance FixReplaceParent (Composite v name i o) where
    fixReplaceParent _ = id

instance FixReplaceParent (PostfixFunc v name i o) where
    fixReplaceParent _ = id

-- * Replace-parent with fragment sets directly to fragment expression
-- * Replace-parent of fragment expr without "heal" available -
--   replaces parent of fragment rather than fragment itself (i.e: replaces grandparent).

-- TODO: These instances have a repeating pattern
instance FixReplaceParent (Binder v name i o) where
    fixReplaceParent setToExpr =
        (hVal . _BinderTerm . typeMismatchPayloads %~ join setToExpr) .
        ((bodyIndex . Lens.filteredByIndex _BinderTerm . fragmentAnnIndex) <. annotation %@~ setToExpr)

instance FixReplaceParent (Term v name i o) where
    fixReplaceParent setToExpr =
        (hVal . typeMismatchPayloads %~ join setToExpr) .
        ((bodyIndex . fragmentAnnIndex) <. annotation %@~ setToExpr)

instance FixReplaceParent (Else v name i o) where
    fixReplaceParent setToExpr =
        (hVal . _SimpleElse . typeMismatchPayloads %~ join setToExpr) .
        ((bodyIndex . Lens.filteredByIndex _SimpleElse . fragmentAnnIndex) <. annotation%@~ setToExpr)

-- TODO: This is an indexed lens of some sort?
typeMismatchPayloads ::
    (a -> Identity a) ->
    Term v name i o # Annotated a -> Identity (Term v name i o # Annotated a)
typeMismatchPayloads =
    _BodyFragment . Lens.filteredBy (fTypeMismatch . Lens._Just) . fExpr .
    annotation

setChildReplaceParentActions ::
    Monad m =>
    ConvertM m (
        ExprIRef.HRef m # V.Term ->
        Term v name (OnceT (T m)) (T m) # Annotated (ConvertPayload m a) ->
        Term v name (OnceT (T m)) (T m) # Annotated (ConvertPayload m a)
    )
setChildReplaceParentActions =
    ConvertM.typeProtectedSetToVal
    <&>
    \protectedSetToVal stored bod ->
    let setToExpr srcPl =
            pActions . mReplaceParent ?~
            (protectedSetToVal
                stored
                (srcPl ^. pInput . Input.stored . ExprIRef.iref)
                <&> EntityId.ofValI)
        setForChildren = hmap (\_ -> annotation %~ join setToExpr)
    in
    case bod of
    BodyFragment f | Lens.has (fTypeMismatch . Lens._Just) f ->
        -- Replace-parent for child of expr in fragment attempts heal
        f & fExpr . hVal %~ setForChildren & BodyFragment
    _ -> setForChildren bod
    & hmap (Proxy @FixReplaceParent #> fixReplaceParent setToExpr)

subexprPayloads ::
    forall h m a.
    Recursively HFoldable h =>
    h # Ann (Input.Payload m a) -> [ConvertPayload m a] -> [a]
subexprPayloads subexprs cullPoints =
    withDict (recursively (Proxy @(HFoldable h))) $
    hfoldMap
    ( Proxy @(Recursively HFoldable) #> culledSubexprPayloads toCull
    ) subexprs
    where
        -- | The direct child exprs of the sugar expr
        cullSet =
            cullPoints ^.. Lens.folded . pInput . Input.stored . ExprIRef.iref
            <&> EntityId.ofValI
            & Set.fromList
        toCull :: Input.Payload m a # n -> Maybe a
        toCull pl =
            pl ^. Input.userData <$
            guard (not (cullSet ^. Lens.contains (pl ^. Input.entityId)))

addActionsWith ::
    Monad m =>
    a -> Input.Payload m b # V.Term ->
    Term v InternalName (OnceT (T m)) (T m) # Annotated (ConvertPayload m a) ->
    ConvertM m (ExpressionU v m a)
addActionsWith userData exprPl bodyS =
    do
        actions <- makeActions exprPl
        addReplaceParents <- setChildReplaceParentActions
        Ann
            { _hVal = addReplaceParents (exprPl ^. Input.stored) bodyS
            , _hAnn =
                Const ConvertPayload
                { _pInput = exprPl & Input.userData .~ userData
                , _pActions = actions
                , _pLambdas = []
                }
            } & pure

addActions ::
    (Monad m, Monoid a, Recursively HFoldable h) =>
    h # Ann (Input.Payload m a) -> Input.Payload m a # V.Term ->
    Term v InternalName (OnceT (T m)) (T m) # Annotated (ConvertPayload m a) ->
    ConvertM m (ExpressionU v m a)
addActions subexprs exprPl bodyS =
    addActionsWith (mconcat (subexprPayloads subexprs (bodyS ^.. childPayloads)))
    exprPl bodyS

makeTypeAnnotation ::
    MonadTransaction n m =>
    EntityId -> Pure # T.Type -> m (Annotated EntityId # Type InternalName)
makeTypeAnnotation = convertType . EntityId.ofTypeOf

mkEvalPrep :: ConvertPayload m a -> EvalPrep
mkEvalPrep pl =
    EvalPrep
    { _eType = pl ^. pInput . Input.inferredType
    , _eEvalId = pl ^. pInput . Input.entityId
    , _eLambdas = pl ^. pLambdas
    }

convertPayloads ::
    Recursively HFunctor h =>
    Annotated (ConvertPayload m a) # h ->
    Annotated (Payload EvalPrep InternalName (OnceT (T m)) (T m), a) # h
convertPayloads = hflipped %~ hmap (const (Lens._Wrapped %~ convertPayload))

convertPayload ::
    ConvertPayload m a ->
    (Payload EvalPrep InternalName (OnceT (T m)) (T m), a)
convertPayload pl =
    ( Payload
        { _plAnnotation = mkEvalPrep pl
        , _plActions = pl ^. pActions
        , _plNeverShrinkTypeAnnotations = False
        , _plEntityId = pl ^. pInput . Input.entityId
        }
    , pl ^. pInput . Input.userData
    )

valFromLiteral ::
    Monad m =>
    ConvertM m (Literal Identity -> (Val (Pure # T.Type), T m ()))
valFromLiteral =
    Lens.view ConvertM.scFrozenDeps
    <&>
    \frozenDeps ->
    \case
    LiteralNum (Identity x) -> (literalExpr (PrimVal.Float x), pure ())
    LiteralBytes (Identity x) -> (literalExpr (PrimVal.Bytes x), pure ())
    LiteralText (Identity x) ->
        ( encodeUtf8 x
            & PrimVal.Bytes
            & literalExpr
            & ToNom Builtins.textTid
            & V.BToNom
            & Ann (Lens._Wrapped . _Pure # T.TInst (NominalInst Builtins.textTid noParams))
        , Property.pureModify frozenDeps (<> textDep)
        )
    where
        literalExpr v =
            V.LLiteral prim & V.BLeaf
            & Ann (Lens._Wrapped . _Pure # T.TInst (NominalInst (prim ^. V.primType) noParams))
            where
                prim = PrimVal.fromKnown v
        noParams = T.Types (S.QVarInstances mempty) (S.QVarInstances mempty)
        textDep =
            mempty
            & depsNominals .~
                Map.singleton Builtins.textTid
                ( _Pure # NominalDecl
                { _nParams = T.Types (S.QVars mempty) (S.QVars mempty)
                , _nScheme =
                    S.Scheme
                    { S._sForAlls = T.Types (S.QVars mempty) (S.QVars mempty)
                    , S._sTyp = _Pure . T._TInst # NominalInst Builtins.bytesTid noParams
                    }
                })
