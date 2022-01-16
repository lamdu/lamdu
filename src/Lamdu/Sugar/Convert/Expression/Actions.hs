{-# LANGUAGE TypeFamilies, TypeApplications, ScopedTypeVariables #-}

module Lamdu.Sugar.Convert.Expression.Actions
    ( addActions, makeActions, makeApply
    ) where

import           Control.Applicative ((<|>))
import qualified Control.Lens.Extended as Lens
import           Control.Monad.Once (OnceT)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Data.Map as Map
import qualified Data.Property as Property
import           Data.Text.Encoding (encodeUtf8)
import           Hyper
import           Hyper.Syntax.Nominal (ToNom(..), NominalDecl(..), NominalInst(..))
import qualified Hyper.Syntax.Scheme as S
import           Hyper.Type.Prune (Prune(..))
import           Hyper.Unify (UVar)
import           Hyper.Unify.Generalize (generalize)
import qualified Lamdu.Builtins.Anchors as Builtins
import qualified Lamdu.Builtins.PrimVal as PrimVal
import qualified Lamdu.Cache as Cache
import           Lamdu.Calc.Definition (depsGlobalTypes, depsNominals)
import           Lamdu.Calc.Infer (runPureInfer)
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM(..))
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.Convert.PostProcess as PostProcess
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

mkExtract ::
    Monad m =>
    Input.Payload m # V.Term -> ConvertM m (T m ExtractDestination)
mkExtract exprPl =
    Lens.view (ConvertM.scScopeInfo . ConvertM.siExtractPos) >>=
    \case
    Nothing -> mkExtractToDef exprPl <&> Lens.mapped %~ ExtractToDef
    Just outerScope ->
        mkExtractToLet (outerScope ^. ConvertM.osiPos) (exprPl ^. Input.stored)
        <&> ExtractToLet & pure

mkExtractToDef :: Monad m => Input.Payload m # V.Term -> ConvertM m (T m EntityId)
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

makeSetToLiteral ::
    Monad m =>
    Input.Payload m # V.Term -> ConvertM m (Literal Identity -> T m EntityId)
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

makeActions ::
    Monad m =>
    Input.Payload m # V.Term -> ConvertM m (NodeActions (T m))
makeActions exprPl =
    do
        ext <- mkExtract exprPl
        postProcess <- ConvertM.postProcessAssert
        setToLit <- makeSetToLiteral exprPl
        apply <- makeApply stored
        pure NodeActions
            { _detach = DataOps.applyHoleTo stored <* postProcess <&> EntityId.ofValI & DetachAction
            , _delete = DataOps.setToHole stored <* postProcess <&> EntityId.ofValI & SetToHole
            , _setToLiteral = setToLit
            , _extract = ext
            , _mReplaceParent = Nothing
            , _mApply = Just apply
            }
    where
        stored = exprPl ^. Input.stored

makeApply :: forall m. Monad m => ExprIRef.HRef m # V.Term -> ConvertM m (T m EntityId)
makeApply stored =
    Lens.view ConvertM.scPostProcessRoot
    <&> \checkOk ->
    do
        tryApp checkOk noop noop
            <|> tryApp checkOk wrap noop -- prefer wrapping outside
            <|> tryApp checkOk noop wrap -- then wrapping inside
            <|> tryApp checkOk wrap wrap -- then both
            & runMaybeT
            <&> fromMaybe (error "Failed to type-check apply with fragments in&out of it")
        <&> EntityId.ofValI
    where
        tryApp checkOk outside inside =
            do
                holeArg <- V.BLeaf V.LHole & ExprIRef.newValI
                thing <- inside (stored ^. ExprIRef.iref)
                V.App thing holeArg & V.BApp & ExprIRef.newValI
                    >>= outside
                    >>= stored ^. ExprIRef.setIref
                pure holeArg
                & ConvertM.typeProtect checkOk
                & MaybeT
        noop :: ExprIRef.ValI m -> T m (ExprIRef.ValI m)
        noop = pure
        wrap :: ExprIRef.ValI m -> T m (ExprIRef.ValI m)
        wrap iref = V.App <$> DataOps.newHole ?? iref <&> V.BApp >>= ExprIRef.newValI

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
        (hVal . bBody . _BinderTerm . typeMismatchPayloads %~ join setToExpr) .
        ((bodyIndex . Lens.filteredByIndex (bBody . _BinderTerm) . fragmentAnnIndex) <. annotation %@~ setToExpr)

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
        Term v name (OnceT (T m)) (T m) # Annotated (ConvertPayload m) ->
        Term v name (OnceT (T m)) (T m) # Annotated (ConvertPayload m)
    )
setChildReplaceParentActions =
    ConvertM.typeProtectedSetToVal
    <&>
    \protectedSetToVal stored bod ->
    let setToExpr srcPl =
            pActions . mReplaceParent ?~
            (protectedSetToVal
                stored
                (srcPl ^. pStored . ExprIRef.iref)
                <&> EntityId.ofValI)
        setForChildren = hmap (\_ -> annotation %~ join setToExpr)
    in
    case bod of
    BodyFragment f | Lens.has (fTypeMismatch . Lens._Just) f ->
        -- Replace-parent for child of expr in fragment attempts heal
        f & fExpr . hVal %~ setForChildren & BodyFragment
    _ -> setForChildren bod
    & hmap (Proxy @FixReplaceParent #> fixReplaceParent setToExpr)

addActions ::
    Monad m =>
    Ann (Input.Payload m) # V.Term ->
    Term v InternalName (OnceT (T m)) (T m) # Annotated (ConvertPayload m) ->
    ConvertM m (ExpressionU v m)
addActions expr bodyS =
    do
        actions <- makeActions (expr ^. hAnn)
        addReplaceParents <- setChildReplaceParentActions
        Ann
            { _hVal = addReplaceParents (expr ^. hAnn . Input.stored) bodyS
            , _hAnn =
                Const ConvertPayload
                { _pUnsugared = expr
                , _pActions = actions
                , _pLambdas = []
                , _pEntityId = expr ^. hAnn . Input.entityId
                }
            } & pure


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
    LiteralChar (Identity x) -> (literalExpr (PrimVal.Char x), pure ())
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
