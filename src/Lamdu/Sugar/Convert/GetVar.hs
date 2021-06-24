{-# LANGUAGE PolyKinds #-}

module Lamdu.Sugar.Convert.GetVar
    ( convert, globalNameRef
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Extended ((~~>))
import           Control.Monad.Trans.Except.Extended (runMatcherT, justToLeft)
import           Control.Monad.Trans.Maybe (MaybeT)
import           Control.Monad.Transaction (MonadTransaction, getP, setP)
import qualified Control.Monad.Transaction as Transaction
import           Data.Maybe.Extended (maybeToMPlus)
import qualified Data.Property as Property
import qualified Data.Set as Set
import           Hyper
import           Hyper.Syntax.Row (freExtends)
import qualified Hyper.Syntax.Scheme as S
import qualified Lamdu.Calc.Lens as ExprLens
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Def
import qualified Lamdu.Data.Ops as DataOps
import           Lamdu.Expr.IRef (DefI, HRef)
import qualified Lamdu.Expr.IRef as ExprIRef
import           Lamdu.Sugar.Convert.Binder.Params (mkVarInfo)
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM, siTagParamInfos, tpiFromParameters)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

jumpToDefI ::
    Monad m => Anchors.CodeAnchors m -> DefI m -> T m EntityId
jumpToDefI cp defI =
    EntityId.ofIRef defI <$ DataOps.newPane cp (Anchors.PaneDefinition defI)

inlineDef :: Monad m => V.Var -> HRef m # V.Term -> ConvertM m (T m EntityId)
inlineDef globalId dest =
    (,)
    <$> Lens.view id
    <*> ConvertM.postProcessAssert
    <&>
    \(ctx, postProcess) ->
    do
        let gotoDef = jumpToDefI (ctx ^. Anchors.codeAnchors) defI
        let doInline def defExpr =
                do
                    (dest ^. ExprIRef.setIref) (defExpr ^. Def.expr)
                    Property.pureModify (ctx ^. ConvertM.scFrozenDeps) (<> defExpr ^. Def.exprFrozenDeps)
                    newDefExpr <- DataOps.newHole
                    def & Def.defBody .~ Def.BodyExpr (Def.Expr newDefExpr mempty)
                        & Def.defType .~
                            _Pure # S.Scheme
                            { S._sForAlls = T.Types (S.QVars ("a" ~~> mempty)) (S.QVars mempty)
                            , S._sTyp = _Pure # T.TVar "a"
                            }
                        & Transaction.writeIRef defI
                    setP (Anchors.assocDefinitionState defI) DeletedDefinition
                    postProcess
                    defExpr ^. Def.expr & EntityId.ofValI & pure
        def <- Transaction.readIRef defI
        case def ^. Def.defBody of
            Def.BodyBuiltin _ ->
                -- Cannot inline builtins.
                -- Jump to it instead so that user understands that it's a builtin.
                gotoDef
            Def.BodyExpr defExpr ->
                do
                    isRecursive <-
                        ExprIRef.readRecursively (defExpr ^. Def.expr)
                        <&> Lens.has (ExprLens.valGlobals mempty . Lens.only globalId)
                    if isRecursive
                        then gotoDef
                        else doInline def defExpr
    where
        defI = ExprIRef.defI globalId

globalNameRef ::
    (MonadTransaction n m, Monad f) =>
    Anchors.CodeAnchors f -> DefI f -> m (NameRef InternalName (T f))
globalNameRef cp defI =
    taggedName Nothing defI <&>
    \name ->
    NameRef
    { _nrName = name
    , _nrGotoDefinition = jumpToDefI cp defI
    }

inlineableDefinition :: ConvertM.Context m -> V.Var -> EntityId -> Bool
inlineableDefinition ctx var entityId =
    Lens.nullOf
    (ConvertM.scTopLevelExpr . ExprLens.valGlobals recursiveVars . Lens.ifiltered f)
    ctx
    where
        recursiveVars =
            ctx ^.
            ConvertM.scScopeInfo . ConvertM.siRecursiveRef . Lens._Just . ConvertM.rrDefI .
            Lens.to (Set.singleton . ExprIRef.globalId)
        f pl v = v == var && entityId /= pl ^. Input.entityId

convertGlobal ::
    Monad m =>
    V.Var -> Input.Payload m a # V.Term -> MaybeT (ConvertM m) (GetVar InternalName (T m))
convertGlobal var exprPl =
    do
        ctx <- Lens.view id
        notElem var (exprPl ^. Input.localsInScope <&> fst) & guard
        lifeState <- Anchors.assocDefinitionState defI & getP
        let defForm =
                case lifeState of
                DeletedDefinition -> DefDeleted
                LiveDefinition ->
                    ctx ^. ConvertM.scOutdatedDefinitions . Lens.at var
                    <&> Lens.mapped .~ exprPl ^. Input.entityId
                    & maybe DefUpToDate DefTypeChanged
        nameRef <- globalNameRef (ctx ^. Anchors.codeAnchors) defI & lift
        inline <-
            case defForm of
            DefUpToDate
                | inlineableDefinition ctx var (exprPl ^. Input.entityId) ->
                    inlineDef var (exprPl ^. Input.stored) & lift <&> InlineVar
            _ -> pure CannotInline
        GetBinder BinderVarRef
            { _bvNameRef = nameRef
            , _bvVar = var
            , _bvForm = GetDefinition defForm
            , _bvInline = inline
            } & pure
    where
        defI = ExprIRef.defI var

convertGetLet ::
    Monad m =>
    V.Var -> Input.Payload m a # V.Term -> MaybeT (ConvertM m) (GetVar InternalName (T m))
convertGetLet param exprPl =
    do
        inline <-
            Lens.view (ConvertM.scScopeInfo . ConvertM.siLetItems . Lens.at param)
            >>= maybeToMPlus
        varInfo <- mkVarInfo (exprPl ^. Input.inferredType)
        nameRef <- convertLocalNameRef varInfo param
        GetBinder BinderVarRef
            { _bvNameRef = nameRef
            , _bvVar = param
            , _bvForm = GetLet
            , _bvInline = inline (exprPl ^. Input.entityId)
            } & pure

convertParamsRecord ::
    Monad m =>
    V.Var -> Input.Payload m a # V.Term -> MaybeT (ConvertM m) (GetVar InternalName (T m))
convertParamsRecord param exprPl =
    GetParamsRecord ParamsRecordVarRef
    { _prvFieldNames =
        exprPl
        ^.. Input.inferredType . _Pure . T._TRecord . T.flatRow
        . freExtends . Lens.itraversed . Lens.asIndex
        <&> nameWithContext Nothing param
    } <$ check
    where
        check =
            Lens.view (ConvertM.scScopeInfo . siTagParamInfos)
            <&> (^.. Lens.traversed . ConvertM._TagFieldParam)
            <&> map tpiFromParameters
            <&> elem param
            >>= guard

convertLocalNameRef ::
    (Applicative f, MonadTransaction n m) =>
    VarInfo -> V.Var -> m (NameRef InternalName f)
convertLocalNameRef varInfo param =
    Anchors.assocTag param & getP
    <&> \tag ->
    NameRef
    { _nrName = nameWithContext (Just varInfo) param tag
    , _nrGotoDefinition = EntityId.ofTaggedEntity param tag & pure
    }

convertParam ::
    (MonadTransaction u m, Applicative n) =>
    V.Var -> Input.Payload u a # V.Term -> m (GetVar InternalName n)
convertParam param exprPl =
    mkVarInfo (exprPl ^. Input.inferredType)
    >>= (`convertLocalNameRef` param)
    <&>
    \nameRef ->
    GetParam ParamRef
    { _pNameRef = nameRef
    , _pBinderMode = NormalBinder
    }

convert ::
    (Monad m, Monoid a) =>
    V.Var -> Input.Payload m a # V.Term -> ConvertM m (ExpressionU v m a)
convert param exprPl =
    do
        convertGlobal param exprPl & justToLeft
        convertGetLet param exprPl & justToLeft
        convertParamsRecord param exprPl & justToLeft
        convertParam param exprPl & lift
    & runMatcherT
    <&> BodyLeaf . LeafGetVar
    >>= addActions (Const ()) exprPl
