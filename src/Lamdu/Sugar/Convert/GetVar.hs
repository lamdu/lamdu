module Lamdu.Sugar.Convert.GetVar
    ( convert, globalNameRef
    ) where

import           Algebra.Lattice (BoundedJoinSemiLattice(..))
import           AST (_Pure)
import           AST.Term.Row (freExtends)
import qualified AST.Term.Scheme as S
import qualified Control.Lens as Lens
import           Control.Monad.Trans.Except.Extended (runMatcherT, justToLeft)
import           Control.Monad.Trans.Maybe (MaybeT)
import           Control.Monad.Transaction (MonadTransaction, getP, setP)
import qualified Control.Monad.Transaction as Transaction
import qualified Data.Map as Map
import           Data.Maybe.Extended (maybeToMPlus)
import qualified Data.Property as Property
import qualified Lamdu.Calc.Lens as ExprLens
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Def
import qualified Lamdu.Data.Ops as DataOps
import           Lamdu.Expr.IRef (DefI, ValP)
import qualified Lamdu.Expr.IRef as ExprIRef
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
import qualified Lamdu.Sugar.Convert.Fragment as ConvertFragment
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
jumpToDefI cp defI = EntityId.ofIRef defI <$ DataOps.newDefPane cp defI

inlineDef :: Monad m => V.Var -> ValP m -> ConvertM m (T m EntityId)
inlineDef globalId dest =
    (,)
    <$> Lens.view id
    <*> ConvertM.postProcessAssert
    <&>
    \(ctx, postProcess) ->
    do
        let gotoDef = jumpToDefI (ctx ^. has) defI
        let doInline def defExpr =
                do
                    (dest ^. Property.pSet) (defExpr ^. Def.expr)
                    Property.pureModify (ctx ^. ConvertM.scFrozenDeps) (<> defExpr ^. Def.exprFrozenDeps)
                    newDefExpr <- DataOps.newHole
                    def & Def.defBody .~ Def.BodyExpr (Def.Expr newDefExpr mempty)
                        & Def.defType .~
                            _Pure # S.Scheme
                            { S._sForAlls = T.Types (S.QVars (mempty & Lens.at "a" ?~ bottom)) (S.QVars mempty)
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
                        ExprIRef.readVal (defExpr ^. Def.expr)
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
    taggedName defI <&>
    \name ->
    NameRef
    { _nrName = name
    , _nrGotoDefinition = jumpToDefI cp defI
    }

convertGlobal ::
    Monad m => V.Var -> Input.Payload m a -> MaybeT (ConvertM m) (GetVar InternalName (T m))
convertGlobal var exprPl =
    do
        ctx <- Lens.view id
        notElem var (exprPl ^. Input.localsInScope) & guard
        lifeState <- Anchors.assocDefinitionState defI & getP
        let defForm =
                case lifeState of
                DeletedDefinition -> DefDeleted
                LiveDefinition ->
                    ctx ^. ConvertM.scOutdatedDefinitions . Lens.at var
                    <&> Lens.mapped .~ exprPl ^. Input.entityId
                    & maybe DefUpToDate DefTypeChanged
        nameRef <- globalNameRef (ctx ^. has) defI & lift
        inline <-
            case defForm of
            DefUpToDate
                | (ctx ^. ConvertM.scInlineableDefinition) var (exprPl ^. Input.entityId) ->
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
    Monad m => V.Var -> Input.Payload m a -> MaybeT (ConvertM m) (GetVar InternalName (T m))
convertGetLet param exprPl =
    do
        inline <-
            Lens.view (ConvertM.scScopeInfo . ConvertM.siLetItems . Lens.at param)
            >>= maybeToMPlus
        nameRef <- convertLocalNameRef param
        GetBinder BinderVarRef
            { _bvNameRef = nameRef
            , _bvVar = param
            , _bvForm = GetLet
            , _bvInline = inline (exprPl ^. Input.entityId)
            } & pure

convertParamsRecord ::
    Monad m => V.Var -> Input.Payload m a -> MaybeT (ConvertM m) (GetVar InternalName (T m))
convertParamsRecord param exprPl =
    GetParamsRecord ParamsRecordVarRef
    { _prvFieldNames =
        exprPl
        ^.. Input.inferredType . _Pure . T._TRecord . T.flatRow
        . freExtends . Lens.to Map.toList . traverse . Lens._1
        <&> nameWithContext param
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
    V.Var -> m (NameRef InternalName f)
convertLocalNameRef param =
    Anchors.assocTag param & getP
    <&> \tag ->
    NameRef
    { _nrName = nameWithContext param tag
    , _nrGotoDefinition = EntityId.ofTaggedEntity param tag & pure
    }

convertParam ::
    (MonadTransaction u m, Applicative n) => V.Var -> m (GetVar InternalName n)
convertParam param =
    convertLocalNameRef param
    <&>
    \nameRef ->
    GetParam ParamRef
    { _pNameRef = nameRef
    , _pBinderMode = NormalBinder
    }

convert ::
    (Monad m, Monoid a) =>
    V.Var -> Input.Payload m a -> ConvertM m (ExpressionU m a)
convert param exprPl
    | param == ConvertFragment.fragmentVar =
        addActions [] exprPl BodyPlaceHolder
    | otherwise =
        do
            convertGlobal param exprPl & justToLeft
            convertGetLet param exprPl & justToLeft
            convertParamsRecord param exprPl & justToLeft
            convertParam param & lift
        & runMatcherT
        <&> BodyGetVar
        >>= addActions [] exprPl
