{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}

module Lamdu.Sugar.Convert
    ( loadWorkArea, InternalName
    ) where

import           AST (Tree, Pure, Children, Recursive)
import           AST.Knot.Ann (Ann, ann, annotations, val)
import           Control.Applicative ((<|>))
import qualified Control.Lens as Lens
import           Control.Monad.Transaction (MonadTransaction)
import           Data.CurAndPrev (CurAndPrev)
import           Data.List.Extended (insertAt, removeAt)
import           Data.Property (Property(Property))
import qualified Data.Property as Property
import           Data.Proxy (Proxy(..))
import qualified Data.Set as Set
import qualified GUI.Momentu.Direction as Dir
import qualified Lamdu.Annotations as Annotations
import qualified Lamdu.Cache as Cache
import qualified Lamdu.Calc.Lens as ExprLens
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Debug as Debug
import           Lamdu.Eval.Results (EvalResults)
import qualified Lamdu.Eval.Results as ER
import           Lamdu.Eval.Results.Process (addTypes)
import           Lamdu.Expr.IRef (DefI, ValI, ValP)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Load as ExprLoad
import           Lamdu.I18N.LangId (LangId)
import           Lamdu.Sugar.Annotations (ShowAnnotation, MarkAnnotations, markNodeAnnotations, alwaysShowAnnotations)
import           Lamdu.Sugar.Config (Config, showAllAnnotations)
import           Lamdu.Sugar.Convert.Binder (convertBinder)
import           Lamdu.Sugar.Convert.Binder.Params (mkVarInfo)
import qualified Lamdu.Sugar.Convert.DefExpr as ConvertDefExpr
import qualified Lamdu.Sugar.Convert.DefExpr.OutdatedDefs as OutdatedDefs
import qualified Lamdu.Sugar.Convert.Eval as ConvertEval
import qualified Lamdu.Sugar.Convert.Expression as ConvertExpr
import           Lamdu.Sugar.Convert.Expression.Actions (convertPayload)
import qualified Lamdu.Sugar.Convert.GetVar as ConvertGetVar
import qualified Lamdu.Sugar.Convert.Input as Input
import qualified Lamdu.Sugar.Convert.Load as Load
import           Lamdu.Sugar.Convert.Monad (Context(..), ScopeInfo(..), RecursiveRef(..))
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.Convert.PostProcess as PostProcess
import           Lamdu.Sugar.Convert.Tag (convertTaggedEntityWith)
import qualified Lamdu.Sugar.Convert.Type as ConvertType
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.OrderTags as OrderTags
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction
import           Text.PrettyPrint.HughesPJClass (Pretty(..))

import           Lamdu.Prelude

type T = Transaction

convertDefIBuiltin ::
    (MonadTransaction n m, Monad f) =>
    Tree Pure T.Scheme -> Definition.FFIName -> DefI f ->
    m (DefinitionBody InternalName i (T f) a)
convertDefIBuiltin scheme name defI =
    ConvertType.convertScheme (EntityId.currentTypeOf entityId) scheme
    <&> \typeS ->
    DefinitionBodyBuiltin DefinitionBuiltin
    { _biName = name
    , _biSetName = setName
    , _biType = typeS
    }
    where
        entityId = ExprIRef.globalId defI & EntityId.ofBinder
        setName newName =
            Transaction.writeIRef defI
            Definition.Definition
            { Definition._defBody = Definition.BodyBuiltin newName
            , Definition._defType = scheme
            , Definition._defPayload = ()
            }

emptyScopeInfo :: Maybe (RecursiveRef m) -> ScopeInfo m
emptyScopeInfo recursiveRef =
    ScopeInfo
    { _siTagParamInfos = mempty
    , _siNullParams = mempty
    , _siLetItems = mempty
    , _siMOuter = Nothing
    , _siRecursiveRef = recursiveRef
    }

canInlineDefinition :: Val (Input.Payload m [EntityId]) -> Set V.Var -> V.Var -> EntityId -> Bool
canInlineDefinition defExpr recursiveVars var entityId =
    Lens.nullOf (ExprLens.valGlobals recursiveVars . Lens.ifiltered f) defExpr
    where
        f pl v = v == var && entityId `notElem` pl ^. Input.userData

trimParamAnnotation :: Annotations.Mode -> Annotation name i -> Annotation name i
trimParamAnnotation Annotations.None _ = AnnotationNone
trimParamAnnotation Annotations.Evaluation (AnnotationVal x) =
    x & annotationType .~ Nothing & AnnotationVal
trimParamAnnotation Annotations.Evaluation _ = AnnotationNone
trimParamAnnotation Annotations.Types (AnnotationVal x) =
    maybe AnnotationNone AnnotationType (x ^. annotationType)
trimParamAnnotation Annotations.Types x = x

assertInferSuccess :: HasCallStack => Either (Tree Pure T.TypeError) a -> a
assertInferSuccess =
    either (error . ("Type inference failed: " ++) . show . pPrint) id

convertInferDefExpr ::
    forall m env.
    ( HasCallStack, Monad m, Has LangId env, Has Dir.Layout env
    , Has Debug.Monitors env
    , Has (CurAndPrev (EvalResults (ValI m))) env
    , Has Config env, Has Cache.Functions env, Has Annotations.Mode env
    ) =>
    env -> Anchors.CodeAnchors m ->
    Tree Pure T.Scheme -> Definition.Expr (Val (ValP m)) -> DefI m ->
    T m (DefinitionBody InternalName (T m) (T m) (Payload InternalName (T m) (T m) [EntityId]))
convertInferDefExpr env cp defType defExpr defI =
    do
        Load.InferResult valInferred newInferContext <-
            Load.inferDef cachedInfer (env ^. has) (env ^. has) defExpr defVar
            <&> assertInferSuccess
        outdatedDefinitions <-
            OutdatedDefs.scan entityId defExpr setDefExpr postProcess
            <&> Lens.mapped . defTypeUseCurrent %~ (<* postProcess)
        let context =
                Context
                { _scInferContext = newInferContext
                , _scConfig = env ^. has
                , _scCodeAnchors = cp
                , _scScopeInfo =
                    emptyScopeInfo
                    ( Just RecursiveRef
                      { _rrDefI = defI
                      , _rrDefType = defType
                      }
                    )
                , _scDebugMonitors = env ^. has
                , _scCacheFunctions = env ^. has
                , _scPostProcessRoot = postProcess
                , _scOutdatedDefinitions = outdatedDefinitions
                , _scInlineableDefinition = canInlineDefinition valInferred (Set.singleton defVar)
                , _scFrozenDeps =
                    Property (defExpr ^. Definition.exprFrozenDeps) setFrozenDeps
                , scConvertSubexpression = ConvertExpr.convert
                , _scLanguageIdentifier = env ^. has
                , _scLanguageDir = env ^. has
                }
        ConvertDefExpr.convert
            defType (defExpr & Definition.expr .~ valInferred) defI
            <&> _DefinitionBodyExpression . deContent %~ markAnnotations (env ^. has)
            >>= (_DefinitionBodyExpression . deContent . annotations) (convertPayload (env ^. has))
            & ConvertM.run context
            <&> _DefinitionBodyExpression . deContent . val %~
                SugarLens.onSubExprParams
                (Proxy @(BinderParams InternalName (T m) (T m)))
                (SugarLens.paramsAnnotations %~ trimParamAnnotation (env ^. has))
    where
        cachedInfer = Cache.infer (env ^. has)
        postProcess = PostProcess.def cachedInfer (env ^. has) defI
        entityId = EntityId.ofBinder defVar
        defVar = ExprIRef.globalId defI
        setDefExpr x =
            Definition.Definition (Definition.BodyExpr x) defType ()
            & Transaction.writeIRef defI
        setFrozenDeps deps =
            Transaction.readIRef defI
            <&> Definition.defBody . Definition._BodyExpr . Definition.exprFrozenDeps .~ deps
            >>= Transaction.writeIRef defI

convertDefBody ::
    ( HasCallStack, Monad m, Has LangId env, Has Dir.Layout env
    , Has Debug.Monitors env
    , Has (CurAndPrev (EvalResults (ValI m))) env
    , Has Config env, Has Cache.Functions env, Has Annotations.Mode env
    ) =>
    env -> Anchors.CodeAnchors m ->
    Definition.Definition (Val (ValP m)) (DefI m) ->
    T m
    (DefinitionBody InternalName (T m) (T m) (Payload InternalName (T m) (T m) [EntityId]))
convertDefBody env cp (Definition.Definition bod defType defI) =
    case bod of
    Definition.BodyBuiltin builtin -> convertDefIBuiltin defType builtin defI
    Definition.BodyExpr defExpr ->
        convertInferDefExpr env cp defType defExpr defI

markAnnotations ::
    (MarkAnnotations t, Recursive Children t) =>
    Config ->
    Tree (Ann a) t ->
    Tree (Ann (ShowAnnotation, a)) t
markAnnotations config
    | config ^. showAllAnnotations = annotations %~ (,) alwaysShowAnnotations
    | otherwise = markNodeAnnotations

convertRepl ::
    forall m env.
    ( HasCallStack, Monad m
    , Has LangId env
    , Has Dir.Layout env
    , Has Debug.Monitors env
    , Has (CurAndPrev (EvalResults (ValI m))) env
    , Has Config env, Has Cache.Functions env, Has Annotations.Mode env
    ) =>
    env -> Anchors.CodeAnchors m ->
    T m
    (Repl InternalName (T m) (T m)
        (Payload InternalName (T m) (T m) [EntityId]))
convertRepl env cp =
    do
        defExpr <- ExprLoad.defExpr prop
        entityId <- Property.getP prop <&> (^. Definition.expr) <&> EntityId.ofValI
        Load.InferResult valInferred newInferContext <-
            Load.inferDefExpr cachedInfer (env ^. has) (env ^. has) defExpr
            <&> assertInferSuccess
        outdatedDefinitions <-
            OutdatedDefs.scan entityId defExpr (Property.setP prop) postProcess
        let context =
                Context
                { _scInferContext = newInferContext
                , _scConfig = env ^. has
                , _scCodeAnchors = cp
                , _scScopeInfo = emptyScopeInfo Nothing
                , _scDebugMonitors = env ^. has
                , _scCacheFunctions = env ^. has
                , _scPostProcessRoot = postProcess
                , _scOutdatedDefinitions = outdatedDefinitions
                , _scInlineableDefinition = canInlineDefinition valInferred mempty
                , _scFrozenDeps =
                    Property (defExpr ^. Definition.exprFrozenDeps) setFrozenDeps
                , scConvertSubexpression = ConvertExpr.convert
                , _scLanguageIdentifier = env ^. has
                , _scLanguageDir = env ^. has
                }
        let typ = valInferred ^. ann . Input.inferredType
        nomsMap <-
            valInferred ^.. annotations . Input.inferredType . ExprLens.tIds
            & Load.makeNominalsMap
        let completion =
                env ^. has
                <&> (^. ER.erCompleted)
                <&> Lens._Just . Lens._Right %~ addTypes nomsMap typ
        expr <-
            convertBinder valInferred
            <&> markAnnotations (env ^. has)
            >>= annotations (convertPayload (env ^. has))
            & ConvertM.run context
            <&> val %~
                SugarLens.onSubExprParams
                (Proxy @(BinderParams InternalName (T m) (T m)))
                (SugarLens.paramsAnnotations %~ trimParamAnnotation (env ^. has))
            >>= OrderTags.orderNode
        let replEntityId = expr ^. SugarLens.binderResultExpr . plEntityId
        pure Repl
            { _replExpr = expr
            , _replVarInfo = mkVarInfo typ
            , _replResult = ConvertEval.completion cp replEntityId completion
            }
    where
        cachedInfer = Cache.infer (env ^. has)
        postProcess = PostProcess.expr cachedInfer (env ^. has) prop
        prop = Anchors.repl cp
        setFrozenDeps deps =
            prop ^. Property.mkProperty
            >>= (`Property.pureModify` (Definition.exprFrozenDeps .~ deps))

loadAnnotatedDef ::
    Monad m =>
    (pl -> DefI m) ->
    pl -> T m (Definition.Definition (Val (ValP m)) pl)
loadAnnotatedDef getDefI x =
    getDefI x & ExprLoad.def <&> Definition.defPayload .~ x

loadPanes ::
    ( Monad m, Has LangId env, Has Dir.Layout env
    , Has Debug.Monitors env
    , Has (CurAndPrev (EvalResults (ValI m))) env
    , Has Config env, Has Cache.Functions env, Has Annotations.Mode env
    ) =>
    env -> Anchors.CodeAnchors m -> EntityId ->
    T m
    [Pane InternalName (T m) (T m)
        (Payload InternalName (T m) (T m) [EntityId])]
loadPanes env cp replEntityId =
    do
        Property panes setPanes <- Anchors.panes cp ^. Property.mkProperty
        paneDefs <- traverse (loadAnnotatedDef Anchors.paneDef) panes
        let mkDelPane i =
                entityId <$ setPanes newPanes
                where
                    entityId =
                        newPanes ^? Lens.ix i
                        <|> newPanes ^? Lens.ix (i-1)
                        <&> (EntityId.ofIRef . Anchors.paneDef)
                        & fromMaybe replEntityId
                    newPanes = removeAt i panes
        let movePane oldIndex newIndex =
                insertAt newIndex item (before ++ after)
                & setPanes
                where
                    (before, item:after) = splitAt oldIndex panes
        let mkMMovePaneDown i
                | i+1 < length paneDefs = Just $ movePane i (i+1)
                | otherwise = Nothing
        let mkMMovePaneUp i
                | i-1 >= 0 = Just $ movePane i (i-1)
                | otherwise = Nothing
        let convertPane i def =
                do
                    bodyS <-
                        def
                        <&> Anchors.paneDef
                        & convertDefBody env cp
                    tag <- Anchors.tags cp & convertTaggedEntityWith env defVar
                    defS <-
                        OrderTags.orderDef Definition
                        { _drEntityId = EntityId.ofIRef defI
                        , _drName = tag
                        , _drBody = bodyS
                        , _drDefinitionState =
                            Anchors.assocDefinitionState defI ^. Property.mkProperty
                        , _drDefI = defVar
                        }
                    pure Pane
                        { _paneDefinition = defS
                        , _paneClose = mkDelPane i
                        , _paneMoveDown = mkMMovePaneDown i
                        , _paneMoveUp = mkMMovePaneUp i
                        }
                where
                    defVar = ExprIRef.globalId defI
                    defI = def ^. Definition.defPayload & Anchors.paneDef
        paneDefs & Lens.itraversed %%@~ convertPane

loadWorkArea ::
    ( HasCallStack, Monad m, Has LangId env, Has Dir.Layout env
    , Has Debug.Monitors env
    , Has (CurAndPrev (EvalResults (ValI m))) env
    , Has Config env, Has Cache.Functions env, Has Annotations.Mode env
    ) =>
    env -> Anchors.CodeAnchors m ->
    T m
    (WorkArea InternalName (T m) (T m)
        (Payload InternalName (T m) (T m) [EntityId]))
loadWorkArea env cp =
    do
        repl <- convertRepl env cp
        panes <-
            repl ^. replExpr . SugarLens.binderResultExpr . plEntityId
            & loadPanes env cp
        pure WorkArea
            { _waRepl = repl
            , _waPanes = panes
            , _waGlobals =
                Anchors.globals cp & Property.getP <&> Set.toList
                >>= traverse (ConvertGetVar.globalNameRef cp)
            }
