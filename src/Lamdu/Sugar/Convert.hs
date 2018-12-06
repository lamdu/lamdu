module Lamdu.Sugar.Convert
    ( loadWorkArea, InternalName
    ) where

import           AST (ann, annotations)
import           Control.Applicative ((<|>))
import qualified Control.Lens as Lens
import           Control.Monad.Transaction (MonadTransaction)
import           Data.CurAndPrev (CurAndPrev)
import           Data.List.Extended (insertAt, removeAt)
import           Data.Property (Property(Property))
import qualified Data.Property as Property
import qualified Data.Set as Set
import qualified Lamdu.Cache as Cache
import qualified Lamdu.Calc.Lens as ExprLens
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type.Scheme as Scheme
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Debug as Debug
import           Lamdu.Eval.Results (EvalResults)
import qualified Lamdu.Eval.Results as ER
import           Lamdu.Eval.Results.Process (addTypes)
import           Lamdu.Expr.IRef (DefI, ValI, ValP)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Load as ExprLoad
import           Lamdu.Sugar.Annotations (markNodeAnnotations)
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

import           Lamdu.Prelude

type T = Transaction

convertDefIBuiltin ::
    (MonadTransaction n m, Monad f) =>
    Scheme.Scheme -> Definition.FFIName -> DefI f ->
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

trimParamAnnotation :: Input.AnnotationMode -> Annotation name i -> Annotation name i
trimParamAnnotation Input.None _ = AnnotationNone
trimParamAnnotation Input.Evaluation (AnnotationVal x) =
    x & annotationType .~ Nothing & AnnotationVal
trimParamAnnotation Input.Evaluation _ = AnnotationNone
trimParamAnnotation Input.Types (AnnotationVal x) =
    maybe AnnotationNone AnnotationType (x ^. annotationType)
trimParamAnnotation Input.Types x = x

convertInferDefExpr ::
    (HasCallStack, Monad m) =>
    Cache.Functions -> Debug.Monitors ->
    Input.AnnotationMode -> CurAndPrev (EvalResults (ValI m)) -> Anchors.CodeAnchors m ->
    Scheme.Scheme -> Definition.Expr (Val (ValP m)) -> DefI m ->
    T m (DefinitionBody InternalName (T m) (T m) (Payload InternalName (T m) (T m) [EntityId]))
convertInferDefExpr cache monitors annMode evalRes cp defType defExpr defI =
    do
        Load.InferResult valInferred newInferContext <-
            Load.inferDef cachedInfer monitors evalRes defExpr defVar
            <&> Load.assertInferSuccess
        outdatedDefinitions <-
            OutdatedDefs.scan entityId defExpr setDefExpr postProcess
        let context =
                Context
                { _scInferContext = newInferContext
                , _scCodeAnchors = cp
                , _scScopeInfo =
                    emptyScopeInfo
                    ( Just RecursiveRef
                      { _rrDefI = defI
                      , _rrDefType = defType
                      }
                    )
                , _scDebugMonitors = monitors
                , _scCacheFunctions = cache
                , _scPostProcessRoot = postProcess
                , _scOutdatedDefinitions = outdatedDefinitions
                , _scInlineableDefinition = canInlineDefinition valInferred (Set.singleton defVar)
                , _scFrozenDeps =
                    Property (defExpr ^. Definition.exprFrozenDeps) setFrozenDeps
                , scConvertSubexpression = ConvertExpr.convert
                }
        ConvertDefExpr.convert
            defType (defExpr & Definition.expr .~ valInferred) defI
            <&> _DefinitionBodyExpression . deContent %~ markNodeAnnotations
            >>= (_DefinitionBodyExpression . deContent . annotations) (convertPayload annMode)
            & ConvertM.run context
            <&> _DefinitionBodyExpression . deContent . SugarLens.assignmentSubExprParams .
                SugarLens.paramsAnnotations %~ trimParamAnnotation annMode
    where
        cachedInfer = Cache.infer cache
        postProcess = PostProcess.def cachedInfer monitors defI
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
    (HasCallStack, Monad m) =>
    Cache.Functions -> Debug.Monitors ->
    Input.AnnotationMode -> CurAndPrev (EvalResults (ValI m)) -> Anchors.CodeAnchors m ->
    Definition.Definition (Val (ValP m)) (DefI m) ->
    T m
    (DefinitionBody InternalName (T m) (T m) (Payload InternalName (T m) (T m) [EntityId]))
convertDefBody cache monitors annMode evalRes cp (Definition.Definition bod defType defI) =
    case bod of
    Definition.BodyExpr defExpr -> convertInferDefExpr cache monitors annMode evalRes cp defType defExpr defI
    Definition.BodyBuiltin builtin -> convertDefIBuiltin defType builtin defI

convertRepl ::
    (HasCallStack, Monad m) =>
    Cache.Functions -> Debug.Monitors ->
    Input.AnnotationMode -> CurAndPrev (EvalResults (ValI m)) -> Anchors.CodeAnchors m ->
    T m
    (Repl InternalName (T m) (T m)
        (Payload InternalName (T m) (T m) [EntityId]))
convertRepl cache monitors annMode evalRes cp =
    do
        defExpr <- ExprLoad.defExpr prop
        entityId <- Property.getP prop <&> (^. Definition.expr) <&> EntityId.ofValI
        Load.InferResult valInferred newInferContext <-
            Load.inferDefExpr cachedInfer monitors evalRes defExpr
            <&> Load.assertInferSuccess
        outdatedDefinitions <-
            OutdatedDefs.scan entityId defExpr (Property.setP prop) postProcess
        let context =
                Context
                { _scInferContext = newInferContext
                , _scCodeAnchors = cp
                , _scScopeInfo = emptyScopeInfo Nothing
                , _scDebugMonitors = monitors
                , _scCacheFunctions = cache
                , _scPostProcessRoot = postProcess
                , _scOutdatedDefinitions = outdatedDefinitions
                , _scInlineableDefinition = canInlineDefinition valInferred mempty
                , _scFrozenDeps =
                    Property (defExpr ^. Definition.exprFrozenDeps) setFrozenDeps
                , scConvertSubexpression = ConvertExpr.convert
                }
        nomsMap <-
            valInferred ^.. annotations . Input.inferredType & Load.makeNominalsMap
        let typ = valInferred ^. ann . Input.inferredType
        let completion =
                evalRes
                <&> (^. ER.erCompleted)
                <&> Lens._Just . Lens._Right %~ addTypes nomsMap typ
        expr <-
            convertBinder valInferred
            <&> markNodeAnnotations
            >>= annotations (convertPayload annMode)
            & ConvertM.run context
            <&> SugarLens.binderSubExprParams . SugarLens.paramsAnnotations %~
                trimParamAnnotation annMode
            >>= OrderTags.orderNode
        let replEntityId = expr ^. SugarLens.binderResultExpr . plEntityId
        pure Repl
            { _replExpr = expr
            , _replVarInfo = mkVarInfo typ
            , _replResult = ConvertEval.completion cp replEntityId completion
            }
    where
        cachedInfer = Cache.infer cache
        postProcess = PostProcess.expr cachedInfer monitors prop
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
    Monad m =>
    Cache.Functions -> Debug.Monitors ->
    Input.AnnotationMode -> CurAndPrev (EvalResults (ValI m)) ->
    Anchors.CodeAnchors m -> EntityId ->
    T m
    [Pane InternalName (T m) (T m)
        (Payload InternalName (T m) (T m) [EntityId])]
loadPanes cache monitors annMode evalRes cp replEntityId =
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
                        & convertDefBody cache monitors annMode evalRes cp
                    tag <- Anchors.tags cp & convertTaggedEntityWith defVar
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
    (HasCallStack, Monad m) =>
    Cache.Functions -> Debug.Monitors ->
    Input.AnnotationMode -> CurAndPrev (EvalResults (ValI m)) ->
    Anchors.CodeAnchors m ->
    T m
    (WorkArea InternalName (T m) (T m)
        (Payload InternalName (T m) (T m) [EntityId]))
loadWorkArea cache monitors annMode evalRes cp =
    do
        repl <- convertRepl cache monitors annMode evalRes cp
        panes <-
            loadPanes cache monitors annMode evalRes cp
            (repl ^. replExpr . SugarLens.binderResultExpr . plEntityId)
        pure WorkArea
            { _waRepl = repl
            , _waPanes = panes
            , _waGlobals =
                Anchors.globals cp & Property.getP <&> Set.toList
                >>= traverse (ConvertGetVar.globalNameRef cp)
            }
