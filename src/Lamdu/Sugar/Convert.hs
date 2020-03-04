{-# LANGUAGE TypeApplications #-}
module Lamdu.Sugar.Convert
    ( loadWorkArea, InternalName
    ) where

import           Control.Applicative ((<|>))
import qualified Control.Lens as Lens
import           Control.Monad.Transaction (MonadTransaction)
import           Data.CurAndPrev (CurAndPrev)
import           Data.List.Extended (insertAt, removeAt)
import           Data.Property (Property(Property))
import qualified Data.Property as Property
import qualified Data.Set as Set
import           Hyper
import           Hyper.Class.Infer.InferOf
import           Hyper.Class.Nodes
import           Hyper.Infer (InferOf)
import qualified Lamdu.Annotations as Annotations
import qualified Lamdu.Cache as Cache
import qualified Lamdu.Calc.Lens as ExprLens
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Tag as Tag
import qualified Lamdu.Debug as Debug
import           Lamdu.Eval.Results (EvalResults)
import qualified Lamdu.Eval.Results as ER
import           Lamdu.Eval.Results.Process (addTypes)
import           Lamdu.Expr.IRef (DefI, HRef)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Load as ExprLoad
import           Lamdu.Sugar.Annotations (ShowAnnotation, MarkAnnotations, markNodeAnnotations, alwaysShowAnnotations)
import           Lamdu.Sugar.Config (Config, showAllAnnotations)
import           Lamdu.Sugar.Convert.Binder (convertBinder)
import           Lamdu.Sugar.Convert.Binder.Params (mkVarInfo)
import qualified Lamdu.Sugar.Convert.DefExpr as ConvertDefExpr
import qualified Lamdu.Sugar.Convert.DefExpr.OutdatedDefs as OutdatedDefs
import qualified Lamdu.Sugar.Convert.Eval as ConvertEval
import qualified Lamdu.Sugar.Convert.Expression as ConvertExpr
import           Lamdu.Sugar.Convert.Expression.Actions (convertPayloads)
import qualified Lamdu.Sugar.Convert.GetVar as ConvertGetVar
import qualified Lamdu.Sugar.Convert.Input as Input
import qualified Lamdu.Sugar.Convert.Load as Load
import           Lamdu.Sugar.Convert.Monad (Context(..), ScopeInfo(..), RecursiveRef(..))
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.Convert.PostProcess as PostProcess
import qualified Lamdu.Sugar.Convert.Tag as ConvertTag
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
    Pure # T.Scheme -> Definition.FFIName -> DefI f ->
    m (DefinitionBody v InternalName i (T f) a)
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

assertInferSuccess :: HasCallStack => Either (Pure # T.TypeError) a -> a
assertInferSuccess =
    either (error . ("Type inference failed: " ++) . show . pPrint) id

convertInferDefExpr ::
    ( HasCallStack, Monad m
    , Has Debug.Monitors env
    , Has Config env, Has Cache.Functions env, Has Annotations.Mode env
    ) =>
    env -> Anchors.CodeAnchors m ->
    Pure # T.Scheme -> Definition.Expr (Ann (HRef m) # V.Term) -> DefI m ->
    T m
    (DefinitionBody
        (EvaluationScopes InternalName (T m)) InternalName (T m) (T m)
        (Payload (EvaluationScopes InternalName (T m)) InternalName (T m) (T m) [EntityId]))
convertInferDefExpr env cp defType defExpr defI =
    do
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
                , _scTopLevelExpr = valInferred
                , _scPostProcessRoot = postProcess
                , _scOutdatedDefinitions = outdatedDefinitions
                , _scFrozenDeps =
                    Property (defExpr ^. Definition.exprFrozenDeps) setFrozenDeps
                , _scAnnotationsMode = env ^. has
                , scConvertSubexpression = ConvertExpr.convert
                }
        ConvertDefExpr.convert
            defType (defExpr & Definition.expr .~ valInferred) defI
            >>= (_DefinitionBodyExpression . deContent) (convertPayloads . markAnnotations (env ^. has))
            & ConvertM.run context
    where
        Load.InferOut valInferred newInferContext =
            Load.inferDef cachedInfer (env ^. has) defExpr defVar
            & assertInferSuccess
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
    ( HasCallStack, Monad m
    , Has Debug.Monitors env
    , Has Config env, Has Cache.Functions env, Has Annotations.Mode env
    ) =>
    env -> Anchors.CodeAnchors m ->
    Definition.Definition (Ann (HRef m) # V.Term) (DefI m) ->
    T m
    (DefinitionBody (EvaluationScopes InternalName (T m)) InternalName (T m) (T m)
        (Payload (EvaluationScopes InternalName (T m)) InternalName (T m) (T m) [EntityId]))
convertDefBody env cp (Definition.Definition bod defType defI) =
    case bod of
    Definition.BodyBuiltin builtin -> convertDefIBuiltin defType builtin defI
    Definition.BodyExpr defExpr ->
        convertInferDefExpr env cp defType defExpr defI

markAnnotations ::
    (MarkAnnotations t, RTraversable t) =>
    Config ->
    Annotated a # t ->
    Annotated (ShowAnnotation, a) # t
markAnnotations config
    | config ^. showAllAnnotations =
        hflipped %~ hmap (const (Lens._Wrapped %~ (,) alwaysShowAnnotations))
    | otherwise = markNodeAnnotations

convertRepl ::
    ( HasCallStack, Monad m
    , Has Debug.Monitors env
    , Has (CurAndPrev EvalResults) env
    , Has Config env, Has Cache.Functions env, Has Annotations.Mode env
    ) =>
    env -> Anchors.CodeAnchors m ->
    T m
    (Repl (EvaluationScopes InternalName (T m)) InternalName (T m) (T m)
        (Payload (EvaluationScopes InternalName (T m)) InternalName (T m) (T m) [EntityId]))
convertRepl env cp =
    do
        defExpr <- ExprLoad.defExpr prop
        entityId <- Property.getP prop <&> (^. Definition.expr) <&> EntityId.ofValI
        let Load.InferOut valInferred newInferContext =
                Load.inferDefExpr cachedInfer (env ^. has) defExpr
                & assertInferSuccess
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
                , _scTopLevelExpr = valInferred
                , _scPostProcessRoot = postProcess
                , _scOutdatedDefinitions = outdatedDefinitions
                , _scFrozenDeps =
                    Property (defExpr ^. Definition.exprFrozenDeps) setFrozenDeps
                , _scAnnotationsMode = env ^. has
                , scConvertSubexpression = ConvertExpr.convert
                }
        let typ = valInferred ^. hAnn . Input.inferredType
        nomsMap <-
            hfoldMap
            ( Proxy @(Recursively (InferOfConstraint HFoldable)) #*#
                Proxy @(Recursively (InferOfConstraint (HNodesHaveConstraint ExprLens.HasTIds))) #*#
                \w ->
                withDict (recursively (p0 w)) $
                withDict (recursively (p1 w)) $
                withDict (inferOfConstraint (Proxy @HFoldable) w) $
                withDict (inferOfConstraint (Proxy @(HNodesHaveConstraint ExprLens.HasTIds)) w) $
                withDict (hNodesHaveConstraint (Proxy @ExprLens.HasTIds) (p2 w)) $
                hfoldMap
                ( Proxy @ExprLens.HasTIds #> (^.. _1 . ExprLens.tIds)
                ) . (^. Input.inferRes . hflipped)
            ) (_HFlip # valInferred)
            & Load.makeNominalsMap
        let completion =
                env ^. has
                <&> (^. ER.erCompleted)
                <&> Lens._Just . Lens._Right %~ addTypes nomsMap typ
        expr <-
            convertBinder valInferred
            <&> markAnnotations (env ^. has)
            >>= convertPayloads
            & ConvertM.run context
            >>= OrderTags.orderNode
        let replEntityId = expr ^. SugarLens.binderResultExpr . plEntityId
        typS <- ConvertType.convertType (EntityId.ofTypeOf replEntityId) typ
        pure Repl
            { _replExpr = expr
            , _replVarInfo = mkVarInfo typS
            , _replResult = ConvertEval.completion cp replEntityId completion
            }
    where
        p0 :: proxy h -> Proxy (InferOfConstraint HFoldable h)
        p0 _ = Proxy
        p1 :: proxy h -> Proxy (InferOfConstraint (HNodesHaveConstraint ExprLens.HasTIds) h)
        p1 _ = Proxy
        p2 :: proxy h -> Proxy (InferOf h)
        p2 _ = Proxy
        cachedInfer = Cache.infer (env ^. has)
        postProcess = PostProcess.expr cachedInfer (env ^. has) prop
        prop = Anchors.repl cp
        setFrozenDeps deps =
            prop ^. Property.mkProperty
            >>= (`Property.pureModify` (Definition.exprFrozenDeps .~ deps))

convertPaneBody ::
    ( Monad m
    , Has Debug.Monitors env
    , Has Config env, Has Cache.Functions env, Has Annotations.Mode env
    ) =>
    env -> Anchors.CodeAnchors m -> Anchors.Pane m ->
    T m
    (PaneBody (EvaluationScopes InternalName (T m)) InternalName (T m) (T m)
        (Payload (EvaluationScopes InternalName (T m)) InternalName (T m) (T m) [EntityId]))
convertPaneBody _ _ (Anchors.PaneTag tagId) =
    ExprIRef.readTagData tagId <&>
    \tagData ->
    PaneTag TagPane
    { _tpTag =
        Tag
        { _tagName = nameWithoutContext tagId
        , _tagInstance = EntityId.ofTagPane tagId
        , _tagVal = tagId
        }
    , _tpTagData = tagData
    , _tpSetSymbol = \sym -> tagData & Tag.tagSymbol .~ sym & writeTag
    , _tpSetTexts =
            \langId text -> tagData & Tag.tagTexts . Lens.at langId ?~ text & writeTag
    }
    where
        writeTag = Transaction.writeIRef (ExprIRef.tagI tagId)
convertPaneBody env cp (Anchors.PaneDefinition defI) =
    do
        bodyS <-
            ExprLoad.def defI <&> Definition.defPayload .~ defI
            >>= convertDefBody env cp
        tag <- Anchors.tags cp & ConvertTag.taggedEntityWith cp defVar
        defState <- Anchors.assocDefinitionState defI ^. Property.mkProperty
        OrderTags.orderDef Definition
            { _drEntityId = EntityId.ofIRef defI
            , _drName = tag
            , _drBody = bodyS
            , _drDefinitionState = defState
            , _drDefI = defVar
            } <&> PaneDefinition
    where
        defVar = ExprIRef.globalId defI

paneEntityId :: Anchors.Pane dummy -> EntityId
paneEntityId (Anchors.PaneDefinition defI) = EntityId.ofIRef defI
paneEntityId (Anchors.PaneTag tag) = EntityId.ofTagPane tag

convertPane ::
    ( Monad m
    , Has Debug.Monitors env
    , Has Config env, Has Cache.Functions env, Has Annotations.Mode env
    ) =>
    env -> Anchors.CodeAnchors m -> EntityId ->
    Property (T m) [Anchors.Pane dummy] ->
    Int -> Anchors.Pane m ->
    T m
    (Pane (EvaluationScopes InternalName (T m)) InternalName (T m) (T m)
        (Payload (EvaluationScopes InternalName (T m)) InternalName (T m) (T m) [EntityId]))
convertPane env cp replEntityId (Property panes setPanes) i pane =
    convertPaneBody env cp pane
    <&> \body -> Pane
    { _paneBody = body
    , _paneClose = mkDelPane
    , _paneMoveDown = mkMMovePaneDown
    , _paneMoveUp = mkMMovePaneUp
    }
    where
        mkDelPane =
            entityId <$ setPanes newPanes
            where
                entityId =
                    newPanes ^? Lens.ix i
                    <|> newPanes ^? Lens.ix (i-1)
                    <&> paneEntityId
                    & fromMaybe replEntityId
                newPanes = removeAt i panes
        movePane oldIndex newIndex =
            insertAt newIndex item (before ++ after)
            & setPanes
            where
                (before, item:after) = splitAt oldIndex panes
        mkMMovePaneDown
            | i+1 < length panes = Just $ movePane i (i+1)
            | otherwise = Nothing
        mkMMovePaneUp
            | i-1 >= 0 = Just $ movePane i (i-1)
            | otherwise = Nothing

loadPanes ::
    ( Monad m
    , Has Debug.Monitors env
    , Has Config env, Has Cache.Functions env, Has Annotations.Mode env
    ) =>
    env -> Anchors.CodeAnchors m -> EntityId ->
    T m
    [Pane (EvaluationScopes InternalName (T m)) InternalName (T m) (T m)
        (Payload (EvaluationScopes InternalName (T m)) InternalName (T m) (T m) [EntityId])]
loadPanes env cp replEntityId =
    do
        prop <- Anchors.panes cp ^. Property.mkProperty
        Property.value prop
            & Lens.itraversed %%@~ convertPane env cp replEntityId prop

loadWorkArea ::
    ( HasCallStack, Monad m
    , Has Debug.Monitors env
    , Has (CurAndPrev EvalResults) env
    , Has Config env, Has Cache.Functions env, Has Annotations.Mode env
    ) =>
    env -> Anchors.CodeAnchors m ->
    T m
    (WorkArea (EvaluationScopes InternalName (T m)) InternalName (T m) (T m)
        (Payload (EvaluationScopes InternalName (T m)) InternalName (T m) (T m) [EntityId]))
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
