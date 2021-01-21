module Lamdu.Sugar.Convert
    ( loadWorkArea, InternalName
    ) where

import           Control.Applicative ((<|>))
import qualified Control.Lens as Lens
import           Control.Monad.Once (OnceT, Typeable)
import           Control.Monad.Transaction (MonadTransaction)
import           Data.CurAndPrev (CurAndPrev(..))
import           Data.List.Extended (insertAt, removeAt)
import           Data.Property (Property(Property))
import qualified Data.Property as Property
import qualified Data.Set as Set
import           Hyper
import qualified Lamdu.Cache as Cache
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Tag as Tag
import qualified Lamdu.Debug as Debug
import           Lamdu.Expr.IRef (DefI, HRef)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Load as ExprLoad
import           Lamdu.Sugar.Config (Config)
import           Lamdu.Sugar.Convert.Binder (convertBinder)
import           Lamdu.Sugar.Convert.Binder.Params (mkVarInfo)
import qualified Lamdu.Sugar.Convert.DefExpr as ConvertDefExpr
import qualified Lamdu.Sugar.Convert.DefExpr.OutdatedDefs as OutdatedDefs
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
    ( HasCallStack, Monad m, Typeable m
    , Has Debug.Monitors env
    , Has Config env, Has Cache.Functions env
    ) =>
    env -> Anchors.CodeAnchors m ->
    Pure # T.Scheme -> Definition.Expr (Ann (HRef m) # V.Term) -> DefI m ->
    OnceT (T m)
    (DefinitionBody EvalPrep InternalName (OnceT (T m)) (T m)
        (Payload EvalPrep InternalName (OnceT (T m)) (T m), [EntityId]))
convertInferDefExpr env cp defType defExpr defI =
    do
        outdatedDefinitions <-
            OutdatedDefs.scan entityId defExpr setDefExpr postProcess
            & lift
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
                , scConvertSubexpression = ConvertExpr.convert
                }
        ConvertDefExpr.convert
            defType (defExpr & Definition.expr .~ valInferred) defI
            <&> _DefinitionBodyExpression . deContent %~ convertPayloads
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
    ( HasCallStack, Monad m, Typeable m
    , Has Debug.Monitors env
    , Has Config env, Has Cache.Functions env
    ) =>
    env -> Anchors.CodeAnchors m ->
    Definition.Definition (Ann (HRef m) # V.Term) (DefI m) ->
    OnceT (T m)
    (DefinitionBody EvalPrep InternalName (OnceT (T m)) (T m)
        (Payload EvalPrep InternalName (OnceT (T m)) (T m), [EntityId]))
convertDefBody env cp (Definition.Definition bod defType defI) =
    case bod of
    Definition.BodyBuiltin builtin -> convertDefIBuiltin defType builtin defI & lift
    Definition.BodyExpr defExpr ->
        convertInferDefExpr env cp defType defExpr defI

convertRepl ::
    ( HasCallStack, Monad m, Typeable m
    , Has Debug.Monitors env
    , Has Config env, Has Cache.Functions env
    ) =>
    env -> Anchors.CodeAnchors m ->
    OnceT (T m)
    (Repl EvalPrep InternalName (OnceT (T m)) (T m)
        (Payload EvalPrep InternalName (OnceT (T m)) (T m), [EntityId]))
convertRepl env cp =
    do
        defExpr <- ExprLoad.defExpr prop & lift
        entityId <- Property.getP prop & lift <&> (^. Definition.expr) <&> EntityId.ofValI
        let Load.InferOut valInferred newInferContext =
                Load.inferDefExpr cachedInfer (env ^. has) defExpr
                & assertInferSuccess
        outdatedDefinitions <-
            OutdatedDefs.scan entityId defExpr (Property.setP prop) postProcess & lift
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
                , scConvertSubexpression = ConvertExpr.convert
                }
        let typ = valInferred ^. hAnn . Input.inferredType
        expr <-
            convertBinder valInferred
            <&> convertPayloads
            & ConvertM.run context
            >>= OrderTags.orderNode
        vinfo <- mkVarInfo typ
        pure Repl
            { _replExpr = expr
            , _replVarInfo = vinfo
            , _replResult = CurAndPrev Nothing Nothing
            }
    where
        cachedInfer = Cache.infer (env ^. has)
        postProcess = PostProcess.expr cachedInfer (env ^. has) prop
        prop = Anchors.repl cp
        setFrozenDeps deps =
            prop ^. Property.mkProperty
            >>= (`Property.pureModify` (Definition.exprFrozenDeps .~ deps))

convertPaneBody ::
    ( Monad m, Typeable m
    , Has Debug.Monitors env
    , Has Config env, Has Cache.Functions env
    ) =>
    env -> Anchors.CodeAnchors m -> Anchors.Pane m ->
    OnceT (T m)
    (PaneBody EvalPrep InternalName (OnceT (T m)) (T m)
        (Payload EvalPrep InternalName (OnceT (T m)) (T m), [EntityId]))
convertPaneBody _ _ (Anchors.PaneTag tagId) =
    ExprIRef.readTagData tagId & lift <&>
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
            ExprLoad.def defI & lift <&> Definition.defPayload .~ defI
            >>= convertDefBody env cp
        tag <- ConvertTag.taggedEntityWith cp defVar & join
        defState <- Anchors.assocDefinitionState defI ^. Property.mkProperty & lift
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
    ( Monad m, Typeable m
    , Has Debug.Monitors env
    , Has Config env, Has Cache.Functions env
    ) =>
    env -> Anchors.CodeAnchors m -> EntityId ->
    Property (T m) [Anchors.Pane dummy] ->
    Int -> Anchors.Pane m ->
    OnceT (T m)
    (Pane EvalPrep InternalName (OnceT (T m)) (T m)
        (Payload EvalPrep InternalName (OnceT (T m)) (T m), [EntityId]))
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
    ( Monad m, Typeable m
    , Has Debug.Monitors env
    , Has Config env, Has Cache.Functions env
    ) =>
    env -> Anchors.CodeAnchors m -> EntityId ->
    OnceT (T m)
    [Pane EvalPrep InternalName (OnceT (T m)) (T m)
        (Payload EvalPrep InternalName (OnceT (T m)) (T m), [EntityId])]
loadPanes env cp replEntityId =
    do
        prop <- Anchors.panes cp ^. Property.mkProperty & lift
        Property.value prop
            & Lens.itraversed %%@~ convertPane env cp replEntityId prop

loadWorkArea ::
    ( HasCallStack, Monad m, Typeable m
    , Has Debug.Monitors env
    , Has Config env, Has Cache.Functions env
    ) =>
    env -> Anchors.CodeAnchors m ->
    OnceT (T m)
    (WorkArea EvalPrep InternalName (OnceT (T m)) (T m)
        (Payload EvalPrep InternalName (OnceT (T m)) (T m), [EntityId]))
loadWorkArea env cp =
    do
        repl <- convertRepl env cp
        panes <-
            repl ^. replExpr . SugarLens.binderResultExpr . _1 . plEntityId
            & loadPanes env cp
        pure WorkArea
            { _waRepl = repl
            , _waPanes = panes
            , _waGlobals =
                Anchors.globals cp & Property.getP & lift <&> Set.toList
                >>= traverse (ConvertGetVar.globalNameRef cp)
            }
