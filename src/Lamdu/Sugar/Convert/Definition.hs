{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}

module Lamdu.Sugar.Convert.Definition
    ( pane
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Once (OnceT, Typeable)
import           Control.Monad.Reader (ReaderT(..))
import           Control.Monad.Transaction (MonadTransaction)
import qualified Data.Map as Map
import           Data.Property (Property(Property))
import qualified Data.Property as Property
import qualified Data.Set as Set
import           Hyper
import           Hyper.Recurse (Recursive(..))
import qualified Lamdu.Cache as Cache
import           Lamdu.Calc.Definition (depsGlobalTypes)
import           Lamdu.Calc.Infer (alphaEq)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Debug as Debug
import           Lamdu.Expr.IRef (DefI, HRef)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Load as ExprLoad
import           Lamdu.Sugar.Config (Sugars)
import qualified Lamdu.Sugar.Convert.DefExpr as ConvertDefExpr
import qualified Lamdu.Sugar.Convert.DefExpr.OutdatedDefs as OutdatedDefs
import qualified Lamdu.Sugar.Convert.Expression as ConvertExpr
import qualified Lamdu.Sugar.Convert.Input as Input
import qualified Lamdu.Sugar.Convert.Load as Load
import           Lamdu.Sugar.Convert.Monad (Context(..), ScopeInfo(..), RecursiveRef(..))
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Convert.NameRef (jumpToDefinition)
import qualified Lamdu.Sugar.Convert.PostProcess as PostProcess
import qualified Lamdu.Sugar.Convert.Tag as ConvertTag
import qualified Lamdu.Sugar.Convert.Type as ConvertType
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction
import           Text.PrettyPrint.HughesPJClass (Pretty(..))

import           Lamdu.Prelude

type T = Transaction

assertInferSuccess :: HasCallStack => Either (Pure # T.TypeError) a -> a
assertInferSuccess =
    either (error . ("Type inference failed: " ++) . show . pPrint) id

emptyScopeInfo :: Maybe (RecursiveRef m) -> ScopeInfo m
emptyScopeInfo recursiveRef =
    ScopeInfo
    { _siRecordParams = mempty
    , _siNullParams = mempty
    , _siLetItems = mempty
    , _siExtractPos = Nothing
    , _siFloatPos = Nothing
    , _siRecursiveRef = recursiveRef
    }

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

convertInferDefExpr ::
    ( HasCallStack, Monad m, Typeable m
    , Has Debug.Monitors env
    , Has (Sugars Bool) env, Has Cache.Functions env
    , Anchors.HasCodeAnchors env m
    ) =>
    env ->
    Pure # T.Scheme -> Definition.Expr (Ann (HRef m) # V.Term) -> DefI m ->
    OnceT (T m)
    (DefinitionBody EvalPrep InternalName (OnceT (T m)) (T m) ([EntityId], ConvertPayload m))
convertInferDefExpr env defType defExpr defI =
    do
        outdatedDefinitions <-
            OutdatedDefs.scan entityId defExpr setDefExpr postProcess
            & (`runReaderT` env)
            & lift
            <&> Lens.mapped . defTypeUseCurrent %~ (<* postProcess)
        let context =
                Context
                { _scInferContext = newInferContext
                , _scSugars = env ^. has
                , _scCodeAnchors = env ^. Anchors.codeAnchors
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
            & ConvertM.run context
            <&> _DefinitionBodyExpression . deContent %~ collectHiddenEntityIds valInferred
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
    , Has (Sugars Bool) env, Has Cache.Functions env
    , Anchors.HasCodeAnchors env m
    ) =>
    env ->
    Definition.Definition (Ann (HRef m) # V.Term) (DefI m) ->
    OnceT (T m)
    (DefinitionBody EvalPrep InternalName (OnceT (T m)) (T m) ([EntityId], ConvertPayload m))
convertDefBody env (Definition.Definition bod defType defI) =
    case bod of
    Definition.BodyBuiltin builtin ->
        convertDefIBuiltin defType builtin defI
        & (`runReaderT` env)
        & lift
    Definition.BodyExpr defExpr ->
        convertInferDefExpr env defType defExpr defI

collectHiddenEntityIds ::
    forall h m.
    RTraversable h =>
    Ann (Input.Payload m) # V.Term ->
    Annotated (ConvertPayload m) # h ->
    Annotated ([EntityId], ConvertPayload m) # h
collectHiddenEntityIds top expr =
    withDict (recurse (Proxy @(RTraversable h))) $
    let nodes =
            expr ^. hflipped
            & hfoldMap (const (^.. Lens._Wrapped . pUnsugared . hAnn . Input.entityId))
            & Set.fromList
        goBody :: forall p. Recursively HFoldable p => p # Ann (Input.Payload m) -> [EntityId]
        goBody =
            withDict (recursively (Proxy @(HFoldable p))) $
            hfoldMap (Proxy @(Recursively HFoldable) #> goNode)
        goNode x
            | nodes ^. Lens.contains (x ^. hAnn . Input.entityId) = []
            | otherwise = x ^. hAnn . Input.entityId : goBody (x ^. hVal)
    in
    expr
    & hflipped %~ hmap (const (Lens._Wrapped %~
        \x ->
        ( filter (/= (x ^. pEntityId))
            (x ^. pUnsugared . hAnn . Input.entityId : goBody (x ^. pUnsugared . hVal))
        , x
        )
    ))
    & annotation . _1 <>~ goNode top

pane ::
    ( HasCallStack, Has Debug.Monitors env, Has Cache.Functions env
    , Has (Sugars Bool) env, Monad m, Typeable m, Anchors.HasCodeAnchors env m
    ) =>
    env -> DefI m -> OnceT (T m) (PaneBody EvalPrep InternalName (OnceT (T m)) (T m) ([EntityId], ConvertPayload m))
pane env defI =
    Definition
    <$> ConvertTag.taggedEntityWith (env ^. Anchors.codeAnchors) Nothing defVar
    <*> pure defVar
    <*> (ExprLoad.def defI & lift <&> Definition.defPayload .~ defI >>= convertDefBody env)
    ?? (nextOutdatedDef (env ^. Anchors.codeAnchors) defI
        >>= Lens._Just (jumpToDefinition (env ^. Anchors.codeAnchors)))
    <&> PaneDefinition
    where
        defVar = ExprIRef.globalId defI

nextOutdatedDef :: Monad m => Anchors.CodeAnchors m -> DefI m -> T m (Maybe (DefI m))
nextOutdatedDef anchors def =
    Property.getP (Anchors.globals anchors) >>= sequenceA . Map.fromSet Transaction.readIRef <&>
    \defs->
    let checkDef (v, s) =
            case defs ^. Lens.at (ExprIRef.defI v) of
            Nothing -> True
            Just d -> d ^. Definition.defType /= s && not (alphaEq (d ^. Definition.defType) s)
        isOutdated d =
            any checkDef (deps ^@.. depsGlobalTypes . Lens.ifolded)
            -- TODO: Also check nominals!
            where
                deps = d ^. Definition.defBody . Definition._BodyExpr . Definition.exprFrozenDeps
    in
    case break (== def) (defs ^.. Lens.ifolded . Lens.filtered isOutdated . Lens.asIndex) of
    (_, _:x:_) -> Just x
    (x:_, _) -> Just x
    _ -> Nothing
