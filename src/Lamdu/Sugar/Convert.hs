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
import qualified Lamdu.Calc.Type.Scheme as Scheme
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val(..))
import qualified Lamdu.Calc.Val.Annotated as Val
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Debug as Debug
import           Lamdu.Eval.Results (EvalResults)
import qualified Lamdu.Eval.Results as ER
import           Lamdu.Eval.Results.Process (addTypes)
import           Lamdu.Expr.IRef (DefI, ValI, ValIProperty)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.Load as ExprLoad
import qualified Lamdu.Sugar.Convert.DefExpr as ConvertDefExpr
import qualified Lamdu.Sugar.Convert.DefExpr.OutdatedDefs as OutdatedDefs
import qualified Lamdu.Sugar.Convert.Eval as ConvertEval
import qualified Lamdu.Sugar.Convert.Expression as ConvertExpr
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
import qualified Lamdu.Sugar.OrderTags as OrderTags
import qualified Lamdu.Sugar.PresentationModes as PresentationModes
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction

import           Lamdu.Prelude

type T = Transaction

convertDefIBuiltin ::
    (MonadTransaction n m, Monad f) =>
    Scheme.Scheme -> Definition.FFIName -> DefI f ->
    m (DefinitionBody InternalName i (T f) (ExpressionU f [EntityId]))
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

convertInferDefExpr ::
    (HasCallStack, Monad m) =>
    Debug.Monitors -> CurAndPrev (EvalResults (ValI m)) -> Anchors.CodeAnchors m ->
    Scheme.Scheme -> Definition.Expr (Val (ValIProperty m)) -> DefI m ->
    T m (DefinitionBody InternalName (T m) (T m) (ExpressionU m [EntityId]))
convertInferDefExpr monitors evalRes cp defType defExpr defI =
    do
        Load.InferResult valInferred newInferContext <-
            Load.inferDef monitors evalRes defExpr defVar
            <&> Load.assertInferSuccess
        outdatedDefinitions <-
            OutdatedDefs.scan entityId defExpr setDefExpr postProcess
            <&> Lens.mapped . defTypeUseCurrent %~ (<* postProcess)
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
                , _scPostProcessRoot = postProcess
                , _scOutdatedDefinitions = outdatedDefinitions
                , _scInlineableDefinition = canInlineDefinition valInferred (Set.singleton defVar)
                , _scFrozenDeps =
                    Property (defExpr ^. Definition.exprFrozenDeps) setFrozenDeps
                , scConvertSubexpression = ConvertExpr.convert
                }
        ConvertDefExpr.convert
            defType (defExpr & Definition.expr .~ valInferred) defI
            & ConvertM.run context
    where
        postProcess = PostProcess.def monitors defI
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
    Debug.Monitors -> CurAndPrev (EvalResults (ValI m)) -> Anchors.CodeAnchors m ->
    Definition.Definition (Val (ValIProperty m)) (DefI m) ->
    T m (DefinitionBody InternalName (T m) (T m) (ExpressionU m [EntityId]))
convertDefBody monitors evalRes cp (Definition.Definition body defType defI) =
    case body of
    Definition.BodyExpr defExpr -> convertInferDefExpr monitors evalRes cp defType defExpr defI
    Definition.BodyBuiltin builtin -> convertDefIBuiltin defType builtin defI

loadRepl ::
    (HasCallStack, Monad m) =>
    Debug.Monitors -> CurAndPrev (EvalResults (ValI m)) -> Anchors.CodeAnchors m ->
    T m (Repl InternalName (T m) (T m) [EntityId])
loadRepl monitors evalRes cp =
    do
        defExpr <- ExprLoad.defExprProperty prop
        entityId <- Property.getP prop <&> (^. Definition.expr) <&> EntityId.ofValI
        Load.InferResult valInferred newInferContext <-
            Load.inferDefExpr monitors evalRes defExpr
            <&> Load.assertInferSuccess
        outdatedDefinitions <- OutdatedDefs.scan entityId defExpr (Property.setP prop) postProcess
        let context =
                Context
                { _scInferContext = newInferContext
                , _scCodeAnchors = cp
                , _scScopeInfo = emptyScopeInfo Nothing
                , _scDebugMonitors = monitors
                , _scPostProcessRoot = postProcess
                , _scOutdatedDefinitions = outdatedDefinitions
                , _scInlineableDefinition = canInlineDefinition valInferred mempty
                , _scFrozenDeps =
                    Property (defExpr ^. Definition.exprFrozenDeps) setFrozenDeps
                , scConvertSubexpression = ConvertExpr.convert
                }
        nomsMap <-
            valInferred ^.. Lens.folded . Input.inferredType & Load.makeNominalsMap
        let typ = valInferred ^. Val.payload . Input.inferredType
        let completion =
                evalRes
                <&> (^. ER.erCompleted)
                <&> Lens._Just . Lens._Right %~ addTypes nomsMap typ
        expr <-
            ConvertM.convertSubexpression valInferred
            & ConvertM.run context
            <&> Lens.mapped %~ (^. pUserData)
            >>= PresentationModes.addToExpr
            >>= OrderTags.orderExpr
        let replEntityId = expr ^. rPayload . plEntityId
        pure Repl
            { _replExpr = expr
            , _replResult = ConvertEval.completion cp replEntityId completion
            }
    where
        postProcess = PostProcess.expr monitors prop
        prop = Anchors.repl cp
        setFrozenDeps deps =
            prop ^. Property.mkProperty
            >>= (`Property.pureModify` (Definition.exprFrozenDeps .~ deps))

loadAnnotatedDef ::
    Monad m =>
    (pl -> DefI m) ->
    pl -> T m (Definition.Definition (Val (ValIProperty m)) pl)
loadAnnotatedDef getDefI annotation =
    getDefI annotation & ExprLoad.def <&> Definition.defPayload .~ annotation

loadPanes ::
    Monad m =>
    Debug.Monitors -> CurAndPrev (EvalResults (ValI m)) -> Anchors.CodeAnchors m ->
    EntityId -> T m [Pane InternalName (T m) (T m) [EntityId]]
loadPanes monitors evalRes cp replEntityId =
    do
        Property panes setPanes <- Anchors.panes cp ^. Property.mkProperty
        paneDefs <- mapM (loadAnnotatedDef Anchors.paneDef) panes
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
                        & convertDefBody monitors evalRes cp
                        <&> Lens.mapped . Lens.mapped %~ (^. pUserData)
                    let defI = def ^. Definition.defPayload & Anchors.paneDef
                    let defVar = ExprIRef.globalId defI
                    tag <- Anchors.tags cp & convertTaggedEntityWith defVar
                    defS <-
                        PresentationModes.addToDef Definition
                        { _drEntityId = EntityId.ofIRef defI
                        , _drName = tag
                        , _drBody = bodyS
                        , _drDefinitionState =
                            Anchors.assocDefinitionState defI ^. Property.mkProperty
                        , _drDefI = defVar
                        }
                        >>= OrderTags.orderDef
                    pure Pane
                        { _paneDefinition = defS
                        , _paneClose = mkDelPane i
                        , _paneMoveDown = mkMMovePaneDown i
                        , _paneMoveUp = mkMMovePaneUp i
                        }
        paneDefs & Lens.itraversed %%@~ convertPane

loadWorkArea ::
    (HasCallStack, Monad m) =>
    Debug.Monitors -> CurAndPrev (EvalResults (ValI m)) -> Anchors.CodeAnchors m ->
    T m (WorkArea InternalName (T m) (T m) [EntityId])
loadWorkArea monitors evalRes cp =
    do
        repl <- loadRepl monitors evalRes cp
        panes <- loadPanes monitors evalRes cp (repl ^. replExpr . rPayload . plEntityId)
        pure WorkArea
            { _waPanes = panes
            , _waRepl = repl
            , _waGlobals =
                Anchors.globals cp & Property.getP <&> Set.toList
                >>= traverse (ConvertGetVar.globalNameRef cp)
            }
