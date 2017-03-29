{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Sugar.Convert
    ( loadWorkArea
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import           Data.CurAndPrev (CurAndPrev)
import qualified Data.Graph as Graph
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Store.Property (Property(..))
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import           Data.UUID.Types (UUID)
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Calc.Type.Nominal as N
import           Lamdu.Calc.Type.Scheme (Scheme, schemeType)
import           Lamdu.Calc.Val.Annotated (Val(..))
import qualified Lamdu.Calc.Val.Annotated as Val
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import           Lamdu.Eval.Results (EvalResults)
import qualified Lamdu.Eval.Results as Results
import           Lamdu.Expr.IRef (DefI, ValI, ValIProperty)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.IRef.Infer as IRefInfer
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.Load as ExprLoad
import qualified Lamdu.Expr.UniqueId as UniqueId
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Sugar.Convert.DefExpr as ConvertDefExpr
import qualified Lamdu.Sugar.Convert.DefExpr.OutdatedDefs as OutdatedDefs
import qualified Lamdu.Sugar.Convert.Expression as ConvertExpr
import qualified Lamdu.Sugar.Convert.Input as Input
import qualified Lamdu.Sugar.Convert.Load as Load
import           Lamdu.Sugar.Convert.Monad (Context(..), ScopeInfo(..), OuterScopeInfo(..))
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import qualified Lamdu.Sugar.OrderTags as OrderTags
import qualified Lamdu.Sugar.PresentationModes as PresentationModes
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

type T = Transaction

convertDefIBuiltin ::
    Monad m => Scheme -> Definition.FFIName -> DefI m ->
    DefinitionBody UUID m (ExpressionU m [EntityId])
convertDefIBuiltin scheme name defI =
    DefinitionBodyBuiltin DefinitionBuiltin
    { _biName = name
    , _biSetName = setName
    , _biType = scheme
    }
    where
        setName newName =
            Transaction.writeIRef defI
            Definition.Definition
            { Definition._defBody = Definition.BodyBuiltin newName
            , Definition._defType = scheme
            , Definition._defPayload = ()
            }

postProcessDef :: Monad m => DefI m -> T m ConvertM.PostProcessResult
postProcessDef defI =
    do
        def <- Transaction.readIRef defI
        case def ^. Definition.defBody of
            Definition.BodyBuiltin {} -> return ConvertM.GoodExpr
            Definition.BodyExpr defExpr ->
                do
                    loaded <- traverse Load.readValAndAddProperties defExpr
                    inferRes <- Load.inferDef (pure Results.empty) loaded (ExprIRef.globalId defI)
                    case inferRes of
                        Left err -> ConvertM.BadExpr err & return
                        Right (inferredVal, inferContext) ->
                            do
                                def
                                    & Definition.defType .~
                                        Infer.makeScheme inferContext inferredType
                                    & Definition.defBody . Definition._BodyExpr .
                                        Definition.exprFrozenDeps .~
                                        Definition.pruneDefExprDeps loaded
                                    & Transaction.writeIRef defI
                                return ConvertM.GoodExpr
                            where
                                inferredType = inferredVal ^. Val.payload . Input.inferredType

postProcessExpr ::
    Monad m =>
    Transaction.MkProperty m (Definition.Expr (ValI m)) ->
    T m ConvertM.PostProcessResult
postProcessExpr mkProp =
    do
        prop <- mkProp ^. Transaction.mkProperty
        defExpr <- traverse Load.readValAndAddProperties (prop ^. Property.pVal)
        inferred <-
            Load.inferDefExpr Infer.emptyScope defExpr
            & IRefInfer.liftInfer
            >>= Load.loadInferPrepareInput (pure Results.empty)
            & IRefInfer.run
        case inferred of
            Left err -> ConvertM.BadExpr err & return
            Right _ ->
                do
                    Definition.exprFrozenDeps .~
                        Definition.pruneDefExprDeps defExpr
                        & Property.pureModify prop
                    return ConvertM.GoodExpr

emptyScopeInfo :: ScopeInfo m
emptyScopeInfo =
    ScopeInfo
      { _siTagParamInfos = mempty
      , _siNullParams = mempty
      , _siLetItems = mempty
      , _siOuter = OuterScopeInfo
        { _osiPos = Nothing
        , _osiVarsUnderPos = []
        }
      }

makeNominalsMap ::
    Monad m => Val (Input.Payload m a) -> T m (Map T.NominalId N.Nominal)
makeNominalsMap val =
    mapM_ loadForType (val ^.. Lens.traverse . Input.inferred . Infer.plType)
    & (`State.execStateT` mempty)
    where
        loadForType typ = typ ^.. ExprLens.typeTIds & mapM_ loadForTid
        loadForTid tid =
            do
                loaded <- State.get
                unless (Map.member tid loaded) $
                    do
                        nom <- ExprLoad.nominal tid & lift
                        Map.insert tid nom loaded & State.put
                        nom ^.. N.nomType . N._NominalType . schemeType & traverse_ loadForType

nonRepeating :: Ord a => [a] -> [a]
nonRepeating = concat . filter (null . tail) . List.group . List.sort

convertInferDefExpr ::
    Monad m =>
    CurAndPrev (EvalResults (ValI m)) -> Anchors.CodeProps m ->
    Scheme -> Definition.Expr (Val (ValIProperty m)) -> DefI m ->
    T m (DefinitionBody UUID m (ExpressionU m [EntityId]))
convertInferDefExpr evalRes cp defType defExpr defI =
    do
        (valInferred, newInferContext) <-
            Load.inferDef evalRes defExpr defVar <&> Load.assertInferSuccess
        nomsMap <- makeNominalsMap valInferred
        outdatedDefinitions <- OutdatedDefs.scan defExpr setDefExpr
        let context =
                Context
                { _scInferContext = newInferContext
                , _scNominalsMap = nomsMap
                , _scGlobalsInScope = Set.singleton defI
                , _scCodeAnchors = cp
                , _scScopeInfo = emptyScopeInfo
                , _scPostProcessRoot = postProcessDef defI
                , _scOutdatedDefinitions = outdatedDefinitions
                , _scInlineableDefinitions =
                    valInferred ^.. ExprLens.valGlobals (Set.singleton defVar)
                    & nonRepeating & Set.fromList
                , _scFrozenDeps =
                    Property (defExpr ^. Definition.exprFrozenDeps) setFrozenDeps
                , scConvertSubexpression = ConvertExpr.convert
                }
        ConvertDefExpr.convert
            defType (defExpr & Definition.expr .~ valInferred) defI
            & ConvertM.run context
    where
        defVar = ExprIRef.globalId defI
        setDefExpr x =
            Definition.Definition (Definition.BodyExpr x) defType ()
            & Transaction.writeIRef defI
        setFrozenDeps deps =
            Transaction.readIRef defI
            <&> Definition.defBody . Definition._BodyExpr . Definition.exprFrozenDeps .~ deps
            >>= Transaction.writeIRef defI

convertDefBody ::
    Monad m =>
    CurAndPrev (EvalResults (ValI m)) -> Anchors.CodeProps m ->
    Definition.Definition (Val (ValIProperty m)) (DefI m) ->
    T m (DefinitionBody UUID m (ExpressionU m [EntityId]))
convertDefBody evalRes cp (Definition.Definition body defType defI) =
    case body of
    Definition.BodyExpr defExpr -> convertInferDefExpr evalRes cp defType defExpr defI
    Definition.BodyBuiltin builtin -> convertDefIBuiltin defType builtin defI & return

convertExpr ::
    Monad m =>
    CurAndPrev (EvalResults (ValI m)) -> Anchors.CodeProps m ->
    Transaction.MkProperty m (Definition.Expr (ValI m)) ->
    T m (ExpressionU m [EntityId])
convertExpr evalRes cp prop =
    do
        defExpr <- ExprLoad.defExprProperty prop
        (valInferred, newInferContext) <-
            Load.inferDefExpr Infer.emptyScope defExpr
            & IRefInfer.liftInfer
            >>= Load.loadInferPrepareInput evalRes
            & IRefInfer.run
            <&> Load.assertInferSuccess
        nomsMap <- makeNominalsMap valInferred
        outdatedDefinitions <- OutdatedDefs.scan defExpr (Transaction.setP prop)
        let context =
                Context
                { _scInferContext = newInferContext
                , _scNominalsMap = nomsMap
                , _scGlobalsInScope = Set.empty
                , _scCodeAnchors = cp
                , _scScopeInfo = emptyScopeInfo
                , _scPostProcessRoot = postProcessExpr prop
                , _scOutdatedDefinitions = outdatedDefinitions
                , _scInlineableDefinitions =
                    valInferred ^.. ExprLens.valGlobals mempty & nonRepeating & Set.fromList
                , _scFrozenDeps =
                    Property (defExpr ^. Definition.exprFrozenDeps) setFrozenDeps
                , scConvertSubexpression = ConvertExpr.convert
                }
        ConvertM.convertSubexpression valInferred & ConvertM.run context
    where
        setFrozenDeps deps =
            prop ^. Transaction.mkProperty
            >>= (`Property.pureModify` (Definition.exprFrozenDeps .~ deps))

loadRepl ::
    Monad m =>
    CurAndPrev (EvalResults (ValI m)) -> Anchors.CodeProps m ->
    T m (Expression UUID m [EntityId])
loadRepl evalRes cp =
    convertExpr evalRes cp (Anchors.repl cp)
    >>= OrderTags.orderExpr
    >>= PresentationModes.addToExpr

-- | Returns the list of definition-sets (topographically-sorted by usages)
-- This allows us to choose type inference order with maximal generality
stronglyConnectedDefs ::
    (pl -> DefI m) ->
    [Definition.Definition (Val a) pl] ->
    [[Definition.Definition (Val a) pl]]
stronglyConnectedDefs getDefI defs =
    defs <&> node & Graph.stronglyConnComp <&> Graph.flattenSCC
    where
        node def =
            ( def
            , def ^. Definition.defPayload & getDefI & ExprIRef.globalId
            , def ^.. Definition.defBody . Lens.traverse . ExprLens.valGlobals mempty
            )

loadAnnotatedDef ::
    Monad m =>
    (pl -> DefI m) ->
    pl -> T m (Definition.Definition (Val (ValIProperty m)) pl)
loadAnnotatedDef getDefI annotation =
    getDefI annotation & ExprLoad.def <&> Definition.defPayload .~ annotation

loadPanes ::
    Monad m =>
    CurAndPrev (EvalResults (ValI m)) -> Anchors.CodeProps m -> EntityId ->
    T m [Pane UUID m [EntityId]]
loadPanes evalRes cp replEntityId =
    do
        Property panes setPanes <- Anchors.panes cp ^. Transaction.mkProperty
        ordered <-
            Set.toList panes & mapM (loadAnnotatedDef Anchors.paneDef)
            <&> stronglyConnectedDefs Anchors.paneDef <&> concat <&> reverse
        let convertPane i def =
                do
                    bodyS <- def <&> Anchors.paneDef & convertDefBody evalRes cp
                    let defI = def ^. Definition.defPayload & Anchors.paneDef
                    defS <-
                        OrderTags.orderDef Definition
                        { _drEntityId = EntityId.ofIRef defI
                        , _drName = UniqueId.toUUID defI
                        , _drBody = bodyS
                        , _drDefinitionState = Anchors.assocDefinitionState defI
                        , _drDefI = defI
                        }
                        >>= PresentationModes.addToDef
                    return Pane
                        { _paneDefinition = defS
                        , _paneClose =
                          do
                              Set.delete (def ^. Definition.defPayload) panes
                                  & setPanes
                              ordered ^? Lens.ix (i-1) . Definition.defPayload
                                  & maybe replEntityId (EntityId.ofIRef . Anchors.paneDef)
                                  & return
                        }
        ordered & Lens.itraversed %%@~ convertPane

loadWorkArea ::
    Monad m => CurAndPrev (EvalResults (ValI m)) -> Anchors.CodeProps m ->
    T m (WorkArea UUID m [EntityId])
loadWorkArea evalRes cp =
    do
        repl <- loadRepl evalRes cp
        panes <- loadPanes evalRes cp (repl ^. rPayload . plEntityId)
        WorkArea panes repl & return
