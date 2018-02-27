{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Sugar.Convert
    ( loadWorkArea
    ) where

import           Control.Applicative ((<|>))
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import           Data.CurAndPrev (CurAndPrev)
import qualified Data.List as List
import           Data.List.Utils (insertAt, removeAt)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Calc.Type.Nominal as N
import           Lamdu.Calc.Type.Scheme (Scheme, schemeType)
import           Lamdu.Calc.Val.Annotated (Val(..))
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import           Lamdu.Eval.Results (EvalResults)
import           Lamdu.Expr.IRef (DefI, ValI, ValIProperty)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.Load as ExprLoad
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Sugar.Convert.DefExpr as ConvertDefExpr
import qualified Lamdu.Sugar.Convert.DefExpr.OutdatedDefs as OutdatedDefs
import qualified Lamdu.Sugar.Convert.Expression as ConvertExpr
import qualified Lamdu.Sugar.Convert.Input as Input
import qualified Lamdu.Sugar.Convert.Load as Load
import           Lamdu.Sugar.Convert.Monad (Context(..), ScopeInfo(..), RecursiveRef(..))
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Convert.PostProcess (postProcessDef, postProcessExpr)
import           Lamdu.Sugar.Convert.Tag (convertTaggedEntityWith)
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import qualified Lamdu.Sugar.OrderTags as OrderTags
import qualified Lamdu.Sugar.PresentationModes as PresentationModes
import           Lamdu.Sugar.Types
import           Revision.Deltum.Property (Property(..))
import qualified Revision.Deltum.Property as Property
import           Revision.Deltum.Transaction (Transaction, mkProperty)
import qualified Revision.Deltum.Transaction as Transaction

import           Lamdu.Prelude

type T = Transaction

convertDefIBuiltin ::
    Monad m => Scheme -> Definition.FFIName -> DefI m ->
    DefinitionBody InternalName (T m) (ExpressionU m [EntityId])
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

emptyScopeInfo :: Maybe (RecursiveRef m) -> ScopeInfo m
emptyScopeInfo recursiveRef =
    ScopeInfo
    { _siTagParamInfos = mempty
    , _siNullParams = mempty
    , _siLetItems = mempty
    , _siMOuter = Nothing
    , _siRecursiveRef = recursiveRef
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
    CurAndPrev (EvalResults (ValI m)) -> Anchors.CodeAnchors m ->
    Scheme -> Definition.Expr (Val (ValIProperty m)) -> DefI m ->
    T m (DefinitionBody InternalName (T m) (ExpressionU m [EntityId]))
convertInferDefExpr evalRes cp defType defExpr defI =
    do
        (valInferred, newInferContext) <-
            Load.inferDef evalRes defExpr defVar <&> Load.assertInferSuccess
        nomsMap <- makeNominalsMap valInferred
        outdatedDefinitions <-
            OutdatedDefs.scan defExpr setDefExpr (postProcessDef defI)
            <&> Lens.mapped . defTypeUseCurrent %~ (<* postProcessDef defI)
        let context =
                Context
                { _scInferContext = newInferContext
                , _scNominalsMap = nomsMap
                , _scCodeAnchors = cp
                , _scScopeInfo =
                        emptyScopeInfo
                        ( Just RecursiveRef
                          { _rrDefI = defI
                          , _rrDefType = defType
                          }
                        )
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
    CurAndPrev (EvalResults (ValI m)) -> Anchors.CodeAnchors m ->
    Definition.Definition (Val (ValIProperty m)) (DefI m) ->
    T m (DefinitionBody InternalName (T m) (ExpressionU m [EntityId]))
convertDefBody evalRes cp (Definition.Definition body defType defI) =
    case body of
    Definition.BodyExpr defExpr -> convertInferDefExpr evalRes cp defType defExpr defI
    Definition.BodyBuiltin builtin -> convertDefIBuiltin defType builtin defI & pure

convertExpr ::
    Monad m =>
    CurAndPrev (EvalResults (ValI m)) -> Anchors.CodeAnchors m ->
    Transaction.MkProperty m (Definition.Expr (ValI m)) ->
    T m (ExpressionU m [EntityId])
convertExpr evalRes cp prop =
    do
        defExpr <- ExprLoad.defExprProperty prop
        (valInferred, newInferContext) <-
            Load.inferDefExpr evalRes defExpr <&> Load.assertInferSuccess
        nomsMap <- makeNominalsMap valInferred
        outdatedDefinitions <- OutdatedDefs.scan defExpr (Transaction.setP prop) (postProcessExpr prop)
        let context =
                Context
                { _scInferContext = newInferContext
                , _scNominalsMap = nomsMap
                , _scCodeAnchors = cp
                , _scScopeInfo = emptyScopeInfo Nothing
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
    CurAndPrev (EvalResults (ValI m)) -> Anchors.CodeAnchors m ->
    T m (Expression InternalName (T m) [EntityId])
loadRepl evalRes cp =
    convertExpr evalRes cp (Anchors.repl cp)
    <&> Lens.mapped %~ (^. pUserData)
    >>= PresentationModes.addToExpr
    >>= OrderTags.orderExpr

loadAnnotatedDef ::
    Monad m =>
    (pl -> DefI m) ->
    pl -> T m (Definition.Definition (Val (ValIProperty m)) pl)
loadAnnotatedDef getDefI annotation =
    getDefI annotation & ExprLoad.def <&> Definition.defPayload .~ annotation

loadPanes ::
    Monad m =>
    CurAndPrev (EvalResults (ValI m)) -> Anchors.CodeAnchors m -> EntityId ->
    T m [Pane InternalName (T m) [EntityId]]
loadPanes evalRes cp replEntityId =
    do
        Property panes setPanes <- Anchors.panes cp ^. Transaction.mkProperty
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
                        & convertDefBody evalRes cp
                        <&> Lens.mapped . Lens.mapped %~ (^. pUserData)
                    let defI = def ^. Definition.defPayload & Anchors.paneDef
                    let defVar = ExprIRef.globalId defI
                    tag <-
                        Anchors.tags cp
                        & Transaction.getP
                        & convertTaggedEntityWith defVar
                    defS <-
                        PresentationModes.addToDef Definition
                        { _drEntityId = EntityId.ofIRef defI
                        , _drName = tag
                        , _drBody = bodyS
                        , _drDefinitionState =
                            Anchors.assocDefinitionState defI ^. mkProperty
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
    Monad m => CurAndPrev (EvalResults (ValI m)) -> Anchors.CodeAnchors m ->
    T m (WorkArea InternalName (T m) [EntityId])
loadWorkArea evalRes cp =
    do
        repl <- loadRepl evalRes cp
        panes <- loadPanes evalRes cp (repl ^. rPayload . plEntityId)
        WorkArea panes repl & pure
