module Lamdu.Sugar.Convert
    ( loadWorkArea, InternalName
    ) where

import           Control.Applicative ((<|>))
import qualified Control.Lens as Lens
import           Control.Monad.Once (OnceT, Typeable)
import           Data.List.Extended (insertAt, removeAt)
import           Data.Property (Property(Property))
import qualified Data.Property as Property
import qualified Lamdu.Cache as Cache
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Tag as Tag
import qualified Lamdu.Debug as Debug
import qualified Lamdu.Expr.IRef as ExprIRef
import           Lamdu.Sugar.Config (Config)
import qualified Lamdu.Sugar.Convert.Definition as ConvertDefinition
import qualified Lamdu.Sugar.Convert.NameRef as ConvertNameRef
import qualified Lamdu.Sugar.Convert.Nominal as ConvertNominal
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.OrderTags (orderWorkArea)
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction

import           Lamdu.Prelude

type T = Transaction

convertPaneBody ::
    ( Monad m, Typeable m
    , Has Debug.Monitors env
    , Has Config env, Has Cache.Functions env
    , Anchors.HasCodeAnchors env m
    ) =>
    env -> Anchors.Pane m ->
    OnceT (T m)
    (PaneBody EvalPrep InternalName (OnceT (T m)) (T m) (ConvertPayload m [EntityId]))
convertPaneBody _ (Anchors.PaneTag tagId) =
    ExprIRef.readTagData tagId & lift <&>
    \tagData ->
    PaneTag TagPane
    { _tpTag = tagId
    , _tpTagData = tagData
    , _tpSetSymbol = \sym -> tagData & Tag.tagSymbol .~ sym & writeTag
    , _tpSetOrder = \order -> tagData & Tag.tagOrder .~ order & writeTag
    , _tpSetTexts =
            \langId text -> tagData & Tag.tagTexts . Lens.at langId ?~ text & writeTag
    }
    where
        writeTag = Transaction.writeIRef (ExprIRef.tagI tagId)
convertPaneBody env (Anchors.PaneDefinition defI) = ConvertDefinition.pane env defI
convertPaneBody env (Anchors.PaneNominal nomId) = ConvertNominal.pane env nomId

mkPaneEntityId :: Anchors.Pane dummy -> EntityId
mkPaneEntityId (Anchors.PaneDefinition defI) = EntityId.ofIRef defI
mkPaneEntityId (Anchors.PaneNominal tid) = EntityId.ofNominalPane tid
mkPaneEntityId (Anchors.PaneTag tag) = EntityId.ofTagPane tag

convertPane ::
    ( Monad m, Typeable m
    , Has Debug.Monitors env
    , Has Config env, Has Cache.Functions env
    , Anchors.HasCodeAnchors env m
    ) =>
    env -> EntityId ->
    Property (T m) [Anchors.Pane dummy] ->
    Int -> Anchors.Pane m ->
    OnceT (T m)
    (Pane EvalPrep InternalName (OnceT (T m)) (T m) (ConvertPayload m [EntityId]))
convertPane env replEntityId (Property panes setPanes) i pane =
    do
        body <- convertPaneBody env pane
        defState <- Anchors.assocDefinitionState myEntityId ^. Property.mkProperty & lift
        pure Pane
            { _paneBody = body
            , _paneEntityId = myEntityId
            , _paneDefinitionState = defState
            , _paneClose = mkDelPane
            , _paneMoveDown = mkMMovePaneDown
            , _paneMoveUp = mkMMovePaneUp
            }
    where
        myEntityId = mkPaneEntityId pane
        mkDelPane =
            entityId <$ setPanes newPanes
            where
                entityId =
                    newPanes ^? Lens.ix i
                    <|> newPanes ^? Lens.ix (i-1)
                    <&> mkPaneEntityId
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
    , Anchors.HasCodeAnchors env m
    ) =>
    env -> EntityId ->
    OnceT (T m) [Pane EvalPrep InternalName (OnceT (T m)) (T m) (ConvertPayload m [EntityId])]
loadPanes env replEntityId =
    do
        prop <- Anchors.panes (env ^. Anchors.codeAnchors) ^. Property.mkProperty & lift
        Property.value prop
            & Lens.itraversed %%@~ convertPane env replEntityId prop

globals ::
    Monad m =>
    Anchors.Code (Property.MkProperty (T m) (T m)) m -> Globals InternalName (OnceT (T m)) (T m)
globals cp =
    Globals
    { _globalDefs = globalNameRefs Anchors.globals ConvertNameRef.makeForDefinition
    , _globalNominals = globalNameRefs Anchors.tids ConvertNameRef.makeForNominal
    , _globalTags = globalNameRefs Anchors.tags ConvertNameRef.makeForTag
    }
    where
        globalNameRefs globs makeNameRef =
            Property.getP (globs cp) & lift <&> (^.. Lens.folded) >>= traverse (makeNameRef cp)

loadWorkArea ::
    ( HasCallStack, Monad m, Typeable m
    , Has Debug.Monitors env
    , Has Config env, Has Cache.Functions env
    , Anchors.HasCodeAnchors env m
    ) =>
    env ->
    OnceT (T m) (WorkArea EvalPrep InternalName (OnceT (T m)) (T m) (ConvertPayload m [EntityId]))
loadWorkArea env =
    do
        repl <- ConvertDefinition.repl env
        panes <-
            repl ^. replExpr . SugarLens.binderResultExpr . pEntityId
            & loadPanes env
        orderWorkArea WorkArea
            { _waRepl = repl
            , _waPanes = panes
            , _waGlobals = globals cp
            }
    where
        cp = env ^. Anchors.codeAnchors
