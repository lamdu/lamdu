module Lamdu.Sugar.Convert
    ( loadWorkArea, InternalName
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Once (OnceT, Typeable)
import           Data.List.Extended (insertAt, removeAt)
import           Data.Property (Property(..), MkProperty)
import qualified Data.Property as Property
import qualified Lamdu.Cache as Cache
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Data.Tag as Tag
import qualified Lamdu.Debug as Debug
import qualified Lamdu.Expr.IRef as ExprIRef
import           Lamdu.Sugar.Config (Sugars)
import qualified Lamdu.Sugar.Convert.Definition as ConvertDefinition
import qualified Lamdu.Sugar.Convert.NameRef as ConvertNameRef
import qualified Lamdu.Sugar.Convert.Nominal as ConvertNominal
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.OrderTags (orderWorkArea)
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction

import           Lamdu.Prelude

type T = Transaction

convertPaneBody ::
    ( HasCallStack, Monad m, Typeable m
    , Has Debug.Monitors env
    , Has (Sugars Bool) env, Has Cache.Functions env
    , Anchors.HasCodeAnchors env m
    ) =>
    env -> Anchors.Pane m ->
    OnceT (T m)
    (PaneBody EvalPrep InternalName (OnceT (T m)) (T m) ([EntityId], ConvertPayload m))
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
    ( HasCallStack, Monad m, Typeable m
    , Has Debug.Monitors env
    , Has (Sugars Bool) env, Has Cache.Functions env
    , Anchors.HasCodeAnchors env m
    ) =>
    env ->
    Property (T m) [Anchors.Pane dummy] ->
    Int -> Anchors.Pane m ->
    OnceT (T m)
    (Pane EvalPrep InternalName (OnceT (T m)) (T m) ([EntityId], ConvertPayload m))
convertPane env (Property panes setPanes) i pane =
    do
        body <- convertPaneBody env pane
        defState <-
            Anchors.assocDefinitionState myEntityId ^. Property.mkProperty & lift
            <&> addRemoveOnDelete (env ^. Anchors.codeAnchors) pane
        pure Pane
            { _paneBody = body
            , _paneEntityId = myEntityId
            , _paneDefinitionState = defState
            , _paneClose = setPanes (removeAt i panes)
            , _paneMoveDown = mkMMovePaneDown
            , _paneMoveUp = mkMMovePaneUp
            }
    where
        myEntityId = mkPaneEntityId pane
        movePane oldIndex newIndex =
            case splitAt oldIndex panes of
            (before, item:after) ->
                insertAt newIndex item (before ++ after)
            _ -> error (show oldIndex <> " out of bounds for " <> show panes)
            & setPanes
        mkMMovePaneDown
            | i+1 < length panes = Just $ movePane i (i+1)
            | otherwise = Nothing
        mkMMovePaneUp
            | i-1 >= 0 = Just $ movePane i (i-1)
            | otherwise = Nothing

addRemoveOnDelete ::
    Monad m =>
    Anchors.CodeAnchors m -> Anchors.Pane m -> Property (T m) DefinitionState -> Property (T m) DefinitionState
addRemoveOnDelete anchors pane =
    Property.pSet . Lens.imapped %@~
    \newState -> (fix newState *>)
    where
        fix newState =
            case pane of
            Anchors.PaneDefinition x -> f Anchors.globals x
            Anchors.PaneNominal x -> f Anchors.tids x
            Anchors.PaneTag x -> f Anchors.tags x
            where
                f w x = Property.modP (w anchors) (Lens.contains x .~ (newState == LiveDefinition))

loadPanes ::
    ( HasCallStack, Monad m, Typeable m
    , Has Debug.Monitors env
    , Has (Sugars Bool) env, Has Cache.Functions env
    , Anchors.HasCodeAnchors env m
    ) =>
    env ->
    OnceT (T m) [Pane EvalPrep InternalName (OnceT (T m)) (T m) ([EntityId], ConvertPayload m)]
loadPanes env =
    do
        prop <- Anchors.panes (env ^. Anchors.codeAnchors) ^. Property.mkProperty & lift
        Property.value prop & Lens.itraversed %%@~ convertPane env prop

globalNameRefs ::
    Monad m =>
    Anchors.Code (MkProperty (T m) (T m)) m ->
    (Anchors.Code (MkProperty (T m) (T m)) m -> MkProperty (T m) (T m) (Set a)) ->
    (a -> OnceT (T m) InternalName) ->
    OnceT (T m) [NameRef InternalName a]
globalNameRefs cp globs makeNameRef =
    Property.getP (globs cp) & lift <&> (^.. Lens.folded)
    >>= traverse (\x -> makeNameRef x <&> (`NameRef` x))

globals ::
    Monad m =>
    Anchors.Code (Property.MkProperty (T m) (T m)) m -> Globals InternalName (OnceT (T m))
globals cp =
    Globals
    { _globalDefs =
        globalNameRefs cp Anchors.globals ConvertNameRef.makeForDefinition
        <&> Lens.mapped . Lens.mapped %~ ExprIRef.globalId
    , _globalNominals = globalNameRefs cp Anchors.tids ConvertNameRef.makeForNominal
    , _globalTags = globalNameRefs cp Anchors.tags (pure . ConvertNameRef.makeForTag)
    }

loadWorkArea ::
    ( HasCallStack, Monad m, Typeable m
    , Has Debug.Monitors env
    , Has (Sugars Bool) env, Has Cache.Functions env
    , Anchors.HasCodeAnchors env m
    ) =>
    env ->
    OnceT (T m) (WorkArea EvalPrep InternalName (OnceT (T m)) (T m) ([EntityId], ConvertPayload m))
loadWorkArea env =
    do
        p <- loadPanes env
        WorkArea p (globals cp) goto & orderWorkArea
    where
        cp = env ^. Anchors.codeAnchors
        goto (GoToDef x) = EntityId.ofBinder x <$ DataOps.newPane cp (Anchors.PaneDefinition (ExprIRef.defI x))
        goto (GoToNom x) = EntityId.ofNominalPane x <$ DataOps.newPane cp (Anchors.PaneNominal x)
        goto (GoToTag x) = EntityId.ofTagPane x <$ DataOps.newPane cp (Anchors.PaneTag x)
