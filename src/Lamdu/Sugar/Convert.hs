module Lamdu.Sugar.Convert
    ( loadWorkArea, InternalName
    ) where

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
    ( Monad m, Typeable m
    , Has Debug.Monitors env
    , Has Config env, Has Cache.Functions env
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
    ( Monad m, Typeable m
    , Has Debug.Monitors env
    , Has Config env, Has Cache.Functions env
    , Anchors.HasCodeAnchors env m
    ) =>
    env ->
    OnceT (T m) [Pane EvalPrep InternalName (OnceT (T m)) (T m) ([EntityId], ConvertPayload m)]
loadPanes env =
    do
        prop <- Anchors.panes (env ^. Anchors.codeAnchors) ^. Property.mkProperty & lift
        Property.value prop & Lens.itraversed %%@~ convertPane env prop

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
    ( Monad m, Typeable m
    , Has Debug.Monitors env
    , Has Config env, Has Cache.Functions env
    , Anchors.HasCodeAnchors env m
    ) =>
    env ->
    OnceT (T m) (WorkArea EvalPrep InternalName (OnceT (T m)) (T m) ([EntityId], ConvertPayload m))
loadWorkArea env =
    loadPanes env <&> (`WorkArea` globals cp)
    >>= orderWorkArea
    where
        cp = env ^. Anchors.codeAnchors
