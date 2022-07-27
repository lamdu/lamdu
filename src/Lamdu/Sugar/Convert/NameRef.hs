module Lamdu.Sugar.Convert.NameRef
    ( jumpToDefinition
    , jumpToNominal
    , jumpToTag
    , makeForDefinition
    , makeForNominal
    , makeForTag
    ) where

import           Control.Monad.Transaction (MonadTransaction)
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import           Lamdu.Expr.IRef (DefI)
import           Lamdu.Expr.UniqueId (ToUUID)
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

jumpInternal ::
    Monad m =>
    (ident -> entityId) ->
    (ident -> Anchors.Pane m) ->
    Anchors.CodeAnchors m -> ident ->
    T m entityId
jumpInternal entityId pane cp ident = entityId ident <$ DataOps.newPane cp (pane ident)

jumpToDefinition :: Monad m => Anchors.CodeAnchors m -> DefI m -> T m EntityId
jumpToDefinition = jumpInternal EntityId.ofIRef Anchors.PaneDefinition

jumpToNominal :: Monad m => Anchors.CodeAnchors m -> T.NominalId -> T m EntityId
jumpToNominal = jumpInternal EntityId.ofNominalPane Anchors.PaneNominal

jumpToTag :: Monad m => Anchors.CodeAnchors m -> T.Tag -> T m EntityId
jumpToTag = jumpInternal EntityId.ofTagPane Anchors.PaneTag

makeInternal :: (MonadTransaction n m, ToUUID global) => global -> m InternalName
makeInternal = taggedName Nothing

makeForDefinition :: MonadTransaction n m => DefI f -> m InternalName
makeForDefinition = makeInternal

makeForNominal :: MonadTransaction n m => T.NominalId -> m InternalName
makeForNominal = makeInternal

makeForTag :: T.Tag -> InternalName
makeForTag = nameWithoutContext
