module Lamdu.Sugar.Convert.TId
    ( convert
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Transaction (Transaction, MonadTransaction)
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

type T = Transaction

gotoToNominalDef ::
    Monad m => Anchors.CodeAnchors m -> T.NominalId -> T m EntityId
gotoToNominalDef cp tid =
    EntityId.ofNominalPane tid <$ DataOps.newPane cp (Anchors.PaneNominal tid)

convert ::
    (MonadTransaction n m, MonadReader env m, Anchors.HasCodeAnchors env n) =>
    T.NominalId -> m (TId InternalName (T n))
convert tid =
    TId
    <$> taggedName Nothing tid
    <*> pure tid
    <*> (Lens.view Anchors.codeAnchors <&> (`gotoToNominalDef` tid))
