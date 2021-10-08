module Lamdu.Sugar.Convert.NameRef
    ( makeForDefinition, jumpToDefinition
    ) where

import           Control.Monad.Transaction (MonadTransaction)
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import           Lamdu.Expr.IRef (DefI)
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

jumpToDefinition ::
    Monad m => Anchors.CodeAnchors m -> DefI m -> T m EntityId
jumpToDefinition cp defI =
    EntityId.ofIRef defI <$ DataOps.newPane cp (Anchors.PaneDefinition defI)

makeForDefinition ::
    (MonadTransaction n m, Monad f) =>
    Anchors.CodeAnchors f -> DefI f -> m (NameRef InternalName (T f))
makeForDefinition cp defI =
    taggedName Nothing defI <&>
    \name ->
    NameRef
    { _nrName = name
    , _nrGotoDefinition = jumpToDefinition cp defI
    }

