module Lamdu.Sugar.Convert.TId
    ( convert
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Transaction (Transaction, MonadTransaction)
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Sugar.Convert.NameRef (jumpToNominal)
import           Lamdu.Sugar.Internal
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

type T = Transaction

convert ::
    (MonadTransaction n m, MonadReader env m, Anchors.HasCodeAnchors env n) =>
    T.NominalId -> m (TId InternalName (T n))
convert tid =
    TId
    <$> taggedName Nothing tid
    <*> pure tid
    <*> (Lens.view Anchors.codeAnchors <&> (`jumpToNominal` tid))
