module Lamdu.Sugar.Convert.TId
    ( convert
    ) where

import           Control.Monad.Transaction (MonadTransaction)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Sugar.Internal
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

convert :: MonadTransaction n m => T.NominalId -> m (TId InternalName)
convert tid = taggedName tid <&> (`TId` tid)
