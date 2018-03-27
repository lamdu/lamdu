module Lamdu.Sugar.Convert.TId
    ( convert
    ) where

import           Control.Monad.Transaction (MonadTransaction, getP)
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Sugar.Internal
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

convert :: MonadTransaction n m => T.NominalId -> m (TId InternalName)
convert tid =
    Anchors.assocTag tid & getP
    <&> \tag ->
    TId
    { _tidName = nameWithContext tid tag
    , _tidTId = tid
    }
