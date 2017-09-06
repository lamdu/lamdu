module Lamdu.Sugar.Convert.TId
    ( convert
    ) where

import           Data.UUID.Types (UUID)
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Expr.UniqueId as UniqueId
import           Lamdu.Sugar.Types

convert :: T.NominalId -> TId UUID
convert tid =
    TId
    { _tidName = UniqueId.toUUID tid
    , _tidTId = tid
    }
