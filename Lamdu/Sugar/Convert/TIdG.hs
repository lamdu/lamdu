module Lamdu.Sugar.Convert.TIdG
    ( convert
    ) where

import           Data.UUID.Types (UUID)
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Expr.UniqueId as UniqueId
import           Lamdu.Sugar.Types

convert :: T.NominalId -> TIdG UUID
convert tid =
    TIdG
    { _tidgName = UniqueId.toUUID tid
    , _tidgTId = tid
    }
