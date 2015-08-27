module Lamdu.Sugar.Convert.TIdG
    ( convert
    ) where

import           Data.Store.Guid (Guid)
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.UniqueId as UniqueId
import           Lamdu.Sugar.Types

convert :: T.NominalId -> TIdG Guid
convert tid =
    TIdG
    { _tidgName = UniqueId.toGuid tid
    , _tidgTId = tid
    }
