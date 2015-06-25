module Lamdu.Sugar.Convert.TIdG
    ( convert
    ) where

import           Data.Store.Guid (Guid)
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.UniqueId as UniqueId
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

convert :: T.Id -> TIdG Guid
convert tid =
    TIdG
    { _tidgName = UniqueId.toGuid tid
    , _tidgEntityId = EntityId.ofTId tid
    , _tidgTId = tid
    }
