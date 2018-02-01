{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Sugar.Convert.TId
    ( convert
    ) where

import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Expr.UniqueId as UniqueId
import           Lamdu.Sugar.Internal
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

convert :: T.NominalId -> TId InternalName
convert tid =
    TId
    { _tidName = UniqueId.toUUID tid & InternalName
    , _tidTId = tid
    }
