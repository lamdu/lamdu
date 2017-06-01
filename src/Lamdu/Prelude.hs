{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Prelude
    ( module X
    ) where

import Control.Lens.Operators as X
import Control.Lens.Tuple as X
import Control.Monad as X (forever, guard, unless, void, when)
import Control.Monad.Trans.Class as X (lift)
import Data.ByteString as X (ByteString)
import Data.Foldable as X (traverse_)
import Data.Maybe as X (fromMaybe)
import Data.Monoid as X ((<>))
import Data.Map as X (Map)
import Data.Set as X (Set)
import Data.Text as X (Text)

import Prelude.Compat as X
