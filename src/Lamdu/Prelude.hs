{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Prelude
    ( module X
    , ByteString, Map, Set, Text
    , (<>)
    , forever, fromMaybe, guard, lift, traverse_, unless, void, when
    ) where

import Control.Lens.Operators as X
import Control.Lens.Tuple as X
import Control.Monad (forever, guard, unless, void, when)
import Control.Monad.Trans.Class (lift)
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)

import Prelude.Compat as X
