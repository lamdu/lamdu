-- | Import/Export JSON support
{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Data.Export.JSON
    ( export
    ) where

import qualified Data.Aeson as Aeson
import           Data.Store.IRef
import           Data.Store.Transaction (Transaction)
import           Lamdu.Data.DbLayout (ViewM)
import qualified Lamdu.Data.DbLayout as DbLayout
import           Lamdu.Expr.IRef (ValI)

import           Prelude.Compat

type T = Transaction

_replIRef :: IRef ViewM (ValI ViewM)
_replIRef = DbLayout.repl DbLayout.codeIRefs

export :: T ViewM Aeson.Value
export = return Aeson.Null
