-- | Initialize a database, populating it with "freshdb.json" if needed
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.Data.Db.Migration
    ( updateMissingCursor
    ) where

import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Db.Layout as DbLayout
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified GUI.Momentu as M

import           Lamdu.Prelude

type T = Transaction

-- | In commit 567ae4620f9222889a76a84936b9b09432a29698, the cursor
-- became a DB field. Easy to migrate from pre-cursor DB so do so:
updateMissingCursor :: T DbLayout.DbM ()
updateMissingCursor =
    do
        exists <- Transaction.irefExists DbLayout.guiState
        Transaction.writeIRef DbLayout.guiState (M.GUIState WidgetIds.defaultCursor mempty)
            & unless exists
