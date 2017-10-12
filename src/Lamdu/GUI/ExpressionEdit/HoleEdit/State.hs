{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.State
    ( setHoleStateAndJump, assocStateRef
    ) where

import           Data.UUID.Types (UUID)
import qualified Data.Store.Transaction as Transaction
import qualified GUI.Momentu.Widget as Widget
import           Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds (WidgetIds(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction.Transaction

setHoleStateAndJump :: Monad m => UUID -> Text -> Sugar.EntityId -> T m Widget.Id
setHoleStateAndJump uuid state entityId = do
    Transaction.setP (assocStateRef uuid) state
    WidgetIds.make entityId & hidOpen & return

assocStateRef :: Monad m => UUID -> Transaction.MkProperty m Text
assocStateRef = Transaction.assocDataRefDef mempty "searchTerm"
