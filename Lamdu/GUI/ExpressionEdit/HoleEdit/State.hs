{-# LANGUAGE OverloadedStrings, TemplateHaskell, DeriveGeneric #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.State
  ( HoleState(..), hsSearchTerm
  , emptyState, setHoleStateAndJump, assocStateRef
  ) where

import Control.MonadA (MonadA)
import Data.Binary (Binary)
import Data.Store.Guid (Guid)
import GHC.Generics (Generic)
import Lamdu.GUI.ExpressionEdit.HoleEdit.Common (searchTermWIdOfHoleEntityId)
import qualified Control.Lens as Lens
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction.Transaction

newtype HoleState = HoleState
  { _hsSearchTerm :: String
  } deriving (Eq, Generic)
Lens.makeLenses ''HoleState
instance Binary HoleState

emptyState :: HoleState
emptyState =
  HoleState
  { _hsSearchTerm = ""
  }

setHoleStateAndJump :: MonadA m => Guid -> HoleState -> Sugar.EntityId -> T m Widget.Id
setHoleStateAndJump guid state entityId = do
  Transaction.setP (assocStateRef guid) state
  return $ searchTermWIdOfHoleEntityId entityId

assocStateRef :: MonadA m => Guid -> Transaction.MkProperty m HoleState
assocStateRef = Transaction.assocDataRefDef emptyState "searchTerm"
