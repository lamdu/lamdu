{-# LANGUAGE RecordWildCards, OverloadedStrings, TemplateHaskell, DeriveGeneric #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.State
    ( HoleState(..), hsSearchTerm
    , emptyState, setHoleStateAndJump, assocStateRef
    ) where

import qualified Control.Lens as Lens
import           Control.MonadA (MonadA)
import           Data.Binary (Binary)
import           Data.Store.Guid (Guid)
import qualified Data.Store.Transaction as Transaction
import           GHC.Generics (Generic)
import qualified Graphics.UI.Bottle.Widget as Widget
import           Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds (WidgetIds(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds as WidgetIds
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
    let WidgetIds{..} = WidgetIds.make entityId
    return hidOpenSearchTerm

assocStateRef :: MonadA m => Guid -> Transaction.MkProperty m HoleState
assocStateRef = Transaction.assocDataRefDef emptyState "searchTerm"
