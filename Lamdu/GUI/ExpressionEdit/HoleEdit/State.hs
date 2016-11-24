{-# LANGUAGE RecordWildCards, OverloadedStrings, TemplateHaskell, DeriveGeneric #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.State
    ( HoleState(..), hsSearchTerm
    , emptyState, setHoleStateAndJump, assocStateRef
    ) where

import qualified Control.Lens as Lens
import           Data.Binary (Binary)
import           Data.UUID.Types (UUID)
import qualified Data.Store.Transaction as Transaction
import           Data.Text (Text)
import           GHC.Generics (Generic)
import qualified Graphics.UI.Bottle.Widget as Widget
import           Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds (WidgetIds(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction.Transaction

newtype HoleState = HoleState
    { _hsSearchTerm :: Text
    } deriving (Eq, Generic)
Lens.makeLenses ''HoleState
instance Binary HoleState

emptyState :: HoleState
emptyState =
    HoleState
    { _hsSearchTerm = ""
    }

setHoleStateAndJump :: Monad m => UUID -> HoleState -> Sugar.EntityId -> T m Widget.Id
setHoleStateAndJump uuid state entityId = do
    Transaction.setP (assocStateRef uuid) state
    let WidgetIds{..} = WidgetIds.make entityId
    return hidOpenSearchTerm

assocStateRef :: Monad m => UUID -> Transaction.MkProperty m HoleState
assocStateRef = Transaction.assocDataRefDef emptyState "searchTerm"
