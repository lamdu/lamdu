{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.State
  ( HoleState(..), hsSearchTerm
  , emptyState, setHoleStateAndJump, assocStateRef
  ) where

import Control.MonadA (MonadA)
import Data.Binary (Binary(..))
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Store.Guid (Guid)
import Lamdu.GUI.ExpressionEdit.HoleEdit.Common (searchTermWIdOfHoleGuid)
import qualified Control.Lens as Lens
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.Widget as Widget

type T = Transaction.Transaction

newtype HoleState = HoleState
  { _hsSearchTerm :: String
  } deriving Eq
Lens.makeLenses ''HoleState
derive makeBinary ''HoleState

emptyState :: HoleState
emptyState =
  HoleState
  { _hsSearchTerm = ""
  }

setHoleStateAndJump :: MonadA m => HoleState -> Guid -> T m Widget.Id
setHoleStateAndJump newHoleState newHoleGuid = do
  Transaction.setP (assocStateRef newHoleGuid) newHoleState
  return $ searchTermWIdOfHoleGuid newHoleGuid

assocStateRef :: MonadA m => Guid -> Transaction.MkProperty m HoleState
assocStateRef = Transaction.assocDataRefDef emptyState "searchTerm"
