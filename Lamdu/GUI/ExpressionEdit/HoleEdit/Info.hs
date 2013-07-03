{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.Info
  ( HoleInfo(..), hsSearchTerm
  , HoleState(..), emptyState, hiSearchTerm
  , setHoleStateAndJump, assocStateRef
  ) where

import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Binary (Binary(..))
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Store.Guid (Guid)
import Data.Store.Property (Property(..))
import Data.Store.Transaction (Transaction, MkProperty)
import qualified Control.Lens as Lens
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction

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

data HoleInfo m = HoleInfo
  { hiStoredGuid :: Guid
  , hiId :: Widget.Id
  , hiState :: Property (T m) HoleState
  , hiActions :: Sugar.HoleActions Sugar.Name m
  , hiMArgument :: Maybe (Sugar.HoleArg m (ExprGuiM.SugarExpr m))
  , hiHoleGuids :: ExprGuiM.HoleGuids
  }

hiSearchTerm :: HoleInfo m -> String
hiSearchTerm holeInfo = Property.value (hiState holeInfo) ^. hsSearchTerm

searchTermWIdOfHoleGuid :: Guid -> Widget.Id
searchTermWIdOfHoleGuid = WidgetIds.searchTermId . FocusDelegator.delegatingId . WidgetIds.fromGuid

setHoleStateAndJump :: MonadA m => HoleState -> Guid -> T m Widget.Id
setHoleStateAndJump newHoleState newHoleGuid = do
  Transaction.setP (assocStateRef newHoleGuid) newHoleState
  return $ searchTermWIdOfHoleGuid newHoleGuid

assocStateRef :: MonadA m => Guid -> MkProperty m HoleState
assocStateRef = Transaction.assocDataRefDef emptyState "searchTerm"
