module Lamdu.GUI.ExpressionEdit.HoleEdit.Info
  ( HoleInfo(..)
  , hiSearchTermProperty
  , hiSearchTerm
  , hiActiveId
  ) where

import Control.Lens.Operators
import Data.Store.Property (Property(..))
import Lamdu.GUI.ExpressionEdit.HoleEdit.Common (diveIntoHole)
import Lamdu.GUI.ExpressionEdit.HoleEdit.State (HoleState, hsSearchTerm)
import Lamdu.Sugar.AddNames.Types (Name)
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction.Transaction

-- | Active hole info
data HoleInfo m = HoleInfo
  { hiEntityId :: Sugar.EntityId
  , hiId :: Widget.Id
  , hiState :: Property (T m) HoleState
  , hiActions :: Sugar.HoleActions (Name m) m
  , hiSuggested :: Sugar.HoleSuggested (Name m) m
  , hiMArgument :: Maybe (Sugar.HoleArg m (ExprGuiM.SugarExpr m))
  , hiHoleEntityIds :: ExprGuiM.HoleEntityIds
  }

hiSearchTermProperty :: HoleInfo m -> Property (T m) String
hiSearchTermProperty holeInfo =
  Property.composeLens hsSearchTerm $ hiState holeInfo

hiActiveId :: HoleInfo m -> Widget.Id
hiActiveId = diveIntoHole . hiId

hiSearchTerm :: HoleInfo m -> String
hiSearchTerm holeInfo = Property.value (hiState holeInfo) ^. hsSearchTerm
