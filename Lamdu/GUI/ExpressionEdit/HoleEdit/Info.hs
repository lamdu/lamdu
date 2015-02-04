module Lamdu.GUI.ExpressionEdit.HoleEdit.Info
  ( HoleIds(..), HoleInfo(..)
  , hiSearchTermProperty
  , hiSearchTerm
  ) where

import           Control.Lens.Operators
import           Data.Store.Property (Property(..))
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.Widget as Widget
import           Lamdu.Expr.Val (Val)
import           Lamdu.GUI.ExpressionEdit.HoleEdit.State (HoleState, hsSearchTerm)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import           Lamdu.Sugar.AddNames.Types (Name)
import           Lamdu.Sugar.NearestHoles (NearestHoles)
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction.Transaction

data HoleIds = HoleIds
  { hidOpen :: Widget.Id
  , hidClosed :: Widget.Id
  }

-- | Open hole info
data HoleInfo m = HoleInfo
  { hiEntityId :: Sugar.EntityId
  , hiIds :: HoleIds
  , hiState :: Property (T m) HoleState
  , hiActions :: Sugar.HoleActions (Name m) m
  , hiSuggested :: Val ()
  , hiMArgument :: Maybe (Sugar.HoleArg m (ExprGuiM.SugarExpr m))
  , hiNearestHoles :: NearestHoles
  }

hiSearchTermProperty :: HoleInfo m -> Property (T m) String
hiSearchTermProperty holeInfo =
  Property.composeLens hsSearchTerm $ hiState holeInfo

hiSearchTerm :: HoleInfo m -> String
hiSearchTerm holeInfo = Property.value (hiState holeInfo) ^. hsSearchTerm
