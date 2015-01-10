module Lamdu.GUI.ExpressionEdit.HoleEdit.Info
  ( HoleIds(..), HoleInfo(..), EditableHoleInfo(..)
  , ehiSearchTermProperty
  , ehiSearchTerm
  ) where

import           Control.Lens.Operators
import           Data.Store.Property (Property(..))
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.Widget as Widget
import           Lamdu.Expr.Type (Type)
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

data HoleInfo m = HoleInfo
  { hiEntityId :: Sugar.EntityId
  , hiInferredType :: Type
  , hiIds :: HoleIds
  , hiSuggested :: Val ()
  , hiMArgument :: Maybe (Sugar.HoleArg m (ExprGuiM.SugarExpr m))
  , hiNearestHoles :: NearestHoles
  }

data EditableHoleInfo m = EditableHoleInfo
  { ehiState :: Property (T m) HoleState
  , ehiActions :: Sugar.HoleActions (Name m) m
  , ehiInfo :: HoleInfo m
  }

ehiSearchTermProperty :: EditableHoleInfo m -> Property (T m) String
ehiSearchTermProperty holeInfo =
  Property.composeLens hsSearchTerm $ ehiState holeInfo

ehiSearchTerm :: EditableHoleInfo m -> String
ehiSearchTerm holeInfo = Property.value (ehiState holeInfo) ^. hsSearchTerm
