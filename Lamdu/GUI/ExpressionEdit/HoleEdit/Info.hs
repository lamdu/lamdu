module Lamdu.GUI.ExpressionEdit.HoleEdit.Info
    ( HoleInfo(..)
    , hiSearchTermProperty
    , hiSearchTerm
    ) where

import           Control.Lens.Operators
import           Data.Store.Property (Property(..))
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import           Lamdu.Calc.Type (Type)
import           Lamdu.GUI.ExpressionEdit.HoleEdit.State (HoleState, hsSearchTerm)
import           Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds (WidgetIds)
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import           Lamdu.Sugar.Names.Types (Name)
import           Lamdu.Sugar.NearestHoles (NearestHoles)
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction.Transaction

data HoleInfo m = HoleInfo
    { hiEntityId :: Sugar.EntityId
    , hiInferredType :: Type
    , hiIds :: WidgetIds
    , hiHole :: Sugar.Hole (Name m) m (ExprGuiT.SugarExpr m)
    , hiNearestHoles :: NearestHoles
    , hiState :: Property (T m) HoleState
    }

hiSearchTermProperty :: HoleInfo m -> Property (T m) String
hiSearchTermProperty holeInfo = hiState holeInfo & Property.composeLens hsSearchTerm

hiSearchTerm :: HoleInfo m -> String
hiSearchTerm holeInfo = Property.value (hiState holeInfo) ^. hsSearchTerm
