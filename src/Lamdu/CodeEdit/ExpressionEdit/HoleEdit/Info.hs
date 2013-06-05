{-# LANGUAGE TemplateHaskell #-}
module Lamdu.CodeEdit.ExpressionEdit.HoleEdit.Info
  ( HoleInfo(..), hsSearchTerm
  , HoleState(..), emptyState, hiSearchTerm
  ) where

import Control.Lens.Operators
import Data.Binary (Binary(..))
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Store.Guid (Guid)
import Data.Store.Property (Property(..))
import Data.Store.Transaction (Transaction)
import qualified Control.Lens.TH as LensTH
import qualified Data.Store.Property as Property
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.CodeEdit.Sugar as Sugar

type T = Transaction

newtype HoleState = HoleState
  { _hsSearchTerm :: String
  } deriving Eq
LensTH.makeLenses ''HoleState
derive makeBinary ''HoleState

emptyState :: HoleState
emptyState =
  HoleState
  { _hsSearchTerm = ""
  }

data HoleInfo m = HoleInfo
  { hiGuid :: Guid
  , hiHoleId :: Widget.Id
  , hiState :: Property (T m) HoleState
  , hiHoleActions :: Sugar.HoleActions Sugar.Name m
  , hiMArgument :: Maybe (Sugar.ExpressionN m)
  , hiMNextHole :: Maybe (Sugar.ExpressionN m)
  }

hiSearchTerm :: HoleInfo m -> String
hiSearchTerm holeInfo = Property.value (hiState holeInfo) ^. hsSearchTerm
