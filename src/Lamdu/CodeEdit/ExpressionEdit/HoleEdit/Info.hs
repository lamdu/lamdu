{-# LANGUAGE TemplateHaskell #-}
module Lamdu.CodeEdit.ExpressionEdit.HoleEdit.Info
  ( HoleInfo(..), hsSearchTerm, hsArgument
  , HoleState(..), emptyState
  ) where

import Data.Binary (Binary(..))
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Store.Guid (Guid)
import Data.Store.Property (Property(..))
import Data.Store.Transaction (Transaction)
import qualified Control.Lens.TH as LensTH
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.CodeEdit.Sugar as Sugar

type T = Transaction

data HoleState m = HoleState
  { _hsSearchTerm :: String
  , _hsArgument :: Maybe (Sugar.ExprStorePoint m)
  } deriving Eq
LensTH.makeLenses ''HoleState
derive makeBinary ''HoleState

emptyState :: HoleState m
emptyState =
  HoleState
  { _hsSearchTerm = ""
  , _hsArgument = Nothing
  }

data HoleInfo m = HoleInfo
  { hiGuid :: Guid
  , hiHoleId :: Widget.Id
  , hiState :: Property (T m) (HoleState m)
  , hiHoleActions :: Sugar.HoleActions Sugar.Name m
  , hiMNextHole :: Maybe (Sugar.ExpressionN m)
  }
