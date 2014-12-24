{-# LANGUAGE TemplateHaskell #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.Open.ShownResult
  ( PickedResult(..), pickedInnerHoleGuid, pickedEventResult
  , ShownResult(..)
  , srPick
  ) where

import Control.Lens.Operators
import Data.Store.Guid (Guid)
import Lamdu.Sugar.AddNames.Types (Name)
import qualified Control.Lens as Lens
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction.Transaction

data PickedResult = PickedResult
  { _pickedInnerHoleGuid :: Maybe Guid
  , _pickedEventResult :: Widget.EventResult
  }
Lens.makeLenses ''PickedResult

data ShownResult m = ShownResult
  { srEventMap :: Widget.EventHandlers (T m)
  , srHoleResult :: Sugar.HoleResult (Name m) m
  , srPickTo :: T m PickedResult
  }

srPick :: Functor m => ShownResult m -> T m Widget.EventResult
srPick = fmap (^. pickedEventResult) . srPickTo
