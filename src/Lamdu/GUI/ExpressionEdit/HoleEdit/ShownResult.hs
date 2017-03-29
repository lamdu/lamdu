{-# LANGUAGE TemplateHaskell #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.ShownResult
    ( PickedResult(..), pickedEventResult, pickedIdTranslations
    , ShownResult(..)
    ) where

import qualified Control.Lens as Lens
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.Widget as Widget
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)

type T = Transaction.Transaction

data PickedResult = PickedResult
    { _pickedEventResult :: Widget.EventResult
    , _pickedIdTranslations :: Widget.Id -> Widget.Id
    }
Lens.makeLenses ''PickedResult

data ShownResult m = ShownResult
    { srMkEventMap :: ExprGuiM m (Widget.EventMap (T m Widget.EventResult))
    , srHasHoles :: Bool
    , srPick :: T m PickedResult
    }
