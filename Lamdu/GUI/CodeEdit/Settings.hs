{-# LANGUAGE TemplateHaskell #-}
module Lamdu.GUI.CodeEdit.Settings
  ( Settings(..), sInfoMode, InfoMode(..), defaultInfoMode
  ) where

import qualified Control.Lens as Lens

data InfoMode = None | Types | Evaluation
  deriving (Eq, Ord, Show, Enum, Bounded)

defaultInfoMode :: InfoMode
defaultInfoMode = None

newtype Settings = Settings
  { _sInfoMode :: InfoMode
  }
Lens.makeLenses ''Settings
