{-# LANGUAGE TemplateHaskell #-}
module Lamdu.GUI.CodeEdit.Settings
  ( Settings(..), sInfoMode, InfoMode(..), defaultInfoMode
  ) where

import qualified Control.Lens as Lens

data InfoMode = None | Types | Examples
  deriving (Show)

defaultInfoMode :: InfoMode
defaultInfoMode = None

newtype Settings = Settings
  { _sInfoMode :: InfoMode
  }
Lens.makeLenses ''Settings
