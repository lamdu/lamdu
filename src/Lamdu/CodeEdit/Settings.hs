{-# LANGUAGE TemplateHaskell #-}
module Lamdu.CodeEdit.Settings
  ( Settings(..), sInfoMode, InfoMode(..), defaultInfoMode
  ) where

import qualified Control.Lens.TH as LensTH

data InfoMode = InfoNone | InfoTypes | InfoExamples

defaultInfoMode :: InfoMode
defaultInfoMode = InfoNone

newtype Settings = Settings
  { _sInfoMode :: InfoMode
  }
LensTH.makeLenses ''Settings
