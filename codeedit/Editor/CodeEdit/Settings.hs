{-# LANGUAGE TemplateHaskell #-}
module Editor.CodeEdit.Settings
  ( Settings(..), sInfoMode, InfoMode(..)
  ) where

import qualified Control.Lens.TH as LensTH

data InfoMode = InfoNone | InfoTypes | InfoExamples

newtype Settings = Settings
  { _sInfoMode :: InfoMode
  }
LensTH.makeLenses ''Settings
