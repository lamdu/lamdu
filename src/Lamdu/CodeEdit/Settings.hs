{-# LANGUAGE TemplateHaskell #-}
module Lamdu.CodeEdit.Settings
  ( Settings(..), sInfoMode, InfoMode(..), defaultInfoMode
  ) where

import qualified Control.Lens.TH as LensTH

data InfoMode = None | Types | Examples
  deriving (Show)

defaultInfoMode :: InfoMode
defaultInfoMode = None

newtype Settings = Settings
  { _sInfoMode :: InfoMode
  }
LensTH.makeLenses ''Settings
