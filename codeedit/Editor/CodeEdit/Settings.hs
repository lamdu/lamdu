{-# LANGUAGE TemplateHaskell #-}
module Editor.CodeEdit.Settings
  ( Settings(..), vsShowInferredTypes
  ) where

import qualified Control.Lens.TH as LensTH

newtype Settings = Settings
  { _vsShowInferredTypes :: Bool
  }
LensTH.makeLenses ''Settings
