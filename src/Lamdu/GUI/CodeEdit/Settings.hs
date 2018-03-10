{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, OverloadedStrings #-}
module Lamdu.GUI.CodeEdit.Settings
    ( Settings(..), sAnnotationMode
    , HasSettings(..)
    , initial
    ) where

import qualified Control.Lens as Lens
import           Lamdu.GUI.CodeEdit.AnnotationMode (AnnotationMode)
import qualified Lamdu.GUI.CodeEdit.AnnotationMode as AnnotationMode

import           Lamdu.Prelude

newtype Settings = Settings
    { _sAnnotationMode :: AnnotationMode
    }
Lens.makeLenses ''Settings

initial :: Settings
initial = Settings AnnotationMode.initial

class HasSettings env where settings :: Lens' env Settings
