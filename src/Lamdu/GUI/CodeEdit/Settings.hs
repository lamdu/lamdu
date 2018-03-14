{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, OverloadedStrings #-}
module Lamdu.GUI.CodeEdit.Settings
    ( AnnotationMode(..)
    , Settings(..), sAnnotationMode, sSelectedTheme
    , HasSettings(..)
    , initial
    ) where

import qualified Control.Lens as Lens
import           Lamdu.GUI.CodeEdit.AnnotationMode (AnnotationMode)
import qualified Lamdu.GUI.CodeEdit.AnnotationMode as AnnotationMode
import qualified Lamdu.Themes as Themes

import           Lamdu.Prelude

data Settings = Settings
    { _sAnnotationMode :: AnnotationMode
    , _sSelectedTheme :: Text
    }
Lens.makeLenses ''Settings

initial :: Settings
initial =
    Settings
    { _sAnnotationMode = AnnotationMode.initial
    , _sSelectedTheme = Themes.initial
    }

class HasSettings env where settings :: Lens' env Settings
