{-# LANGUAGE TemplateHaskell, ConstraintKinds #-}
module Lamdu.Style
    ( Style(..)
    , base, autoNameOrigin, nameAtBinder, bytes, text, char, num
    , HasStyle
    ) where

import qualified Control.Lens as Lens
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextView as TextView

import           Lamdu.Prelude

data Style = Style
    { _base :: TextEdit.Style
    , _autoNameOrigin :: TextEdit.Style
    , _nameAtBinder :: TextEdit.Style
    , _bytes :: TextEdit.Style
    , _text :: TextEdit.Style
    , _char :: TextEdit.Style
    , _num :: TextEdit.Style
    }
Lens.makeLenses ''Style

type HasStyle env = (Has TextEdit.Style env, Has TextView.Style env, Has Style env)
