{-# LANGUAGE TemplateHaskell, NamedFieldPuns, DisambiguateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ConstraintKinds #-}
module Lamdu.Style
    ( Style(..)
    , base, autoNameOrigin, nameAtBinder, bytes, text, num, sprites
    , HasStyle
    , LoadedSprites
    ) where

import qualified Control.Lens as Lens
import           GUI.Momentu.Draw (Sprite)
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import           Lamdu.Config.Theme.Sprites (Sprites)

import           Lamdu.Prelude

type LoadedSprites = Sprites Sprite

data Style = Style
    { _base :: TextEdit.Style
    , _autoNameOrigin :: TextEdit.Style
    , _nameAtBinder :: TextEdit.Style
    , _bytes :: TextEdit.Style
    , _text :: TextEdit.Style
    , _num :: TextEdit.Style
    , _sprites :: LoadedSprites
    }
Lens.makeLenses ''Style

type HasStyle env = (TextEdit.HasStyle env, Has Style env)
