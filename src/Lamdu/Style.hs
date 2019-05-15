{-# LANGUAGE TemplateHaskell, NamedFieldPuns, DisambiguateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ConstraintKinds #-}
module Lamdu.Style
    ( Style(..), base, autoNameOrigin, nameAtBinder, bytes, text, num
    , HasStyle
    ) where

import qualified Control.Lens as Lens
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import           Lamdu.Prelude

data Style = Style
    { _base :: TextEdit.Style
    , _autoNameOrigin :: TextEdit.Style
    , _nameAtBinder :: TextEdit.Style
    , _bytes :: TextEdit.Style
    , _text :: TextEdit.Style
    , _num :: TextEdit.Style
    }
Lens.makeLenses ''Style

type HasStyle env = (TextEdit.HasStyle env, Has Style env)
