{-# OPTIONS -O0 #-}
{-# LANGUAGE TemplateHaskell, FlexibleInstances, DerivingVia, RankNTypes #-}
module Lamdu.I18N.Texts
    ( module Lamdu.I18N.Texts
    , module Lamdu.I18N.Code
    , module Lamdu.I18N.CodeUI
    , module Lamdu.I18N.Collaboration
    , module Lamdu.I18N.Definitions
    , module Lamdu.I18N.StatusBar
    ) where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.EventMap as EventMap
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.Main as MainLoop
import qualified GUI.Momentu.Widgets.Choice as Choice
import qualified GUI.Momentu.Widgets.Grid as Grid
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Zoom as Zoom
import           Lamdu.I18N.Code
import           Lamdu.I18N.CodeUI
import           Lamdu.I18N.Collaboration
import           Lamdu.I18N.Definitions
import           Lamdu.I18N.StatusBar
import           Lamdu.Name (NameTexts)

import           Lamdu.Prelude

data Navigation a = Navigation
    { _jumpToError :: a
    , _goto :: a
    , _goBack :: a
    , _nextEntry :: a
    , _enterSubexpression :: a
    , _leaveSubexpression :: a
    , _blocked :: a
    , _prev :: a
    , _next :: a
    , _prevScopeArrow :: a
    , _nextScopeArrow :: a
    , _jumpToDefBody :: a
    }
    deriving stock (Generic, Generic1, Eq, Functor, Foldable, Traversable)
    deriving Applicative via (Generically1 Navigation)
Lens.makeLenses ''Navigation
JsonTH.derivePrefixed "_" ''Navigation

data Versioning a = Versioning
    { _branches :: a
    , _undo :: a
    , _redo :: a
    }
    deriving stock (Generic, Generic1, Eq, Functor, Foldable, Traversable)
    deriving Applicative via (Generically1 Versioning)
JsonTH.derivePrefixed "_" ''Versioning
Lens.makeLenses ''Versioning

data Texts a = Texts
    { _code :: Code a
    , _codeUI :: CodeUI a
    , _collaborationTexts :: Collaboration a
    , _navigationTexts :: Navigation a
    , _definitions :: Definitions a
    , _name :: NameTexts a
    , _statusBar :: StatusBar a
    , _versioning :: Versioning a
    , _dir :: Dir.Texts a
    , _glue :: Glue.Texts a
    , _menu :: Menu.Texts a
    , _searchMenu :: SearchMenu.Texts a
    , _grid :: Grid.Texts a
    , _eventMap :: EventMap.Texts a
    , _choice :: Choice.Texts a
    , _textEdit :: TextEdit.Texts a
    , _zoom :: Zoom.Texts a
    , _mainLoop :: MainLoop.Texts a
    }
    deriving stock (Generic, Generic1, Eq, Functor, Foldable, Traversable)
    deriving Applicative via (Generically1 Texts)
-- Get-field's dot is currently omitted from the symbols,
-- because it has special disambiguation logic implemented in the dotter etc.

Lens.makeLenses ''Texts
JsonTH.derivePrefixed "_" ''Texts
