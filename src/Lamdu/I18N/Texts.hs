{-# LANGUAGE TemplateHaskell, FlexibleInstances, DerivingVia, RankNTypes #-}
module Lamdu.I18N.Texts where

import qualified Control.Lens as Lens
import           Data.Aeson.TH (deriveJSON)
import qualified Data.Aeson.Types as Aeson
import           Data.Char (toLower)
import           Data.List.Lens (prefixed)
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.EventMap as EventMap
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.Widgets.Choice as Choice
import           Lamdu.Config.Folder (HasConfigFolder(..))

import           Lamdu.Prelude

data Code a = Code
    { _assign :: a -- Assignment
    , _relay :: a -- Apply
    , _let_ :: a
    , _toNom :: a
    , _fromNom :: a
    , _repl :: a
    , -- Case
      _case_ :: a
    , _of_ :: a
    , _absurd :: a
    , -- If:
      _if_ :: a
    , _condColon :: a -- Colon after if's condition
    , _else_ :: a
    , _elseShort :: a -- "el" in "elif"
    , -- Inject
      _inject :: a
    , _nullaryInject :: a
    , -- Getvar
      _paramsRecordOpener :: a
    , _paramsRecordCloser :: a
    , -- Lambda:
      _defer :: a
    , _lam :: a
    , _arrow :: a
    , -- Literal a:
      _textOpener :: a
    , _textCloser :: a
    , -- Record:
      _recordOpener :: a
    , _recordSep :: a
    , _recordCloser :: a
    }
    deriving stock (Generic, Generic1, Eq, Ord, Show, Functor, Foldable, Traversable)
    deriving Applicative via (Generically1 Code)
Lens.makeLenses ''Code
deriveJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = (^?! prefixed "_")} ''Code

data CodeUI a = CodeUI
    { _newDefinitionButton :: a
    , _undeleteButton :: a
    , _defUpdateHeader :: a
    , _defUpdateTo :: a
    , _defUpdateWas :: a
    , _goto :: a
    , _quit :: a
    }
    deriving stock (Generic, Generic1, Eq, Ord, Show, Functor, Foldable, Traversable)
    deriving Applicative via (Generically1 CodeUI)
Lens.makeLenses ''CodeUI
deriveJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = (^?! prefixed "_")} ''CodeUI

data StatusBar a = StatusBar
    { _sbStatusBar :: a
    , _sbAnnotations :: a
    , _sbSwitchAnnotations :: a
    , _sbBranch :: a
    , _sbSwitchHelp :: a
    , _sbHelp :: a
    , _sbLanguage :: a
    , _sbSwitchLanguage :: a
    , _sbTheme :: a
    , _sbSwitchTheme :: a
    }
    deriving stock (Generic, Generic1, Eq, Ord, Show, Functor, Foldable, Traversable)
    deriving Applicative via (Generically1 StatusBar)
Lens.makeLenses ''StatusBar
deriveJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier =
        (Lens.ix 0 %~ toLower) . (^?! prefixed "_sb")
    } ''StatusBar

data Versioning a = Versioning
    { _branches :: a
    , _new :: a
    , _rename :: a
    , _doneRenaming :: a
    , _edit :: a
    , _undo :: a
    , _redo :: a
    , _select :: a
    , _delete :: a
    }
    deriving stock (Generic, Generic1, Eq, Ord, Show, Functor, Foldable, Traversable)
    deriving Applicative via (Generically1 Versioning)
deriveJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = (^?! prefixed "_")} ''Versioning
Lens.makeLenses ''Versioning

data Texts a = Texts
    { _code :: Code a
    , _codeUI :: CodeUI a
    , _statusBar :: StatusBar a
    , _versioning :: Versioning a
    , _dir :: Dir.Texts a
    , _glue :: Glue.Texts a
    , _eventMap :: EventMap.Texts a
    , _choice :: Choice.Texts a
    }
    deriving stock (Generic, Generic1, Eq, Ord, Show, Functor, Foldable, Traversable)
    deriving Applicative via (Generically1 Texts)
-- Get-field's dot is currently omitted from the symbols,
-- because it has special disambiguation logic implemented in the dotter etc.

Lens.makeLenses ''Texts
deriveJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = (^?! prefixed "_")} ''Texts

data Language = Language
    { -- TODO: Should this still be called "Texts?"
      _lDirection :: Dir.Layout
    , _lTexts :: Texts Text
    } deriving (Eq, Show)

Lens.makeLenses ''Language
deriveJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier =
        (Lens.ix 0 %~ toLower) . (^?! prefixed "_l")
    } ''Language

instance HasConfigFolder Language where
    configFolder _ = "languages"

class
    ( Glue.HasTexts env, Dir.HasTexts env, Choice.HasTexts env
    ) => HasLanguage env where
    language :: Lens' env Language
instance EventMap.HasTexts Language where texts = lTexts . eventMap

instance Dir.HasLayoutDir Language where layoutDir = lDirection
instance Dir.HasTexts Language where texts = lTexts . dir
instance Glue.HasTexts Language where texts = lTexts . glue
instance Choice.HasTexts Language where texts = lTexts . choice
instance HasLanguage Language where language = id

texts :: HasLanguage env => Lens' env (Texts Text)
texts = language . lTexts
