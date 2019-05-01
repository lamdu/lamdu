{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module Lamdu.I18N.Texts where

import qualified Control.Lens as Lens
import           Data.Aeson.TH (deriveJSON)
import qualified Data.Aeson.Types as Aeson
import           Data.List.Lens (prefixed)
import           Lamdu.Config.Folder (HasConfigFolder(..))

import           Lamdu.Prelude

data CodeTexts a = CodeTexts
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
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
Lens.makeLenses ''CodeTexts
deriveJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = (^?! prefixed "_")} ''CodeTexts

data CodeUITexts a = CodeUITexts
    { _newDefinitionButton :: a
    , _undeleteButton :: a
    , _defUpdateHeader :: a
    , _defUpdateTo :: a
    , _defUpdateWas :: a
    }
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
Lens.makeLenses ''CodeUITexts
deriveJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = (^?! prefixed "_")} ''CodeUITexts

data Texts a = Texts
    { -- TODO: Should this still be called "Texts?"
      -- Using a boolean for the JSON instance
      _isLeftToRight :: Bool
    , _code :: CodeTexts a
    , _codeUI :: CodeUITexts a
    }
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
-- Get-field's dot is currently omitted from the symbols,
-- because it has special disambiguation logic implemented in the dotter etc.

Lens.makeLenses ''Texts
deriveJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = (^?! prefixed "_")} ''Texts

type Language = Texts Text

instance HasConfigFolder Language where
    configFolder _ = "languages"

class HasTexts env where texts :: Lens' env Language
instance HasTexts (Texts Text) where texts = id

-- TODO: Better way to do this? Auto-generated applicative instance?
dummyTexts :: Texts ()
dummyTexts =
    Texts
    { _isLeftToRight = True
    , _code = CodeTexts () () () () () () () () () () () () () () () () () () () () () () () () ()
    , _codeUI = CodeUITexts () () () () ()
    }
