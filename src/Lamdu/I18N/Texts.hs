{-# LANGUAGE TemplateHaskell #-}
module Lamdu.I18N.Texts where

import qualified Control.Lens as Lens
import           Data.Aeson.TH (deriveJSON)
import qualified Data.Aeson.Types as Aeson
import           Data.List.Lens (prefixed)
import           Lamdu.Config.Folder (HasConfigFolder(..))

import           Lamdu.Prelude

data CodeTexts = CodeTexts
    { _assign :: Text -- Assignment
    , _relay :: Text -- Apply
    , _let_ :: Text
    , _toNom :: Text
    , _fromNom :: Text
    , _repl :: Text
    , -- Case
      _case_ :: Text
    , _of_ :: Text
    , _absurd :: Text
    , -- If:
      _if_ :: Text
    , _condColon :: Text -- Colon after if's condition
    , _else_ :: Text
    , _elseShort :: Text -- "el" in "elif"
    , -- Inject
      _inject :: Text
    , _nullaryInject :: Text
    , -- Getvar
      _paramsRecordOpener :: Text
    , _paramsRecordCloser :: Text
    , -- Lambda:
      _defer :: Text
    , _lam :: Text
    , _arrow :: Text
    , -- Literal text:
      _textOpener :: Text
    , _textCloser :: Text
    , -- Record:
      _recordOpener :: Text
    , _recordSep :: Text
    , _recordCloser :: Text
    }
    deriving (Eq, Ord, Show)
Lens.makeLenses ''CodeTexts
deriveJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = (^?! prefixed "_")} ''CodeTexts

data CodeButtonTexts = CodeButtonTexts
    { _newDefinitionButton :: Text
    , _undeleteButton :: Text
    , _defUpdateHeader :: Text
    , _defUpdateTo :: Text
    , _defUpdateWas :: Text
    }
    deriving (Eq, Ord, Show)
Lens.makeLenses ''CodeButtonTexts
deriveJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = (^?! prefixed "_")} ''CodeButtonTexts

data Texts = Texts
    { -- TODO: Should this still be called "Texts?"
      -- Using a boolean for the JSON instance
      _isLeftToRight :: Bool
    , _code :: CodeTexts
    , _codeButtons :: CodeButtonTexts
    }
    deriving (Eq, Ord, Show)
-- Get-field's dot is currently omitted from the symbols,
-- because it has special disambiguation logic implemented in the dotter etc.

Lens.makeLenses ''Texts
deriveJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = (^?! prefixed "_")} ''Texts

instance HasConfigFolder Texts where
    configFolder _ = "languages"

class HasTexts env where texts :: Lens' env Texts
instance HasTexts Texts where texts = id
