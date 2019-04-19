{-# LANGUAGE TemplateHaskell #-}
module Lamdu.I18N.Texts where

import qualified Control.Lens as Lens

import           Lamdu.Prelude

data Texts = Texts
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
    , _defUpdateHeader :: Text
    , _defUpdateTo :: Text
    , _defUpdateWas :: Text
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
    , _newDefinitionButton :: Text
    , _undeleteButton :: Text
    }
Lens.makeLenses ''Texts

-- Get-field's dot is currently omitted from the symbols,
-- because it has special disambiguation logic implemented in the dotter etc.
