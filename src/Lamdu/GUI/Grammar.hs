{-# LANGUAGE TemplateHaskell #-}
module Lamdu.GUI.Grammar where

import qualified Control.Lens as Lens

import           Lamdu.Prelude

data Grammar = Grammar
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
    }
Lens.makeLenses ''Grammar

-- Get-field's dot is currently omitted from the symbols,
-- because it has special disambiguation logic implemented in the dotter etc.

grammar :: Grammar
grammar =
    Grammar
    { _assign = "="
    , _relay = "➾"
    , _let_ = "let"
    , _toNom = "«"
    , _fromNom = "»"
    , _repl = "⋙"
    , _case_ = "case"
    , _of_ = "of"
    , _absurd = "Ø"
    , _if_ = "if "
    , _else_ = "else"
    , _condColon = ": "
    , _elseShort = "el"
    , _inject = ":"
    , _nullaryInject = "."
    , _paramsRecordOpener = "Params {"
    , _paramsRecordCloser = "}"
    , _defUpdateHeader = "Update"
    , _defUpdateTo = "to: "
    , _defUpdateWas = "Type was: "
    , _defer = "|"
    , _lam = "λ"
    , _arrow = "→"
    , _textOpener = "“"
    , _textCloser = "„"
    , _recordOpener = "{"
    , _recordSep = ","
    , _recordCloser = "}"
    }
