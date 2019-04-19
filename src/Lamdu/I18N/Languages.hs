-- | The grammars we have already defined

module Lamdu.I18N.Languages where

import Lamdu.I18N.Texts (Texts(..))

-- TODO: Should these come from JSON files?

english :: Texts
english =
    Texts
    { _assign = "="
    , _relay = "⇒"
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
    , _newDefinitionButton = "New..."
    , _undeleteButton = "Undelete..."
    }

-- TODO: This will come from reader
texts :: Texts
texts = english
