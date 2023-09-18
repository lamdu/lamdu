-- | Code texts
{-# OPTIONS -O0 #-}
{-# LANGUAGE TemplateHaskell, DerivingVia #-}
module Lamdu.I18N.Code where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH
import           GUI.Momentu.Element.Id (ElemIds)

import           Lamdu.Prelude

-- All words here are reserved (conflicted when used as user names)
data Code a = Code
    { _assign :: a -- Assignment
    , _punnedFields :: a -- Apply
    , _let_ :: a
    , _repl :: a
    , -- Case
      _case_ :: a
    , -- If:
      _if_ :: a
    , _else_ :: a
    , _elseIf :: a -- shorthand for else-if, "elif" a la Python
    , -- Inject
      _injectSymbol :: a
    , -- Lambda:
      _defer :: a
    , _lam :: a
    , _arrow :: a
    , -- Literal a:
      _textOpener :: a
    , _textCloser :: a
    , -- Record:
      _recordOpener :: a
    , _caseOpener :: a
    , _compositeSeparator :: a
    , _compositeExtendTail :: a
    , _recordCloser :: a
    , _caseCloser :: a
      -- Kinds
    , _typ :: a
    , _row :: a
    }
    deriving stock (Generic, Generic1, Eq, Functor, Foldable, Traversable)
    deriving anyclass ElemIds
    deriving Applicative via (Generically1 Code)
Lens.makeLensesWith (Lens.lensRules & Lens.generateRecordSyntax .~ True) ''Code
JsonTH.derivePrefixed "_" ''Code

