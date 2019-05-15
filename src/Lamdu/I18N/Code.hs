-- | Code texts
{-# OPTIONS -O0 #-}
{-# LANGUAGE TemplateHaskell, DerivingVia #-}
module Lamdu.I18N.Code where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH
import           GUI.Momentu.Animation.Id (ElemIds)

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
    deriving stock (Generic, Generic1, Eq, Functor, Foldable, Traversable)
    deriving anyclass ElemIds
    deriving Applicative via (Generically1 Code)
Lens.makeLenses ''Code
JsonTH.derivePrefixed "_" ''Code

