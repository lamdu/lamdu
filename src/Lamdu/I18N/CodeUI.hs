-- | CodeUI textxs
{-# OPTIONS -O0 #-}
{-# LANGUAGE TemplateHaskell, FlexibleInstances, DerivingVia, RankNTypes #-}
module Lamdu.I18N.CodeUI where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH

import           Lamdu.Prelude

data CodeUI a = CodeUI
    { _hidden :: a
    , _shown :: a
    , _pick :: a
    , _new :: a
    , _select :: a
    , _delete :: a
    , _rename :: a
    , _doneRenaming :: a
    , _changeImportedName :: a
    , _doneChangingImportedName :: a
    , _pane :: a
    , _close :: a
    , _moveDown :: a
    , _moveUp :: a
    , _presentationMode :: a
    , _pModeVerbose :: a
    , _pModeOO :: a
    , _pModeInfix :: a
    , _jsException :: a
    , _jsReachedAHole :: a
    , _jsStaleDep :: a
    , _jsUnhandledCase :: a
    , _transform :: a
    , _replace :: a
    , _replaceParent :: a
    , _applyOperator :: a
    , _add :: a
    , _letClause :: a
    , _modify :: a
    , _detach :: a
    , _literal :: a
    , _literalText :: a
    , _literalNumber :: a
    , _startEditing :: a
    , _stopEditing :: a
    , _setToHole :: a
    , _negate :: a
    , _value :: a
    , _evaluation :: a
    , _scope :: a
    }
    deriving stock (Generic, Generic1, Eq, Functor, Foldable, Traversable)
    deriving Applicative via (Generically1 CodeUI)
Lens.makeLenses ''CodeUI
JsonTH.derivePrefixed "_" ''CodeUI

