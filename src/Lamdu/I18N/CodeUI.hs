-- | CodeUI textxs
{-# OPTIONS -O0 #-}
{-# LANGUAGE TemplateHaskell, DerivingVia #-}
module Lamdu.I18N.CodeUI where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH
import           GUI.Momentu.Animation.Id (ElemIds)

import           Lamdu.Prelude

data CodeUI a = CodeUI
    { _hidden :: a
    , _shown :: a
    , _hide :: a
    , _show :: a
    , _pick :: a
    , _new :: a
    , _apply :: a
    , _lambda :: a
    , _completion :: a
    , _rename :: a
    , _doneRenaming :: a
    , _changeImportedName :: a
    , _doneChangingImportedName :: a
    , _pane :: a
    , _nominal :: a
    , _deleteToNominal :: a
    , _deleteFromNominal :: a
    , _inject :: a
    , _caseLabel :: a
    , _addAlt :: a
    , _deleteAlt :: a
    , _toLambdaCase :: a
    , _open :: a
    , _close :: a
    , _shrinkLambdaParams :: a
    , _expandLambdaParams :: a
    , _getField :: a
    , _dot :: a
    , _deleteDot :: a
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
    , _inline :: a
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
    , _fragment :: a
    , _showResults :: a
    , _heal :: a
    , _setToHole :: a
    , _negate :: a
    , _value :: a
    , _evaluation :: a
    , _scope :: a
    }
    deriving stock (Generic, Generic1, Eq, Functor, Foldable, Traversable)
    deriving anyclass ElemIds
    deriving Applicative via (Generically1 CodeUI)
Lens.makeLenses ''CodeUI
JsonTH.derivePrefixed "_" ''CodeUI

