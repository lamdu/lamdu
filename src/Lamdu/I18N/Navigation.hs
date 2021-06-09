{-# OPTIONS -O0 #-}
{-# LANGUAGE TemplateHaskell #-}
module Lamdu.I18N.Navigation where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH

import           Lamdu.Prelude

data Navigation a = Navigation
    { _jumpToError :: a
    , _closeHole :: a
    , _goto :: a
    , _goBack :: a
    , _goToParent :: a
    , _nextEntry :: a
    , _enterSubexpression :: a
    , _leaveSubexpression :: a
    , _blocked :: a
    , _prev :: a
    , _next :: a
    , _prevScopeArrow :: a
    , _nextScopeArrow :: a
    , _jumpToThen :: a
    , _jumpToTag :: a
    , _jumpToDef :: a
    , _jumpToDefBody :: a
    , _jumpToFirstUse :: a
    , _jumpToNextUse :: a
    , _moveInwards :: a
    } deriving Eq
Lens.makeLenses ''Navigation
JsonTH.derivePrefixed "_" ''Navigation
