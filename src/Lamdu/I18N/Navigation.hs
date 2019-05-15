{-# OPTIONS -O0 #-}
{-# LANGUAGE TemplateHaskell, FlexibleInstances, DerivingVia, RankNTypes #-}
module Lamdu.I18N.Navigation where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH

import           Lamdu.Prelude

data Navigation a = Navigation
    { _jumpToError :: a
    , _goto :: a
    , _goBack :: a
    , _nextEntry :: a
    , _enterSubexpression :: a
    , _leaveSubexpression :: a
    , _blocked :: a
    , _prev :: a
    , _next :: a
    , _prevScopeArrow :: a
    , _nextScopeArrow :: a
    , _jumpToDef :: a
    , _jumpToDefBody :: a
    , _jumpToFirstUse :: a
    , _jumpToNextUse :: a
    , _moveInwards :: a
    }
    deriving stock (Generic, Generic1, Eq, Functor, Foldable, Traversable)
    deriving Applicative via (Generically1 Navigation)
Lens.makeLenses ''Navigation
JsonTH.derivePrefixed "_" ''Navigation
