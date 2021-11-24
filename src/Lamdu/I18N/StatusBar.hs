-- | StatusBar texts
{-# OPTIONS -O0 #-}
{-# LANGUAGE TemplateHaskell, DerivingVia #-}
module Lamdu.I18N.StatusBar where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH
import           GUI.Momentu.Animation.Id (ElemIds)

import           Lamdu.Prelude

data StatusBar a = StatusBar
    { _sbStatusBar :: a
    , _sbAnnotations :: a
    , _sbTypes :: a
    , _sbNone :: a
    , _sbSwitchAnnotations :: a
    , _sbBranch :: a
    , _sbSwitchHelp :: a
    , _sbHelp :: a
    , _sbSwitchLanguage :: a
    , _sbTheme :: a
    , _sbSwitchTheme :: a
    , _sbExtraOptions :: a
    }
    deriving stock (Generic, Generic1, Eq, Functor, Foldable, Traversable)
    deriving anyclass ElemIds
    deriving Applicative via (Generically1 StatusBar)
Lens.makeLenses ''StatusBar
JsonTH.derivePrefixed "_sb" ''StatusBar
