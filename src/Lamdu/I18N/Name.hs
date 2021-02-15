{-# OPTIONS -O0 #-}
{-# LANGUAGE TemplateHaskell #-}
module Lamdu.I18N.Name where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH

import           Lamdu.Prelude

-- All words here are reserved (conflicted when used as user names)
data Name a = Name
    { _unnamed :: a
    , _emptyName :: a
    } deriving (Eq, Functor, Foldable, Traversable)
Lens.makeLenses ''Name

JsonTH.derivePrefixed "_" ''Name
