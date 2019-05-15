{-# OPTIONS -O0 #-}
{-# LANGUAGE TemplateHaskell, DerivingVia #-}
module Lamdu.I18N.Name
    ( Name(..), unnamed, emptyName
    ) where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH

import           Lamdu.Prelude

data Name a = Name
    { _unnamed :: a
    , _emptyName :: a
    }
    deriving stock (Generic, Generic1, Eq, Ord, Show, Functor, Foldable, Traversable)
    deriving Applicative via (Generically1 Name)
Lens.makeLenses ''Name

JsonTH.derivePrefixed "_" ''Name
