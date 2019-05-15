{-# LANGUAGE TemplateHaskell, DerivingVia #-}

module GUI.Momentu.I18N where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH

import           Lamdu.Prelude

data Texts a = Texts
    { _edit :: a
    , _insert :: a
    , _delete :: a
    }
    deriving stock (Generic, Generic1, Eq, Ord, Show, Functor, Foldable, Traversable)
    deriving Applicative via (Generically1 Texts)
Lens.makeLenses ''Texts
JsonTH.derivePrefixed "_" ''Texts
