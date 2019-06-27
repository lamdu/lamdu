{-# LANGUAGE TemplateHaskell, DerivingVia #-}

module GUI.Momentu.I18N where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH
import           GUI.Momentu.Animation.Id (ElemIds)

import           Lamdu.Prelude

data Texts a = Texts
    { _edit :: a
    , _view :: a
    , _insert :: a
    , _delete :: a
    , _navigation :: a
    , _move :: a
    , _choose :: a
    , _forward :: a
    , _backward :: a
    } deriving stock (Eq, Generic, Generic1, Functor, Foldable, Traversable)
    deriving Applicative via (Generically1 Texts)
    deriving anyclass ElemIds
Lens.makeLenses ''Texts
JsonTH.derivePrefixed "_" ''Texts
