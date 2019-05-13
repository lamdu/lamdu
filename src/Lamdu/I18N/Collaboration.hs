-- | Collaboration texts
{-# OPTIONS -O0 #-}
{-# LANGUAGE TemplateHaskell, FlexibleInstances, DerivingVia, RankNTypes #-}
module Lamdu.I18N.Collaboration where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH

import           Lamdu.Prelude

data Collaboration a = Collaboration
    { _collaboration :: a
    , _exportDefToJSON :: a
    , _exportEverythingToJSON :: a
    , _exportReplToJSON :: a
    , _exportReplToJS :: a
    , _importJSON :: a
    , _importReplFromJSON :: a
    }
    deriving stock (Generic, Generic1, Eq, Functor, Foldable, Traversable)
    deriving Applicative via (Generically1 Collaboration)
Lens.makeLenses ''Collaboration
JsonTH.derivePrefixed "_" ''Collaboration
