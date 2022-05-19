-- | Collaboration texts
{-# OPTIONS -O0 #-}
{-# LANGUAGE TemplateHaskell #-}
module Lamdu.I18N.Collaboration where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH

import           Lamdu.Prelude

data Collaboration a = Collaboration
    { _collaboration :: a
    , _exportDefToJSON :: a
    , _exportTagToJSON :: a
    , _exportNominalToJSON :: a
    , _exportEverythingToJSON :: a
    , _exportDefToJS :: a
    , _importJSON :: a
    } deriving Eq
Lens.makeLenses ''Collaboration
JsonTH.derivePrefixed "_" ''Collaboration
