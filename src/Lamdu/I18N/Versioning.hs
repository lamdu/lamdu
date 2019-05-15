{-# OPTIONS -O0 #-}
{-# LANGUAGE TemplateHaskell #-}
module Lamdu.I18N.Versioning where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH

import           Lamdu.Prelude

data Versioning a = Versioning
    { _branches :: a
    , _undo :: a
    , _redo :: a
    } deriving Eq
JsonTH.derivePrefixed "_" ''Versioning
Lens.makeLenses ''Versioning
