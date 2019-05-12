{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Annotations
    ( Mode(..), _Evaluation, _Types, _None
    ) where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH

import           Lamdu.Prelude

data Mode = Evaluation | Types | None
    deriving (Generic, Eq, Ord)

JsonTH.derivePrefixed "" ''Mode
Lens.makePrisms ''Mode
