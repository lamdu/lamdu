{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Annotations
    ( Mode(..), _Evaluation, _Types, _None
    ) where

import qualified Control.Lens as Lens

import           Lamdu.Prelude

data Mode = Evaluation | Types | None
    deriving (Eq, Ord)

Lens.makePrisms ''Mode
