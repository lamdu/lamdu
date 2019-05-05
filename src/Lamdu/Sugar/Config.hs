{-# LANGUAGE TemplateHaskell #-}

module Lamdu.Sugar.Config where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH

import           Lamdu.Prelude

newtype Config = Config
    { _showAllAnnotations :: Bool
    } deriving (Eq, Show)
JsonTH.derivePrefixed "_" ''Config
Lens.makeLenses ''Config
