{-# LANGUAGE TemplateHaskell #-}

module Lamdu.Sugar.Config where

import qualified Control.Lens as Lens
import           Data.Aeson.TH (deriveJSON)
import qualified Data.Aeson.Types as Aeson
import           Data.List.Lens (prefixed)

import           Lamdu.Prelude

data Config = Config {} deriving (Eq, Show)
deriveJSON Aeson.defaultOptions
    {Aeson.fieldLabelModifier = (^?! prefixed "_")}
    ''Config
Lens.makeLenses ''Config
