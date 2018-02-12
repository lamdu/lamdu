{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}
module Lamdu.GUI.VersionControl.Config
    ( Config(..), Theme(..)
    ) where

import           Data.Aeson.Types (defaultOptions)
import           Data.Aeson.TH (deriveJSON)
import qualified GUI.Momentu.Draw as Draw
import           GUI.Momentu.MetaKey (MetaKey)

import           Lamdu.Prelude

data Config = Config
    { undoKeys :: [MetaKey]
    , redoKeys :: [MetaKey]
    , makeBranchKeys :: [MetaKey]
    , jumpToBranchesKeys :: [MetaKey]
    , delBranchKeys :: [MetaKey]
    } deriving (Eq, Show)
deriveJSON defaultOptions ''Config

newtype Theme = Theme
    { selectedBranchColor :: Draw.Color
    } deriving (Eq, Show)
deriveJSON defaultOptions ''Theme
