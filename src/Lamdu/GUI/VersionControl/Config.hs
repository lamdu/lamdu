{-# LANGUAGE DeriveGeneric #-}
module Lamdu.GUI.VersionControl.Config
    ( Config(..), Theme(..)
    ) where

import qualified Data.Aeson.Types as Aeson
import           GHC.Generics (Generic)
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.DrawingCombinators.Utils ()
import           Graphics.UI.Bottle.MetaKey (MetaKey)

data Config = Config
    { undoKeys :: [MetaKey]
    , redoKeys :: [MetaKey]
    , makeBranchKeys :: [MetaKey]
    , jumpToBranchesKeys :: [MetaKey]
    , delBranchKeys :: [MetaKey]
    } deriving (Eq, Generic, Show)

instance Aeson.ToJSON Config where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions
instance Aeson.FromJSON Config

newtype Theme = Theme
    { selectedBranchColor :: Draw.Color
    } deriving (Eq, Generic, Show)
instance Aeson.ToJSON Theme where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions
instance Aeson.FromJSON Theme
