{-# LANGUAGE DeriveGeneric #-}
module Lamdu.GUI.VersionControl.Config
    ( Config(..)
    ) where

import           Data.Aeson (ToJSON(..), FromJSON(..))
import           GHC.Generics (Generic)
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.DrawingCombinators.Utils ()
import           Graphics.UI.Bottle.ModKey (ModKey(..))

data Config = Config
  { undoKeys :: [ModKey]
  , redoKeys :: [ModKey]
  , makeBranchKeys :: [ModKey]
  , jumpToBranchesKeys :: [ModKey]
  , delBranchKeys :: [ModKey]
  , selectedBranchColor :: Draw.Color
  } deriving (Eq, Generic, Show)

instance ToJSON Config
instance FromJSON Config
