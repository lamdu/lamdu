{-# LANGUAGE TemplateHaskell #-}
module Lamdu.GUI.VersionControl.Config
    ( Config(..), undoKeys, redoKeys, makeBranchKeys, jumpToBranchesKeys, delBranchKeys
    , Theme(..), selectedBranchColor
    ) where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH
import qualified GUI.Momentu.Draw as Draw

import           Lamdu.Prelude

data Config key = Config
    { _undoKeys :: [key]
    , _redoKeys :: [key]
    , _makeBranchKeys :: [key]
    , _jumpToBranchesKeys :: [key]
    , _delBranchKeys :: [key]
    } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)
JsonTH.derivePrefixed "_" ''Config

Lens.makeLenses ''Config

newtype Theme = Theme
    { _selectedBranchColor :: Draw.Color
    } deriving stock (Eq, Show)
JsonTH.derivePrefixed "_" ''Theme

Lens.makeLenses ''Theme
