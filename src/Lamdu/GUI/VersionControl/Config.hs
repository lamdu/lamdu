{-# LANGUAGE TemplateHaskell #-}
module Lamdu.GUI.VersionControl.Config
    ( Config(..), undoKeys, redoKeys, makeBranchKeys, jumpToBranchesKeys, delBranchKeys
    , HasConfig(..)
    , Theme(..), selectedBranchColor
    , HasTheme(..)
    ) where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH
import qualified GUI.Momentu.Draw as Draw
import           GUI.Momentu.MetaKey (MetaKey)

import           Lamdu.Prelude

data Config = Config
    { _undoKeys :: [MetaKey]
    , _redoKeys :: [MetaKey]
    , _makeBranchKeys :: [MetaKey]
    , _jumpToBranchesKeys :: [MetaKey]
    , _delBranchKeys :: [MetaKey]
    } deriving (Eq, Show)
JsonTH.derivePrefixed "_" ''Config

Lens.makeLenses ''Config

newtype Theme = Theme
    { _selectedBranchColor :: Draw.Color
    } deriving (Eq, Show)
JsonTH.derivePrefixed "_" ''Theme

Lens.makeLenses ''Theme

class HasConfig env where config :: Lens' env Config
class HasTheme env where theme :: Lens' env Theme
