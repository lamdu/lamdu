{-# LANGUAGE TemplateHaskell #-}
module Lamdu.GUI.VersionControl.Config
    ( Config(..), undoKeys, redoKeys, makeBranchKeys, jumpToBranchesKeys, delBranchKeys
    , HasConfig(..)
    , Theme(..), selectedBranchColor
    , HasTheme(..)
    ) where

import qualified Control.Lens as Lens
import           Data.Aeson.TH (deriveJSON)
import qualified Data.Aeson.Types as Aeson
import           Data.List.Lens (prefixed)
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
deriveJSON Aeson.defaultOptions
    {Aeson.fieldLabelModifier = (^?! prefixed "_")}
    ''Config

Lens.makeLenses ''Config

newtype Theme = Theme
    { _selectedBranchColor :: Draw.Color
    } deriving (Eq, Show)
deriveJSON Aeson.defaultOptions
    {Aeson.fieldLabelModifier = (^?! prefixed "_")}
    ''Theme

Lens.makeLenses ''Theme

class HasConfig env where config :: Lens' env Config
class HasTheme env where theme :: Lens' env Theme
