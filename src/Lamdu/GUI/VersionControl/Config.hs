{-# LANGUAGE TemplateHaskell, CPP #-}
module Lamdu.GUI.VersionControl.Config
    ( Config(..), undoKeys, redoKeys, makeBranchKeys, jumpToBranchesKeys, delBranchKeys
    , HasConfig(..)
    , Theme(..), selectedBranchColor
    , HasTheme(..)
    ) where

import qualified Control.Lens as Lens
import           Data.Aeson.TH (deriveJSON)
import           Data.Aeson.Types (defaultOptions)
import qualified Data.Aeson.Types as Aeson
import qualified GUI.Momentu.Draw as Draw
import           GUI.Momentu.MetaKey (MetaKey)
#ifndef NO_CODE
import           Data.Aeson.Utils (removePrefix)
#endif

import           Lamdu.Prelude

data Config = Config
    { _undoKeys :: [MetaKey]
    , _redoKeys :: [MetaKey]
    , _makeBranchKeys :: [MetaKey]
    , _jumpToBranchesKeys :: [MetaKey]
    , _delBranchKeys :: [MetaKey]
    } deriving (Eq, Show)
deriveJSON defaultOptions
#ifndef NO_CODE
    {Aeson.fieldLabelModifier = removePrefix "_"}
#endif
    ''Config

Lens.makeLenses ''Config

newtype Theme = Theme
    { _selectedBranchColor :: Draw.Color
    } deriving (Eq, Show)
deriveJSON defaultOptions
#ifndef NO_CODE
    {Aeson.fieldLabelModifier = removePrefix "_"}
#endif
    ''Theme

Lens.makeLenses ''Theme

class HasConfig env where config :: Lens' env Config
class HasTheme env where theme :: Lens' env Theme
