-- | The Fonts component of the Lamdu Theme
{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Config.Theme.Fonts
    ( Fonts(..)
    , base, help, literalText, autoName, literalBytes
    , binders, debugInfo
    ) where

import qualified Control.Lens as Lens
import           Data.Aeson.TH (deriveJSON)
import qualified Data.Aeson.Types as Aeson
import           Data.Char (toLower)
import           Data.List.Lens (prefixed)

import           Lamdu.Prelude

data Fonts a = Fonts
    { _base :: a
    , _help :: a
    , _literalText :: a
    , _literalBytes :: a
    , _autoName :: a
    , _binders :: a
    , _debugInfo :: a
    } deriving (Eq, Generic, Show, Functor, Foldable, Traversable)
deriveJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier
        = (Lens.ix 0 %~ toLower)
        . (^?! prefixed "_")
    }
    ''Fonts
Lens.makeLenses ''Fonts
