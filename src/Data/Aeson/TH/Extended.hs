-- | Utility functions for nicer deriveJSON
module Data.Aeson.TH.Extended
    ( derivePrefixed
    , module Data.Aeson.TH
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Data.Aeson.TH
import qualified Data.Aeson.Types as Aeson
import           Data.Char (toLower)
import           Data.List.Lens (prefixed)
import           Language.Haskell.TH.Syntax (Name, Q, Dec)

import           Prelude.Compat

derivePrefixed :: String -> Name -> Q [Dec]
derivePrefixed prefix =
    deriveJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier = (Lens.ix 0 %~ toLower) . (^?! prefixed prefix)
    }
