{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}
-- | The themes/ config format
module Lamdu.Config.Theme.Name
    ( Name(..)
    ) where

import           Data.Aeson.TH (deriveJSON)
import           Data.Aeson.Types (defaultOptions)
import           Data.Vector.Vector2 (Vector2)
import qualified GUI.Momentu.Draw as Draw

import           Lamdu.Prelude

data Name = Name
    { collisionSuffixTextColor :: Draw.Color
    , tagCollisionSuffixBGColor :: Draw.Color
    , textCollisionSuffixBGColor :: Draw.Color
    , collisionSuffixScaleFactor :: Vector2 Double
    , definitionColor :: Draw.Color
    , parameterColor :: Draw.Color
    , letColor :: Draw.Color
    , recordTagColor :: Draw.Color
    , caseTagColor :: Draw.Color
    , argTagColor :: Draw.Color
    } deriving (Eq, Show)
deriveJSON defaultOptions ''Name
