{-# LANGUAGE NoImplicitPrelude, DeriveGeneric #-}
-- | The themes/ config format
module Lamdu.Config.Theme.Name
    ( Name(..)
    ) where

import qualified Data.Aeson.Types as Aeson
import           Data.Vector.Vector2 (Vector2)
import           GHC.Generics (Generic)
import qualified GUI.Momentu.Draw as Draw

import           Lamdu.Prelude

data Name = Name
    { collisionSuffixTextColor :: Draw.Color
    , collisionSuffixBGColor :: Draw.Color
    , collisionSuffixScaleFactor :: Vector2 Double
    , definitionColor :: Draw.Color
    , parameterColor :: Draw.Color
    , letColor :: Draw.Color
    , recordTagColor :: Draw.Color
    , caseTagColor :: Draw.Color
    , paramTagColor :: Draw.Color
    } deriving (Eq, Generic, Show)
instance Aeson.ToJSON Name where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions
instance Aeson.FromJSON Name
