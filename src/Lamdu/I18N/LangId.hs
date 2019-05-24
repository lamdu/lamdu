-- | Language Id
{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}

module Lamdu.I18N.LangId where

import qualified Control.Lens as Lens
import           Data.Aeson (ToJSON, FromJSON, ToJSONKey, FromJSONKey)
import           Data.Binary (Binary)

import           Lamdu.Prelude

newtype LangId = LangId Text
    deriving newtype (Eq, Ord, FromJSON, ToJSON, Binary, ToJSONKey, FromJSONKey)
    deriving stock Show

Lens.makePrisms ''LangId
