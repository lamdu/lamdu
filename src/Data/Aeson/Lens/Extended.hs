module Data.Aeson.Lens.Extended
    ( _Key
    ) where

import qualified Control.Lens as Lens
import qualified Data.Aeson.Key as Aeson.Key
import           Data.Text (Text)

_Key :: Lens.Iso' Aeson.Key.Key Text
_Key = Lens.iso Aeson.Key.toText Aeson.Key.fromText
