{-# OPTIONS -fno-warn-orphans #-}
module Data.Aeson.Instances () where

import Data.Aeson.Key (Key, fromText, toText)
import Data.Binary (Binary(..))

import Lamdu.Prelude

instance Binary Key where
    get = get <&> fromText
    put = put . toText
