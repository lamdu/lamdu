{-# LANGUAGE TemplateHaskell #-}

module Lamdu.Data.Tag
    ( Tag(..), tagName, tagOrder
    ) where

import qualified Control.Lens as Lens
import           Data.Binary

import           Lamdu.Prelude

data Tag = Tag
    { _tagName :: Text
    , _tagOrder :: Int
    } deriving Generic
Lens.makeLenses ''Tag

instance Binary Tag
