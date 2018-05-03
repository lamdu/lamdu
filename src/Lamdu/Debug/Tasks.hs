-- | Debug.Tasks data-type, to avoid Config dependency on debug code
{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Debug.Tasks
    ( Tasks(..), inference
    ) where

import qualified Control.Lens as Lens
import           Data.Aeson.TH (deriveJSON)
import qualified Data.Aeson.Types as Aeson
import           Data.List.Lens (prefixed)

import           Lamdu.Prelude

newtype Tasks a = Tasks
    { _inference :: a
    } deriving (Eq, Show, Functor, Foldable, Traversable)
Lens.makeLenses ''Tasks
deriveJSON Aeson.defaultOptions
    {Aeson.fieldLabelModifier = (^?! prefixed "_")}
    ''Tasks

instance Applicative Tasks where
    pure = Tasks
    Tasks f <*> Tasks x = Tasks (f x)
