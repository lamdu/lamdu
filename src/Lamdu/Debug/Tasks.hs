-- | Debug.Tasks data-type, to avoid Config dependency on debug code
{-# LANGUAGE TemplateHaskell, CPP #-}
module Lamdu.Debug.Tasks
    ( Tasks(..), inference
    ) where

import qualified Control.Lens as Lens
#ifndef NO_CODE
import           Data.Aeson.Utils (removePrefix)
#endif
import           Data.Aeson.TH (deriveJSON)
import qualified Data.Aeson.Types as Aeson

import           Lamdu.Prelude

newtype Tasks a = Tasks
    { _inference :: a
    } deriving (Eq, Show, Functor, Foldable, Traversable)
Lens.makeLenses ''Tasks
deriveJSON Aeson.defaultOptions
#ifndef NO_CODE
    {Aeson.fieldLabelModifier = removePrefix "_"}
#endif
    ''Tasks

instance Applicative Tasks where
    pure = Tasks
    Tasks f <*> Tasks x = Tasks (f x)
