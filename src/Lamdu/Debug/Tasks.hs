-- | Debug.Tasks data-type, to avoid Config dependency on debug code
{-# LANGUAGE TemplateHaskell, DerivingVia #-}
module Lamdu.Debug.Tasks
    ( Tasks(..), inference, sugaring, layout, database, naming
    ) where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH

import           Lamdu.Prelude

data Tasks a = Tasks
    { _inference :: a
    , _sugaring :: a
    , _layout :: a
    , _database :: a
    , _naming :: a
    } deriving stock (Generic, Generic1, Eq, Show, Functor, Foldable, Traversable)
    deriving Applicative via Generically1 Tasks
Lens.makeLenses ''Tasks
JsonTH.derivePrefixed "_" ''Tasks
