-- | Debug.Tasks data-type, to avoid Config dependency on debug code
{-# LANGUAGE TemplateHaskell, DerivingVia #-}
module Lamdu.Debug.Tasks
    ( Tasks(..), inference, sugaring, layout, database, naming
    ) where

import qualified Control.Lens as Lens
import           Data.Aeson.TH (deriveJSON)
import qualified Data.Aeson.Types as Aeson
import           Data.List.Lens (prefixed)

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
deriveJSON Aeson.defaultOptions
    {Aeson.fieldLabelModifier = (^?! prefixed "_")}
    ''Tasks
