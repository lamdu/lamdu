-- | Debug.Tasks data-type, to avoid Config dependency on debug code
{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Debug.Tasks
    ( Tasks(..), inference, sugaring, layout, database
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
    } deriving (Eq, Show, Functor, Foldable, Traversable)
Lens.makeLenses ''Tasks
deriveJSON Aeson.defaultOptions
    {Aeson.fieldLabelModifier = (^?! prefixed "_")}
    ''Tasks

instance Applicative Tasks where
    pure x = Tasks x x x x
    Tasks f0 f1 f2 f3 <*> Tasks x0 x1 x2 x3 = Tasks (f0 x0) (f1 x1) (f2 x2) (f3 x3)
