{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Debug
    ( Monitors(..), inference
    , HasMonitors(..)
    , Ekg.Evaluator(..)
    , makeMonitors
    ) where

import qualified Control.Lens as Lens
import           System.Remote.Monitoring.Shim (Ekg)
import qualified System.Remote.Monitoring.Shim as Ekg

import           Lamdu.Prelude

newtype Monitors = Monitors
    { _inference :: Ekg.Evaluator
    }
Lens.makeLenses ''Monitors

class HasMonitors env where
    monitors :: Lens' env Monitors

instance HasMonitors Monitors where monitors = id

idE :: Ekg.Evaluator
idE = Ekg.Evaluator id

makeMonitors :: Maybe Ekg -> IO Monitors
makeMonitors Nothing = Monitors idE & pure
makeMonitors (Just ekg) =
    Monitors
    <$> Ekg.timedEvaluator "Inference" ekg
