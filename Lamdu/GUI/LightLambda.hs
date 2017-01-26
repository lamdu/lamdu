{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.GUI.LightLambda
    ( withUnderline
    ) where

import           Graphics.UI.Bottle.Widgets.TextView (Underline(..))
import qualified Lamdu.Config as Config
import           Lamdu.Config (Config)
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM

import           Lamdu.Prelude

withUnderline :: Config -> ExprGuiM m a -> ExprGuiM m a
withUnderline config =
    Underline
    { _underlineColor = Config.lightLambdaUnderlineColor config
    , _underlineWidth = realToFrac (Config.underlineWidth config)
    } & ExprGuiM.withLocalUnderline
