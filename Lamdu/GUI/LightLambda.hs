{-# LANGUAGE NoImplicitPrelude, RecordWildCards #-}
module Lamdu.GUI.LightLambda
    ( withUnderline
    ) where

import           Graphics.UI.Bottle.Widgets.TextView (Underline(..))
import qualified Lamdu.Config as Config
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM

import           Lamdu.Prelude

withUnderline :: Config.LightLambda -> ExprGuiM m a -> ExprGuiM m a
withUnderline Config.LightLambda{..} =
    Underline
    { _underlineColor = lightLambdaUnderlineColor
    , _underlineWidth = realToFrac lightLambdaUnderlineWidth
    } & ExprGuiM.withLocalUnderline
