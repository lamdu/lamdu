{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.GUI.LightLambda
    ( withUnderline
    ) where

import           GUI.Momentu.Widgets.TextView (Underline(..))
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM

import           Lamdu.Prelude

withUnderline :: Monad m => Theme -> ExprGuiM m a -> ExprGuiM m a
withUnderline theme =
    Underline
    { _underlineColor = Theme.lightLambdaUnderlineColor (Theme.codeForegroundColors theme)
    , _underlineWidth = realToFrac (Theme.underlineWidth theme)
    } & ExprGuiM.withLocalUnderline
