-- | GUI choice of themes
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Lamdu.GUI.Themes
    ( switchEventMap
    ) where

import qualified Control.Lens as Lens
import           Data.Property (Property(..))
import qualified Data.Text as Text
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.State as GuiState
import qualified Lamdu.Config as Config
import           Lamdu.Config.Sampler (Sampler)
import qualified Lamdu.Config.Sampler as ConfigSampler
import qualified Lamdu.Themes as Themes
import qualified System.FilePath as FilePath

import           Lamdu.Prelude

switchEventMap ::
    (MonadReader env m, Config.HasConfig env) =>
    Sampler -> Property IO Text -> m (EventMap (IO GuiState.Update))
switchEventMap configSampler (Property curTheme setTheme) =
    Lens.view Config.config
    <&> \config ->
    let keys = Config.changeThemeKeys config
    in  do
            themeFiles <-
                Themes.getFiles
                <&> map (Text.pack . FilePath.takeFileName . FilePath.dropExtension)
            let newTheme = dropWhile (/= curTheme) themeFiles ++ themeFiles & tail & head
            setTheme newTheme
            ConfigSampler.setTheme configSampler newTheme
        & E.keysEventMap keys (E.Doc ["Theme", "Switch"])
