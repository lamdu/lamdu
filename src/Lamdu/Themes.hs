{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Lamdu.Themes
    ( themeSwitchEventMap, defaultTheme, getThemeFiles
    ) where

import           Data.IORef
import qualified Data.Text as Text
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.MetaKey (MetaKey)
import qualified GUI.Momentu.State as GuiState
import           Lamdu.Config.Sampler (Sampler)
import qualified Lamdu.Config.Sampler as ConfigSampler
import qualified Paths.Utils as Paths
import qualified Paths_Lamdu
import qualified System.Directory as Directory
import           System.FilePath ((</>))
import qualified System.FilePath as FilePath

import           Lamdu.Prelude

getThemeFiles :: IO [FilePath]
getThemeFiles =
    do
        themesDir <-
            Paths.get Paths_Lamdu.getDataFileName "config.json"
            <&> FilePath.takeDirectory <&> (</> "themes")
        Directory.getDirectoryContents themesDir
            <&> filter ((== ".json") . FilePath.takeExtension)
            <&> map (themesDir </>)

themeSwitchEventMap ::
    [MetaKey] -> Sampler -> IORef Text -> EventMap (IO GuiState.Update)
themeSwitchEventMap keys configSampler themeRef =
    do
        curTheme <- readIORef themeRef
        themeFiles <-
            getThemeFiles
            <&> map (Text.pack . FilePath.takeFileName . FilePath.dropExtension)
        let newTheme = dropWhile (/= curTheme) themeFiles ++ themeFiles & tail & head
        writeIORef themeRef newTheme
        ConfigSampler.setTheme configSampler newTheme
    & E.keysEventMap keys (E.Doc ["Theme", "Switch"])

defaultTheme :: Text
defaultTheme = "default"
