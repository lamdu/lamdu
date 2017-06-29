{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Lamdu.Themes
    ( themeSwitchEventMap, defaultTheme
    ) where

import           Data.IORef
import qualified Data.Text as Text
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.MetaKey (MetaKey)
import qualified Graphics.UI.Bottle.Widget as Widget
import           Lamdu.Config.Sampler (Sampler)
import qualified Lamdu.Config.Sampler as ConfigSampler
import qualified Paths.Utils as Paths
import qualified Paths_Lamdu
import qualified System.Directory as Directory
import           System.FilePath ((</>))
import qualified System.FilePath as FilePath

import           Lamdu.Prelude

themeSwitchEventMap ::
    [MetaKey] -> Sampler -> IORef Text -> Widget.EventMap (IO Widget.EventResult)
themeSwitchEventMap keys configSampler themeRef =
    do
        curTheme <- readIORef themeRef
        themeFiles <-
            Paths.get Paths_Lamdu.getDataFileName "config.json"
            <&> FilePath.takeDirectory <&> (</> "themes")
            >>= Directory.getDirectoryContents
            <&> filter ((== ".json") . FilePath.takeExtension)
            <&> map (Text.pack . FilePath.dropExtension)
        let newTheme = dropWhile (/= curTheme) themeFiles ++ themeFiles & tail & head
        writeIORef themeRef newTheme
        ConfigSampler.setTheme configSampler newTheme
    & Widget.keysEventMap keys (E.Doc ["Theme", "Switch"])

defaultTheme :: Text
defaultTheme = "default"
