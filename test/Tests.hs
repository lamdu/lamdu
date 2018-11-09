module Main where

import qualified Tests.AnimIdClash
import qualified Tests.Codec
import qualified Tests.ColorSchemes
import qualified Tests.Config
import qualified Tests.DataFiles
import qualified Tests.EventMap
import qualified Tests.Gui
import qualified Tests.JsExport
import qualified Tests.JsRtsTags
import qualified Tests.Momentu
import qualified Tests.Names
import qualified Tests.Nix
import qualified Tests.Precedence
import qualified Tests.Readme
import qualified Tests.Stdlib
import qualified Tests.Sugar
import qualified Tests.ValUtils
import qualified Tests.WidgetGlue

import           Test.Lamdu.Prelude

main :: IO ()
main =
    defaultMain tests
    where
        tests =
            [ Tests.AnimIdClash.test
            , Tests.Codec.test
            , Tests.ColorSchemes.test
            , Tests.Config.test
            , Tests.DataFiles.test
            , Tests.EventMap.test
            , Tests.Gui.test
            , Tests.JsExport.test
            , Tests.JsRtsTags.test
            , Tests.Momentu.test
            , Tests.Names.test
            , Tests.Nix.test
            , Tests.Precedence.test
            , Tests.Readme.test
            , Tests.Stdlib.test
            , Tests.Sugar.test
            , Tests.ValUtils.test
            , Tests.WidgetGlue.test
            ]
