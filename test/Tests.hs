module Main where

import qualified Tests.ElemIdClash
import qualified Tests.Builds
import qualified Tests.Codec
import qualified Tests.ColorSchemes
import qualified Tests.Config
import qualified Tests.DataFiles
import qualified Tests.EventMap
import qualified Tests.FuzzySearch
import qualified Tests.Gui
import qualified Tests.JsExport
import qualified Tests.JsRtsTags
import qualified Tests.Names
import qualified Tests.Precedence
import qualified Tests.Readme
import qualified Tests.Stdlib
import qualified Tests.Sugar
import qualified Tests.Version
import qualified Tests.Wytiwys

import           Test.Lamdu.Prelude

main :: IO ()
main =
    (:
        [ Tests.ElemIdClash.test
        , Tests.Builds.test
        , Tests.Codec.test
        , Tests.ColorSchemes.test
        , Tests.Config.test
        , Tests.DataFiles.test
        , Tests.EventMap.test
        , Tests.FuzzySearch.test
        , Tests.JsExport.test
        , Tests.JsRtsTags.test
        , Tests.Names.test
        , Tests.Precedence.test
        , Tests.Readme.test
        , Tests.Stdlib.test
        , Tests.Sugar.test
        , Tests.Version.test
        , Tests.Wytiwys.test
        ]
    )
    <$> Tests.Gui.test
    <&> testGroup "Tests"
    >>= defaultMain
