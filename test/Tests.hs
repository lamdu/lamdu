module Main where

import qualified TestAnimIdClash
import qualified TestCodec
import qualified TestColorSchemes
import qualified TestConfig
import qualified TestGui
import qualified TestJsExport
import qualified TestJsRtsTags
import qualified TestMomentu
import qualified TestNames
import qualified TestNearestHoles
import qualified TestNix
import qualified TestPrecedence
import qualified TestReadme
import qualified TestStdlib
import qualified TestSugar
import qualified TestValUtils

import           Test.Lamdu.Prelude

main :: IO ()
main =
    defaultMainWithOpts tests mempty
    where
        tests =
            [ TestAnimIdClash.test
            , TestCodec.test
            , TestColorSchemes.test
            , TestConfig.test
            , TestGui.test
            , TestJsExport.test
            , TestJsRtsTags.test
            , TestMomentu.test
            , TestNames.test
            , TestNearestHoles.test
            , TestNix.test
            , TestPrecedence.test
            , TestReadme.test
            , TestStdlib.test
            , TestSugar.test
            , TestValUtils.test
            ]
