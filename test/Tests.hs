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
import qualified TestNix
import qualified TestPrecedence
import qualified TestStdlib
import qualified TestSugar
import qualified TestValUtils

import           Test.Lamdu.Prelude

main :: IO ()
main =
    defaultMainWithOpts tests mempty
    where
        tests =
            [ TestSugar.test
            , TestPrecedence.test
            , TestStdlib.test
            , TestNix.test
            , TestMomentu.test
            , TestAnimIdClash.test
            , TestCodec.test
            , TestColorSchemes.test
            , TestConfig.test
            , TestGui.test
            , TestNames.test
            , TestJsExport.test
            , TestJsRtsTags.test
            , TestValUtils.test
            ]
