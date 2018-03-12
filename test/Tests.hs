{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           TestInstances ()

import           Data.Data.Lens (template)
import qualified Data.Map as Map
import           GUI.Momentu.Draw (Color(..))
import qualified Lamdu.Config.Sampler as ConfigSampler
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.Data.Export.JSON as JsonCodec
import qualified Lamdu.Themes as Themes
import           System.FilePath (takeFileName)
import           Text.PrettyPrint.HughesPJClass (prettyShow)

import           Lamdu.Prelude
import           Test.HUnit
import           Test.Framework
import           Test.Framework.Providers.HUnit

jsonCodecMigrationTest :: IO ()
jsonCodecMigrationTest = JsonCodec.fileImportAll "test/old-codec-factorial.json" & void

colorSchemeTest :: IO ()
colorSchemeTest = Themes.getFiles >>= traverse_ verifyTheme

colorSV :: Color -> (Double, Double)
colorSV (Color r g b _a) =
    (if v == 0 then 0 else (v - m) / v, v)
    where
        v = maximum [r, g, b]
        m = minimum [r, g, b]

colorSat :: Color -> Double
colorSat = fst . colorSV

roundIn :: RealFrac a => a -> a -> a
roundIn unit x = fromIntegral (round (x / unit) :: Integer) * unit

verifyTheme :: FilePath -> IO ()
verifyTheme filename =
    ConfigSampler.readJson filename >>= verify
    where
        verify :: Theme -> IO ()
        verify theme
            | "retro.json" == takeFileName filename = traverse_ verifyRetroColor colors
            | Map.size saturations <= 3 = pure ()
            | otherwise =
                assertString
                ("Too many saturation options in theme " ++ filename ++ ":\n" ++
                prettyShow (Map.toList saturations))
            where
                saturations =
                    colors <&> (\c -> (roundIn 0.001 (colorSat c), [c]))
                    & Map.fromListWith (++)
                colors = theme ^.. template
        verifyRetroColor col@(Color r g b a)
            | all (`elem` [0, 0.5, 1.0]) [r, g, b]
                && elem a [0, 0.05, 0.1, 0.5, 1.0] = pure ()
            | otherwise =
                assertString ("Bad retro color in theme " ++ filename ++ ": " ++ show col)

main :: IO ()
main =
    defaultMainWithOpts
    [ testCase "json-codec-migration" jsonCodecMigrationTest
    , testCase "color-scheme" colorSchemeTest
    ] mempty
