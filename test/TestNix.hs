module TestNix (test) where

import qualified Control.Lens as Lens
import qualified Data.Set as Set
import           Data.List (isInfixOf, isPrefixOf)
import           Data.List.Split (splitOn)
import           Test.Framework (Test)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit (assertString)

import           Lamdu.Prelude

-- TODO: Consider using Cabal & nix format parsers
-- instead of quick & dirty string manipulations

test :: Test
test =
    do
        cabalDeps <- readFile "Lamdu.cabal" <&> parseCabalDeps
        nixHaskellDeps <-
            readFile "nix/lamdu.nix"
            <&> dropWhile (/= '[') <&> tail <&> takeWhile (/= ']')
            <&> words
            <&> Set.fromList
        checkMissing cabalDeps nixHaskellDeps "Missing nix deps"
        checkMissing nixHaskellDeps cabalDeps "Unneccesary nix deps"
    & testCase "verify-nix-packaging"

checkMissing :: Set String -> Set String -> String -> IO ()
checkMissing whole part msg
    | Set.null diff = pure ()
    | otherwise = assertString (msg <> ": " <> show diff)
    where
        diff = Set.difference whole part

parseCabalDeps :: String -> Set String
parseCabalDeps cabal =
    scanl processLine (False, mempty) (lines cabal) ^. traverse . Lens._2
    where
        processLine (isDeps, _) line
            | isPrefixOf buildDepsPrefix line =
                (True, parseDeps (drop (length buildDepsPrefix) line))
            | isInfixOf ":" line = (False, mempty)
            | isDeps = (True, parseDeps line)
            | otherwise = (False, mempty)
        parseDeps line =
            splitOn "," line
            <&> dropWhile (== ' ') <&> takeWhile (/= ' ')
            & filter (`notElem` ["", "Lamdu"])
            & Set.fromList
        -- Specifically check for one indentation level, to avoid conditional "build-depends: ekg".
        buildDepsPrefix = "  build-depends:"
