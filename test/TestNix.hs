-- What to do when "verify-nix-stack" fails -
--
-- Update of .nix files is requiried:
-- * Update the "rev" value
-- * Update the "sha256" value using
--   [nix-prefetch-git](https://github.com/NixOS/nixpkgs/blob/master/pkgs/build-support/fetchgit/nix-prefetch-git)
-- * Update the cabal dependencies of the dependency as necessary
--
-- Make sure to do these steps and not just make the test pass by changing the "rev" value,
-- otherwise the nix build will be broken!

module TestNix (test) where

import qualified Control.Lens as Lens
import qualified Data.Aeson.Lens as LensAeson
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Data.List (isInfixOf, isPrefixOf)
import           Data.List.Split (splitOn)
import qualified Data.Yaml as Yaml
import           System.Directory (listDirectory)

import           Test.Lamdu.Prelude

-- TODO: Consider using Cabal & nix format parsers
-- instead of quick & dirty string manipulations

test :: Test
test = testGroup "Nix" [stackDepsTest, cabalDepsTest]

stackDepsTest :: Test
stackDepsTest =
    do
        stackYaml <-
            Yaml.decodeFileEither "stack.yaml"
            >>= either (fail . Yaml.prettyPrintParseException) pure
            :: IO Yaml.Value
        let deps = stackYaml ^.. LensAeson.key "packages" . LensAeson.values . LensAeson.key "location"
        traverse_ verifyStackDep deps
        let expectedNixFiles =
                (deps ^.. traverse . LensAeson.key "git" . LensAeson._String . Lens.to Text.unpack
                    <&> packageNameFromGitUrl
                    <&> (<> ".nix")
                ) <> extraNixFiles
                & filter (`notElem` knownMissingNixFiles)
        nixFiles <- listDirectory "nix"
        assertSetEquals "Nix files" (Set.fromList expectedNixFiles) (Set.fromList nixFiles)
        & testCase "verify-nix-stack"
    where
        -- Nix files that don't reflect stack.yaml dependencies
        extraNixFiles = ["lamdu.nix"]
        -- Using our freetype2 version via nix causes GHC panics for some reason
        -- TODO: Remove this and fix the nix issues
        knownMissingNixFiles = ["freetype2.nix"]

verifyStackDep :: Yaml.Value -> IO ()
verifyStackDep dep =
    do
        git <- getKey "git"
        commit <- getKey "commit"
        let packageName = packageNameFromGitUrl git
        do
            nixFile <- readFile ("nix/" <> packageName <> ".nix")
            let nixCommit = splitOn "rev = \"" nixFile !! 1 & takeWhile (/= '"')
            when (nixCommit /= commit)
                (assertString ("Nix revision for dependency " <> packageName <> " mismatches stack"))
            -- The nix setup doesn't use the freetype2 fork.
            -- TODO: is this fine?
            & when (packageName /= "freetype2")
    where
        getKey key =
            dep ^? LensAeson.key key . LensAeson._String
            & maybe (fail ("stack.yaml dependency with no " <> show key <> "?")) (pure . Text.unpack)

packageNameFromGitUrl :: String -> String
packageNameFromGitUrl url
    | repoName == "Algorithm-W-Step-By-Step" = "AlgoW"
    | otherwise = repoName
    where
        repoName = splitOn "/" url & last

cabalDepsTest :: Test
cabalDepsTest =
    do
        cabalDeps <- readFile "Lamdu.cabal" <&> parseCabalDeps
        nixFile <- readFile "nix/lamdu.nix"
        let nixHaskellDeps =
                nixFile
                & dropWhile (/= '[') & tail & takeWhile (/= ']')
                & words
                & Set.fromList
        let nixVars =
                tail nixFile
                & takeWhile (/= '}')
                & words
                <&> takeWhile (/= ',')
                & filter (/= "")
                & Set.fromList
        checkMissing cabalDeps nixHaskellDeps "Missing nix deps"
        checkMissing nixHaskellDeps nixVars "Undeclared nix vars"
        checkMissing nixHaskellDeps cabalDeps "Unneccesary nix deps"
    & testCase "verify-nix-cabal"

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
            | buildDepsPrefix `isPrefixOf` line =
                (True, parseDeps (drop (length buildDepsPrefix) line))
            | ":" `isInfixOf` line = (False, mempty)
            | isDeps = (True, parseDeps line)
            | otherwise = (False, mempty)
        parseDeps line =
            splitOn "," line
            <&> dropWhile (== ' ') <&> takeWhile (/= ' ')
            & filter (`notElem` ["", "Lamdu"])
            & Set.fromList
        -- Specifically check for one indentation level, to avoid conditional "build-depends: ekg".
        buildDepsPrefix = "  build-depends:"
