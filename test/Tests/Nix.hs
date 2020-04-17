{-# LANGUAGE CPP #-}

-- What to do when "verify-nix-stack" fails -
--
-- Update of .nix files is requiried:
-- * Update the "rev" value
-- * Update the "sha256" value using
--   [nix-prefetch-git](https://github.com/NixOS/nixpkgs/blob/master/pkgs/build-support/fetchgit/nix-prefetch-git)
--   (use --fetch-submodules)
-- * Update the cabal dependencies of the dependency as necessary
--
-- Make sure to do these steps and not just make the test pass by changing the "rev" value,
-- otherwise the nix build will be broken!

module Tests.Nix (test) where

import qualified Control.Lens as Lens
import qualified Data.Aeson.Lens as LensAeson
import           Data.List (isInfixOf, isPrefixOf)
import           Data.List.Split (splitOn)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Yaml as Yaml
import           System.Directory (listDirectory)

import           Test.Lamdu.Prelude

-- TODO: Consider using Cabal & nix format parsers
-- instead of quick & dirty string manipulations

test :: Test
test = testGroup "Nix" [stackDepsTest, cabalDepsTest, ghcVersionTest]

stackDepsTest :: Test
stackDepsTest =
    do
        stackYaml <-
            Yaml.decodeFileEither "stack.yaml"
            >>= either (fail . Yaml.prettyPrintParseException) pure
            :: IO Yaml.Value
        let deps =
                stackYaml ^..
                LensAeson.key "extra-deps" . LensAeson.values . Lens.filteredBy (LensAeson.key "github")
        traverse_ verifyStackDep deps
        let expectedNixFiles =
                (deps ^.. traverse . LensAeson.key "github" . LensAeson._String . Lens.to Text.unpack
                    <&> packageNameFromGitUrl
                    <&> (<> ".nix")
                ) <> extraNixFiles
        nixFiles <- listDirectory "nix"
        assertSetEquals "Nix files" (Set.fromList expectedNixFiles) (Set.fromList nixFiles)
        & testCase "verify-stack"
    where
        extraNixFiles =
            [ "lamdu.nix" -- Top level nix file (implicit in stack)
            ]

verifyStackDep :: Yaml.Value -> IO ()
verifyStackDep dep =
    do
        github <- getKey "github"
        commit <- getKey "commit"
        let packageName = packageNameFromGitUrl github
        do
            nixFile <- readFile ("nix/" <> packageName <> ".nix")
            let nixCommit = splitOn "rev = \"" nixFile !! 1 & takeWhile (/= '"')
            when (nixCommit /= commit)
                (assertString ("Nix revision for dependency " <> packageName <> " mismatches stack"))
    where
        getKey key =
            dep ^? LensAeson.key key . LensAeson._String
            & maybe (fail ("stack.yaml dependency with no " <> show key <> "?")) (pure . Text.unpack)

packageNameFromGitUrl :: String -> String
packageNameFromGitUrl = last . splitOn "/"

cabalDepsTest :: Test
cabalDepsTest =
    do
        cabalDeps <- readFile "Lamdu.cabal" <&> parseCabalDeps
        nixFile <- readFile "nix/lamdu.nix"
        let nixFindDeps name =
                nixFile
                & splitOn (name <> " = [")
                & (^?! Lens.ix 1)
                & splitOn "];"
                & (^?! Lens.ix 0)
                & words
                & Set.fromList
        let nixHaskellDeps =
                nixFindDeps "libraryHaskellDepends"
                <> nixFindDeps "executableHaskellDepends"
                <> nixFindDeps "testHaskellDepends"
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
    & testCase "verify-cabal"

checkMissing :: Set String -> Set String -> String -> IO ()
checkMissing whole part msg
    | Set.null diff = pure ()
    | otherwise = assertString (msg <> ": " <> show diff)
    where
        diff = Set.difference whole part

parseCabalDeps :: String -> Set String
parseCabalDeps cabal =
    scanl processLine (False, mempty) (lines cabal) ^. traverse . _2
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

ghcVersionTest :: Test
ghcVersionTest =
    do
        nixFile <- readFile "default.nix"
        let nixGhcVer = read (take 2 (splitOn "ghc" nixFile ^?! Lens.ix 1))
        assertEqual "ghc version" ghcVer nixGhcVer
    & testCase "verify-ghc-version"
    where
        ghcVer :: Int
        ghcVer = __GLASGOW_HASKELL__ `div` 10 + __GLASGOW_HASKELL__ `mod` 10
