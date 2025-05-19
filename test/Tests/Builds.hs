{-# LANGUAGE CPP #-}

-- | What to do when "verify-nix-stack" fails -
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

module Tests.Builds (test) where

import qualified Control.Lens as Lens
import           Data.List.Split (splitOn)

import           Test.Lamdu.Prelude

test :: TestTree
test = testGroup "Builds" [nixGhcVersionTest, ciGhcVersionTest]

nixGhcVersionTest :: TestTree
nixGhcVersionTest =
    do
        nixFile <- readFile "flake.nix"
        let nixGhcVer = read (take 2 (splitOn "ghc" nixFile ^?! Lens.ix 1))
        assertEqual "ghc version" ghcVer nixGhcVer
    & testCase "nix-ghc-version"
    where
        ghcVer :: Int
        ghcVer = __GLASGOW_HASKELL__ `div` 10 + __GLASGOW_HASKELL__ `mod` 10

ciGhcVersionTest :: TestTree
ciGhcVersionTest =
    do
        ciFile <- readFile ".github/workflows/ci.yml"
        let ghcVer = splitOn "." (splitOn "ghc: [\"" ciFile ^?! Lens.ix 1) <&> read
        case take 2 ghcVer of
            [major, minor] -> assertEqual "ghc version" (__GLASGOW_HASKELL__ :: Int) (major*100+minor)
            _ -> error "Missing dot in ghc version"
    & testCase "github-actions-ghc-version"
