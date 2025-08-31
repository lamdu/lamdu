{-# LANGUAGE CPP #-}
module Tests.DataFiles (test) where

import qualified Data.ByteString as BS
import qualified Data.Set as Set
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.PackageDescription.Parsec as CabalParse
import qualified System.Directory as Directory
import           System.FilePath ((</>))
#if MIN_VERSION_Cabal(3,14,0)
import qualified Distribution.Utils.Path as CabalPath
#endif

import           Test.Lamdu.Prelude

recursiveListFiles :: FilePath -> FilePath -> IO [FilePath]
recursiveListFiles prefix path =
    do
        files <- Directory.listDirectory path
        flip foldMap files $ \file ->
            do
                let childPath = path </> file
                isDir <- Directory.doesDirectoryExist childPath
                if isDir
                    then recursiveListFiles (prefix </> file) childPath
                    else pure [prefix </> file]

test :: TestTree
test =
    do
        (_warnings, Right spec) <- BS.readFile "Lamdu.cabal"
            <&> CabalParse.parseGenericPackageDescription
            <&> CabalParse.runParseResult
        actualDataFiles <- recursiveListFiles "" "data" <&> Set.fromList
        let cabalDataFiles =
                Cabal.packageDescription spec & Cabal.dataFiles
#if MIN_VERSION_Cabal(3,14,0)
                & map CabalPath.getSymbolicPath
#endif
                & Set.fromList
        let wrongDataFiles = cabalDataFiles `Set.difference` actualDataFiles
        let unlistedDataFiles = actualDataFiles `Set.difference` cabalDataFiles
        assertEqual "wrong data-files" Set.empty wrongDataFiles
        assertEqual "unlisted data-files" Set.empty unlistedDataFiles
    & testCase "data-files"
