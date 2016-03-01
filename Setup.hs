{-# OPTIONS -Wall #-}
import Control.Monad
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.Verbosity
import System.Directory
import System.FilePath ((</>))

nodeRelPath :: FilePath
nodeRelPath = "submodules/node/node"

buildNode :: Verbosity -> IO ()
buildNode verbosity =
    do
        e <- doesFileExist nodeRelPath
        unless e $
            do
                setCurrentDirectory "submodules/node"
                rawSystemExit verbosity "./configure" ["--prefix=/tmp"]
                rawSystemExit verbosity "make" ["-j4"]
                setCurrentDirectory "../.."

postInstNode :: Verbosity -> PackageDescription -> LocalBuildInfo -> IO ()
postInstNode verbosity pkgDesc localBuildInfo =
    do
        when (verbosity >= verbose) $
            putStrLn $ "Setting file executable " ++ show path
        setFileExecutable path
    where
        path =
            datadir (absoluteInstallDirs pkgDesc localBuildInfo NoCopyDest) </> nodeRelPath

main :: IO ()
main =
    defaultMainWithHooks simpleUserHooks
    { postBuild =
      \args buildFlags pkgDesc localBuildInfo ->
      do
          postBuild simpleUserHooks args buildFlags pkgDesc localBuildInfo
          let verbosity = fromFlagOrDefault normal $ buildVerbosity buildFlags
          buildNode verbosity
    , postInst = wrapInst postInst installVerbosity
    , postCopy = wrapInst postCopy copyVerbosity
    }
    where
        wrapInst f getVerbosity args flags pkgDesc localBuildInfo =
            do
                () <- f simpleUserHooks args flags pkgDesc localBuildInfo
                let verbosity = fromFlagOrDefault normal $ getVerbosity flags
                postInstNode verbosity pkgDesc localBuildInfo
