{-# OPTIONS -Wall #-}
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Verbosity
import Distribution.Simple.Utils
import System.Directory

buildNode :: Verbosity -> IO ()
buildNode verbosity =
    do
        setCurrentDirectory "submodules/node"
        rawSystemExit verbosity "./configure" ["--prefix=/tmp"]
        rawSystemExit verbosity "make" ["-j4"]
        setCurrentDirectory "../.."

main :: IO ()
main = do
    e <- doesFileExist "submodules/node/node"
    if e
        then defaultMain
        else defaultMainWithHooks simpleUserHooks
            { postBuild =
              \args buildFlags pkgDesc localBuildInfo ->
              do
                  postBuild simpleUserHooks args buildFlags pkgDesc localBuildInfo
                  let verbosity = fromFlagOrDefault normal $ buildVerbosity buildFlags
                  buildNode verbosity
            }
