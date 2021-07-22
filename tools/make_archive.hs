#!/usr/bin/env runhaskell

import           Control.Exception (bracket_)
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Monad (when, unless)
import qualified Data.ByteString.Lazy as LBS
import           Data.Foldable (traverse_)
import qualified System.Directory as Dir
import qualified System.Environment as Env
import           System.FilePath ((</>), takeFileName, takeDirectory)
import qualified System.Info as SysInfo
import qualified System.NodeJS.Path as NodeJS
import           System.Process (readProcess, callProcess)

import           Prelude

interestingLibs :: [String]
interestingLibs =
    [ "libleveldb"
    , "libgmp"
    , "libelf"
    , "libdw"
    , "libsnappy"
    , "liblzma"
    , "libbz2"
    , "libbsd"
    , "libGLEW"

    -- for macOS:
    , "libtcmalloc"
    ]

isInteresting :: FilePath -> Bool
isInteresting path =
    baseName `elem` interestingLibs
    where
        -- takeBaseName removes one extension, we remove all:
        baseName = takeFileName path & takeWhile (/= '.')

parseLddOut :: String -> [FilePath]
parseLddOut lddOut =
    lines lddOut
    >>= parseLine
    & filter isInteresting
    where
        parseLine line =
            case words line & dropWhile (/= "=>") of
            [] -> []
            "=>":libPath:_ -> [libPath]
            _ -> error "unexpected break output"

otoolMinMacosVersion :: FilePath -> IO Float
otoolMinMacosVersion path =
    readProcess "otool" ["-l", path] "" <&>
    (^?! Lens.to lines . traverse .
        Lens.to words .
        Lens.filteredBy (Lens.ix 0 . (Lens.only "minos" <> Lens.only "version")) .
        Lens.ix 1 . Lens._Show)

-- Use `otool` to recursively find macOS deps
findDylibs :: FilePath -> IO [FilePath]
findDylibs path =
    do
        deps <- readProcess "otool" ["-L", path] "" <&> parseOtoolLibs <&> filter (/= path)
        traverse findDylibs deps <&> concat <&> (deps <>)
    where
        parseOtoolLibs otoolOut =
            lines otoolOut & tail <&> words >>= take 1
            & filter isInteresting

-- Slightly nicer syntax than using a sum type with case everywhere
isMacOS :: Bool
isMacOS = SysInfo.os == "darwin"

isWindows :: Bool
isWindows = SysInfo.os == "mingw32"

isLinux :: Bool
isLinux = SysInfo.os == "linux"

pkgDir :: FilePath
pkgDir
    | isMacOS = "Lamdu.app"
    | otherwise = "lamdu"

toPackageWith :: FilePath -> FilePath -> IO ()
toPackageWith srcPath relPath =
    do
        putStrLn $ "Packaging " ++ srcPath ++ " to " ++ destPath
        Dir.createDirectoryIfMissing True (takeDirectory destPath)
        callProcess "cp" ["-aLR", srcPath, destPath]
    where
        destPath = contentsDir </> relPath
        contentsDir
            | isMacOS = pkgDir </> "Contents"
            | otherwise = pkgDir

toPackage :: FilePath -> IO ()
toPackage srcPath = toPackageWith srcPath (takeFileName srcPath)

libToPackage :: FilePath -> IO ()
libToPackage srcPath =
    toPackageWith srcPath (dir </> filename)
    where
        filename = takeFileName srcPath
        dir
            | isWindows = "."
            | isMacOS = "MacOS"
            | otherwise = "lib"

findDeps :: String -> IO [FilePath]
findDeps exec
    | isWindows =
        [ "libwinpthread-1.dll"
        , "libstdc++-6.dll"
        , "libgcc_s_seh-1.dll"
        ]
        <&> ("/c/msys64/mingw64/bin/" ++)
        & pure
    | isMacOS =
        findDylibs exec
    | otherwise =
        readProcess "ldd" [exec] "" <&> parseLddOut

fixDylibPaths :: FilePath -> IO ()
fixDylibPaths targetName =
    findDylibs target >>=
    traverse_ fixDep
    where
        target = pkgDir </> "Contents" </> "MacOS" </> targetName
        fixDep dep =
            do
                callProcess "chmod" ["+w", target]
                callProcess "install_name_tool"
                    ["-change", dep, "@executable_path/" ++ takeFileName dep, target]

parseLamduVersion :: String -> String
parseLamduVersion info =
    case lines info <&> words of
    (["Lamdu", result]:_) -> result
    _ -> error "failed parsing version number"

main :: IO ()
main =
    do
        nodePath <- NodeJS.path
        [lamduExec] <- Env.getArgs
        when isMacOS $
            do
                nodeDeps <- findDeps nodePath
                when (nodeDeps /= []) (fail "nodejs not statically linked!")
                minos <- otoolMinMacosVersion lamduExec
                when (minos > 10.9) (fail "Lamdu executable only runs on new macOS versions")
        version <-
            readProcess lamduExec ["--version"] ""
            <&> parseLamduVersion
        dependencies <- findDeps lamduExec
        bracket_ (Dir.createDirectory pkgDir) (unless isMacOS (Dir.removeDirectoryRecursive pkgDir)) $
            do
                toPackageWith lamduExec destPath
                toPackageWith "data" dataDir
                toPackageWith nodePath (dataDir </> "bin/node.exe")
                traverse_ libToPackage dependencies
                when isWindows $
                    callProcess "C:\\Program Files (x86)\\Inno Setup 5\\iscc.exe" ["/Flamdu-" ++ version ++ "-win-setup", "tools\\data\\lamdu.iss"]
                when isLinux $
                    do
                        toPackage "tools/data/run-lamdu.sh"
                        callProcess "tar" ["-c", "-z", "-f", "lamdu-" ++ version ++ "-linux.tgz", pkgDir]
                when isMacOS $
                    do
                        toPackage "tools/data/Info.plist"
                        traverse_ fixDylibPaths ("lamdu" : (dependencies <&> takeFileName))
                        callProcess "sh"
                            [ "tools/data/macos_icon.sh"
                            , "data/Lamdu.png"
                            , pkgDir </> "Contents" </> "Resources" </> "lamdu.icns"
                            ]
                        -- The next steps of signing, notarization, and stapling happen in a seperate script
        putStrLn "Done"
    where
        destPath
            | isWindows = "lamdu.exe"
            | isMacOS = "MacOS/lamdu"
            | otherwise = "bin/lamdu"
        dataDir
            | isMacOS = "Resources"
            | otherwise = "data"
