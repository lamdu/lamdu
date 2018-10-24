#!/usr/bin/env runhaskell

import qualified Codec.Archive.Zip as Zip
import           Control.Exception (bracket_)
import qualified Data.ByteString.Lazy as LBS
import qualified System.Directory as Dir
import qualified System.Environment as Env
import           System.FilePath ((</>), takeFileName, takeDirectory)
import qualified System.Info as SysInfo
import qualified System.NodeJS.Path as NodeJS
import           System.Process (readProcess, callProcess)

import           Lamdu.Prelude

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
        baseName = takeFileName path & break (== '.') & fst

parseLddOut :: String -> [FilePath]
parseLddOut lddOut =
    lines lddOut
    >>= parseLine
    & filter isInteresting
    where
        parseLine line =
            case words line & break (== "=>") & snd of
            [] -> []
            "=>":libPath:_ -> [libPath]
            _ -> error "unexpected break output"

parseOtoolOut :: String -> [FilePath]
parseOtoolOut otoolOut =
    lines otoolOut & tail <&> words >>= take 1
    & filter isInteresting

-- Use `otool` to recursively find macOS deps
findDylibs :: FilePath -> IO [FilePath]
findDylibs path =
    do
        deps <-
            readProcess "otool" ["-L", path] ""
            <&> parseOtoolOut
            <&> filter (/= path)
        traverse findDylibs deps <&> concat <&> (deps ++)

pkgDir :: FilePath
pkgDir
    | SysInfo.os == "darwin" = "Lamdu.app"
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
            | SysInfo.os == "darwin" = pkgDir </> "Contents"
            | otherwise = pkgDir

toPackage :: FilePath -> IO ()
toPackage srcPath = toPackageWith srcPath (takeFileName srcPath)

libToPackage :: FilePath -> IO ()
libToPackage srcPath =
    toPackageWith srcPath (dir </> filename)
    where
        filename = takeFileName srcPath
        dir
            | SysInfo.os == "mingw32" = "."
            | SysInfo.os == "darwin" = "MacOS"
            | otherwise = "lib"

findDeps :: String -> IO [FilePath]
findDeps exec
    | SysInfo.os == "mingw32" =
        [ "libwinpthread-1.dll"
        , "libstdc++-6.dll"
        , "libgcc_s_seh-1.dll"
        ]
        <&> ("/c/msys64/mingw64/bin/" ++)
        & pure
    | SysInfo.os == "darwin" =
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
        [lamduExec] <- Env.getArgs
        version <-
            readProcess lamduExec ["--version"] ""
            <&> parseLamduVersion
        dependencies <- findDeps lamduExec
        bracket_ (Dir.createDirectory pkgDir) (Dir.removeDirectoryRecursive pkgDir) $ do
            toPackageWith lamduExec destPath
            toPackageWith "data" dataDir
            nodePath <- NodeJS.path
            toPackageWith nodePath (dataDir </> "bin/node.exe")
            traverse_ libToPackage dependencies
            when (SysInfo.os == "linux") (toPackage "tools/data/run-lamdu.sh")
            when (SysInfo.os == "darwin") $ do
                toPackage "tools/data/Info.plist"
                traverse_ fixDylibPaths ("lamdu" : (dependencies <&> takeFileName))
                callProcess "sh"
                    [ "tools/data/macos_icon.sh"
                    , "tools/data/Lamdu.png"
                    , pkgDir </> "Contents" </> "Resources" </> "lamdu.icns"
                    ]
            let finalize
                    | SysInfo.os == "linux" =
                        callProcess "tar"
                        ["-c", "-z", "-f", "lamdu-" ++ version ++ ".tgz", pkgDir]
                    | SysInfo.os == "mingw32" =
                        callProcess "C:\\Program Files (x86)\\Inno Setup 5\\iscc.exe"
                        ["/FLamdu-" ++ version ++ "-Setup", "tools\\data\\lamdu.iss"]
                    | otherwise =
                        Zip.addFilesToArchive [Zip.OptRecursive] Zip.emptyArchive [pkgDir]
                        <&> Zip.fromArchive
                        >>= LBS.writeFile ("lamdu-" ++ version ++ ".zip")
            finalize
        putStrLn "Done"
    where
        destPath
            | SysInfo.os == "mingw32" = "lamdu.exe"
            | SysInfo.os == "darwin" = "MacOS/lamdu"
            | otherwise = "bin/lamdu"
        dataDir
            | SysInfo.os == "darwin" = "Resources"
            | otherwise = "data"
