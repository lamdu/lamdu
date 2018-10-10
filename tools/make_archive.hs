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

-- ldd example output:
-- 	linux-vdso.so.1 (0x00007ffc97d9f000)
-- 	libm.so.6 => /lib/x86_64-linux-gnu/libm.so.6 (0x00007f5a328be000)
-- 	libz.so.1 => /lib/x86_64-linux-gnu/libz.so.1 (0x00007f5a326a1000)
-- 	libleveldb.so.1 => /usr/lib/x86_64-linux-gnu/libleveldb.so.1 (0x00007f5a32444000)
--      ...
-- 	/lib64/ld-linux-x86-64.so.2 (0x00007f5a32c5c000)
--      ...
-- 	libXau.so.6 => /usr/lib/x86_64-linux-gnu/libXau.so.6 (0x00007f5a2e134000)
-- 	libXdmcp.so.6 => /usr/lib/x86_64-linux-gnu/libXdmcp.so.6 (0x00007f5a2df2e000)
-- 	libbsd.so.0 => /lib/x86_64-linux-gnu/libbsd.so.0 (0x00007f5a2dd19000)

-- full listing of ldd deps that have files:
--   libm
--   libz
--   libleveldb
--   libGLEW
--   libGLU
--   libGL
--   libX11
--   libXi
--   libXrandr
--   libXxf86vm
--   libXcursor
--   libXinerama
--   libpthread
--   librt
--   libutil
--   libdl
--   libgmp
--   libelf
--   libdw
--   libc
--   libsnappy
--   libstdc++
--   libgcc
--   libGLX
--   libGLdispatch
--   libxcb
--   libXext
--   libXrender
--   libXfixes
--   liblzma
--   libbz2
--   libXau
--   libXdmcp
--   libbsd

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
pkgDir = "lamdu-0.6.0"

toPackageWith :: FilePath -> FilePath -> IO ()
toPackageWith srcPath relPath =
    do
        putStrLn $ "Packaging " ++ srcPath ++ " to " ++ destPath
        Dir.createDirectoryIfMissing True (takeDirectory destPath)
        callProcess "cp" ["-aLR", srcPath, destPath]
    where
        destPath = pkgDir </> relPath

toPackage :: FilePath -> IO ()
toPackage srcPath = toPackageWith srcPath (takeFileName srcPath)

libToPackage :: FilePath -> IO ()
libToPackage srcPath =
    toPackageWith srcPath (dir </> takeFileName srcPath)
    where
        dir
            | SysInfo.os == "mingw32" = "."
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

main :: IO ()
main =
    do
        [lamduExec] <- Env.getArgs
        dependencies <- findDeps lamduExec
        bracket_ (Dir.createDirectory pkgDir) (Dir.removeDirectoryRecursive pkgDir) $ do
            toPackageWith lamduExec destPath
            toPackage "data"
            when (SysInfo.os /= "mingw32") (toPackage "tools/data/run-lamdu.sh")
            nodePath <- NodeJS.path
            toPackageWith nodePath "data/bin/node.exe"
            traverse_ libToPackage dependencies
            if SysInfo.os == "linux"
                then callProcess "tar" ["-c", "-z", "-f", "lamdu.tgz", pkgDir]
                else
                    Zip.addFilesToArchive [Zip.OptRecursive] Zip.emptyArchive [pkgDir]
                    <&> Zip.fromArchive
                    >>= LBS.writeFile "lamdu.zip"
    where
        destPath
            | SysInfo.os == "mingw32" = "lamdu.exe"
            | otherwise = "bin/lamdu"
