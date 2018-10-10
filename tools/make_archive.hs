#!/usr/bin/env runhaskell

import           Control.Exception (bracket_)
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

    -- for Windows:
    , "libwinpthread-1.dll"
    , "libstdc++-6.dll"
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

parseObjdumpOut :: String -> [FilePath]
parseObjdumpOut objdumpOut =
    lines objdumpOut >>= parseLine
    <&> ("/msys64/mingw64/bin" ++)
    & filter isInteresting
    where
        parseLine line =
            case words line of
            ["DLL", "Name:", x] -> [x]
            _ -> []

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
libToPackage srcPath = toPackageWith srcPath ("lib" </> takeFileName srcPath)

findDeps :: String -> IO [FilePath]
findDeps exec
    | SysInfo.os == "mingw32" =
        readProcess "/msys64/usr/bin/objdump" ["-p", exec] "" <&> parseObjdumpOut
    | SysInfo.os == "darwin" =
        findDylibs exec
    | otherwise =
        readProcess "ldd" [exec] "" <&> parseLddOut

main :: IO ()
main =
    do
        [lamduExec] <- Env.getArgs
        dependencies <- findDeps lamduExec
        bracket_ (Dir.createDirectory pkgDir) cleanup $ do
            toPackageWith lamduExec "bin/lamdu.exe"
            toPackage "data"
            toPackage "tools/run-lamdu.sh"
            nodePath <- NodeJS.path
            toPackageWith nodePath "data/bin/node.exe"
            mapM_ libToPackage dependencies
            finalize
    where
        (finalize, cleanup)
            | SysInfo.os == "mingw32" = (pure (), pure ())
            | SysInfo.os == "darwin" =
                ( callProcess "zip" ["-r", "lamdu.zip", pkgDir]
                , Dir.removeDirectoryRecursive pkgDir
                )
            | otherwise =
                ( callProcess "tar" ["-c", "-z", "-f", "lamdu.tgz", pkgDir]
                , Dir.removeDirectoryRecursive pkgDir
                )
