import           Control.Exception (bracket_)
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Monad (when, unless, filterM)
import           Data.Foldable (traverse_)
import qualified System.Directory as Dir
import qualified System.Environment as Env
import           System.FilePath ((</>), takeFileName, takeDirectory)
import qualified System.Info as SysInfo
import           System.Process (readProcess, callProcess)

import           Prelude

interestingLibs :: [String]
interestingLibs =
    [ "libGLEW"
    , "libGLU"
    , "libXcursor"
    , "libXi"
    , "libXinerama"
    , "libXrandr"
    , "libXrender"
    , "libbsd"
    , "libbz2"
    , "libcares"
    , "libcrypto"
    , "libdw"
    , "libelf"
    , "libgflags"
    , "libgmp"
    , "libicudata"
    , "libicui18n"
    , "libicuuc"
    , "liblzma"
    , "libnghttp2"
    , "libnode"
    , "librocksdb"
    , "libsnappy"
    , "libssl"
    , "libuv"
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
        deps <-
            readProcess "otool" ["-L", path] ""
            <&> lines <&> tail <&> map words <&> (>>= take 1)
            <&> filter (/= path)
            >>= filterM Dir.doesPathExist
        traverse findDylibs deps <&> concat <&> (deps <>)

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
        , "librocksdb.dll"
        , "libbz2-1.dll"
        , "liblz4.dll"
        , "zlib1.dll"
        , "libzstd.dll"
        ] <&> ("/mingw64/bin" </>)
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

whichCmd :: String
whichCmd
    | isWindows = "where"
    | otherwise = "which"

main :: IO ()
main =
    do
        [lamduExec] <- Env.getArgs
        nodePath <- readProcess whichCmd ["node"] "" <&> takeWhile (`notElem` "\r\n")
        nodeDeps <- findDeps nodePath
        when isMacOS $
            do
                unless (null nodeDeps) $ fail "nodejs not statically linked!"
                minos <- otoolMinMacosVersion lamduExec
                when (minos > 10.9) (fail "Lamdu executable only runs on new macOS versions")
        version <- readProcess lamduExec ["--version"] "" <&> parseLamduVersion
        lamduDeps <- findDeps lamduExec
        let allDeps = nodeDeps <> lamduDeps
        bracket_ (Dir.createDirectory pkgDir) (unless isMacOS (Dir.removeDirectoryRecursive pkgDir)) $
            do
                toPackageWith lamduExec destPath
                toPackageWith "data" dataDir
                toPackageWith nodePath (dataDir </> "bin/node.exe")
                traverse_ libToPackage allDeps
                when isWindows $
                    callProcess "iscc.exe" ["/Flamdu-" ++ version ++ "-win-setup", "tools\\data\\lamdu.iss"]
                when isLinux $
                    do
                        toPackage "tools/data/run-lamdu.sh"
                        callProcess "tar" ["-c", "-z", "-f", "lamdu-" ++ version ++ "-linux.tgz", pkgDir]
                when isMacOS $
                    do
                        toPackage "tools/data/Info.plist"
                        traverse_ fixDylibPaths ("lamdu" : (allDeps <&> takeFileName))
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
