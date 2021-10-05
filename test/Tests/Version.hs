module Tests.Version (test) where

import Test.Lamdu.Prelude

test :: Test
test =
    do
        ver <-
            readFile "src/main/Lamdu/Version.hs"
            <&> read . head . tail . dropWhile (/= "#else") . lines
        readFile "Lamdu.cabal"
            <&> last . words . head . tail . lines
            >>= assertEqual "cabal version" ver
        readFile "tools/data/Info.plist"
            <&> head . tail . dropWhile (/= "  <key>CFBundleShortVersionString</key>") . lines
            <&> takeWhile (/= '<') . tail . dropWhile (/= '>')
            >>= assertEqual "Info.plist version" ver
        readFile "tools/data/lamdu.iss"
            <&> read . last . words . head . tail . lines
            >>= assertEqual "iss version" ver
    & testCase "verify-version"
