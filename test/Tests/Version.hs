module Tests.Version (test) where

import Data.List (isPrefixOf)

import Test.Lamdu.Prelude

test :: Test
test =
    do
        ver <- readFile "Lamdu.cabal"
            <&> last . words . head . filter ("version:" `isPrefixOf`) . lines
        readFile "tools/data/Info.plist"
            <&> head . tail . dropWhile (/= "  <key>CFBundleShortVersionString</key>") . lines
            <&> takeWhile (/= '<') . tail . dropWhile (/= '>')
            >>= assertEqual "Info.plist version" ver
        readFile "tools/data/lamdu.iss"
            <&> read . last . words . head . tail . lines
            >>= assertEqual "iss version" ver
    & testCase "verify-version"
