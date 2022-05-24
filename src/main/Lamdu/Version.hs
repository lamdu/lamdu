-- | Export lamdu version for --help
{-# OPTIONS -O0 #-}
module Lamdu.Version (currentVersionInfoStr) where

import           Prelude.Compat

currentVersionInfo :: String
currentVersionInfo =
    "0.8.1"

currentVersionInfoStr :: String
currentVersionInfoStr = concat ["Lamdu ", currentVersionInfo, "\n"]
