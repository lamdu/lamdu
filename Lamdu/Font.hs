module Lamdu.Font
    ( with
    ) where

import           Control.Monad (unless)
import qualified Graphics.DrawingCombinators as Draw
import           Lamdu.DataFile (accessDataFile)
import qualified System.Directory as Directory

-- TODO: Move to Lamdu.Config?
defaultFontPath :: String
defaultFontPath = "fonts/DejaVuSans.ttf"

tryFont :: FilePath -> (Draw.Font -> IO a) -> IO a
tryFont path action =
    do
        exists <- Directory.doesFileExist path
        unless exists . ioError . userError $ path ++ " does not exist!"
        Draw.withFont path action

with :: FilePath -> Maybe FilePath -> (Draw.Font -> IO a) -> IO a
with startDir Nothing action = accessDataFile startDir (`tryFont` action) defaultFontPath
with _ (Just path) action = tryFont path action
