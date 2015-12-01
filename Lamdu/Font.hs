module Lamdu.Font
    ( with
    ) where

import           Control.Monad (unless)
import qualified Graphics.DrawingCombinators as Draw
import           Lamdu.DataFile (getDataFilePath)
import qualified System.Directory as Directory

-- TODO: Move to Lamdu.Config?
defaultFontPath :: String
defaultFontPath = "fonts/DejaVuSans.ttf"

tryFont :: (Draw.Font -> IO a) -> FilePath -> IO a
tryFont action path =
    do
        exists <- Directory.doesFileExist path
        unless exists . ioError . userError $ path ++ " does not exist!"
        Draw.withFont path action

with :: FilePath -> Maybe FilePath -> (Draw.Font -> IO a) -> IO a
with startDir Nothing action = getDataFilePath startDir defaultFontPath >>= tryFont action
with _ (Just path) action = tryFont action path
