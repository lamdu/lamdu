module Lamdu.Font
    ( get
    ) where

import           Control.Monad (unless)
import qualified Graphics.DrawingCombinators as Draw
import           Lamdu.DataFile (accessDataFile)
import qualified System.Directory as Directory

-- TODO: Move to Lamdu.Config?
defaultFontPath :: String
defaultFontPath = "fonts/DejaVuSans.ttf"

tryFont :: FilePath -> IO Draw.Font
tryFont path =
    do
        exists <- Directory.doesFileExist path
        unless exists . ioError . userError $ path ++ " does not exist!"
        Draw.openFont path

get :: FilePath -> Maybe FilePath -> IO Draw.Font
get startDir Nothing = accessDataFile startDir tryFont defaultFontPath
get _ (Just path) = tryFont path
