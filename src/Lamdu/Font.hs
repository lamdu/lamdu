module Lamdu.Font
    ( FontSize, Fonts(..)
    , Font.LCDSubPixelEnabled(..), new
    , defaultFontFile
    , Font.height
    ) where

import qualified Control.Exception as E
import           Data.Typeable (Typeable)
import           GUI.Momentu.Font (Font)
import qualified GUI.Momentu.Font as Font
import           Lamdu.Config.Theme.Fonts (Fonts(..))
import qualified System.Directory as Directory

import           Lamdu.Prelude

type FontSize = Float

newtype MissingFont = MissingFont FilePath deriving (Show, Typeable)
instance E.Exception MissingFont

openFont :: Font.LCDSubPixelEnabled -> FontSize -> FilePath -> IO Font
openFont subpixel size path =
    do
        unless (null path) $ -- (if not debug font)
            do
                exists <- Directory.doesFileExist path
                unless exists $ E.throwIO $
                    MissingFont $ path ++ " does not exist!"
        Font.openFont subpixel size path

new :: Font.LCDSubPixelEnabled -> FilePath -> Fonts (FontSize, FilePath) -> IO (Fonts Font)
new subpixel fallbackFontPath =
    traverse openEach
    where
        openEach (fontSize, fontPath) =
            open fontSize fontPath
            `E.catch` \E.SomeException{} ->
            open fontSize fallbackFontPath
        open = openFont subpixel

defaultFontFile :: FilePath
defaultFontFile = "fonts/Purisa.ttf"
