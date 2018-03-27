{-# LANGUAGE TemplateHaskell, CPP #-}
module Lamdu.Font
    ( FontSize, Fonts(..)
    , LCDSubPixelEnabled(..), new
    , fontDefault, fontHelp, fontLiteralText, fontAutoName, fontLiteralBytes, fontBinders
    , Font.height
    ) where

import qualified Control.Exception as E
import qualified Control.Lens as Lens
import           Data.Aeson.TH (deriveJSON)
import           Data.Aeson.Types (defaultOptions)
import qualified Data.Aeson.Types as Aeson
import           Data.Typeable (Typeable)
import           GUI.Momentu.Font (Font)
import qualified GUI.Momentu.Font as Font
import qualified System.Directory as Directory
#ifndef NO_CODE
import           Data.Aeson.Utils (removePrefix, decapitalize)
#endif

import           Lamdu.Prelude

newtype MissingFont = MissingFont FilePath deriving (Show, Typeable)
instance E.Exception MissingFont

data Fonts a = Fonts
    { _fontDefault :: a
    , _fontHelp :: a
    , _fontLiteralText :: a
    , _fontLiteralBytes :: a
    , _fontAutoName :: a
    , _fontBinders :: a
    } deriving (Eq, Generic, Show, Functor, Foldable, Traversable)
deriveJSON
    defaultOptions
#ifndef NO_CODE
    {Aeson.fieldLabelModifier = decapitalize . removePrefix "_font"}
#endif
    ''Fonts
Lens.makeLenses ''Fonts

type FontSize = Float

data LCDSubPixelEnabled = LCDSubPixelEnabled | LCDSubPixelDisabled

load :: LCDSubPixelEnabled -> Float -> FilePath -> IO Font
load LCDSubPixelEnabled = Font.openFont
load LCDSubPixelDisabled = Font.openFontNoLCD

openFont :: LCDSubPixelEnabled -> FontSize -> FilePath -> IO Font
openFont subpixel size path =
    do
        exists <- Directory.doesFileExist path
        unless exists $ E.throwIO $ MissingFont $ path ++ " does not exist!"
        load subpixel size path

new :: LCDSubPixelEnabled -> FilePath -> Fonts (FontSize, FilePath) -> IO (Fonts Font)
new subpixel fallbackFontPath =
    traverse openEach
    where
        openEach (fontSize, fontPath) =
            open fontSize fontPath
            `E.catch` \E.SomeException{} ->
            open fontSize fallbackFontPath
        open = openFont subpixel
