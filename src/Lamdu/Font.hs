{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, DeriveGeneric, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Lamdu.Font
    ( FontSize, Fonts(..)
    , LCDSubPixelEnabled(..), new
    , lfontDefault, lfontHelp, lfontLiteralText, lfontAutoName, lfontLiteralBytes, lfontBinders
    ) where

import qualified Control.Exception as E
import qualified Control.Lens as Lens
import qualified Data.Aeson.Types as Aeson
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import qualified GUI.Momentu.Draw as Draw
import qualified System.Directory as Directory

import           Lamdu.Prelude

newtype MissingFont = MissingFont FilePath deriving (Show, Typeable)
instance E.Exception MissingFont

data Fonts a = Fonts
    { fontDefault :: a
    , fontHelp :: a
    , fontLiteralText :: a
    , fontLiteralBytes :: a
    , fontAutoName :: a
    , fontBinders :: a
    } deriving (Eq, Generic, Show, Functor, Foldable, Traversable)
instance Aeson.ToJSON a => Aeson.ToJSON (Fonts a) where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions
instance Aeson.FromJSON a => Aeson.FromJSON (Fonts a)

Lens.makeLensesFor
    [ ("fontDefault"     , "lfontDefault"     )
    , ("fontHelp"        , "lfontHelp"        )
    , ("fontAutoName"    , "lfontAutoName"    )
    , ("fontBinders"     , "lfontBinders"     )
    , ("fontLiteralText" , "lfontLiteralText" )
    , ("fontLiteralBytes", "lfontLiteralBytes")
    ]
    ''Fonts

type FontSize = Float

data LCDSubPixelEnabled = LCDSubPixelEnabled | LCDSubPixelDisabled

load :: LCDSubPixelEnabled -> Float -> FilePath -> IO Draw.Font
load LCDSubPixelEnabled = Draw.openFont
load LCDSubPixelDisabled = Draw.openFontNoLCD

openFont :: LCDSubPixelEnabled -> FontSize -> FilePath -> IO Draw.Font
openFont subpixel size path =
    do
        exists <- Directory.doesFileExist path
        unless exists $ E.throwIO $ MissingFont $ path ++ " does not exist!"
        load subpixel size path

new :: LCDSubPixelEnabled -> FilePath -> Fonts (FontSize, FilePath) -> IO (Fonts Draw.Font)
new subpixel fallbackFontPath =
    traverse openEach
    where
        openEach (fontSize, fontPath) =
            open fontSize fontPath
            `E.catch` \E.SomeException{} ->
            open fontSize fallbackFontPath
        open = openFont subpixel
