{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, FlexibleContexts, RecordWildCards #-}
module Lamdu.Opts
    ( EditorOpts(..), eoWindowMode, eoCopyJSOutputPath, eoWindowTitle, eoSubpixelEnabled
    , Command(..), _DeleteDb, _Undo, _Editor
    , Parsed(..), pCommand, pLamduDB
    , WindowMode(..), _VideoModeSize, _FullScreen
    , get
    ) where

import           Control.Applicative (optional)
import qualified Control.Lens as Lens
import           Options.Applicative ((<|>))
import qualified Options.Applicative as P

import           Lamdu.Prelude

data WindowMode = VideoModeSize | FullScreen

data EditorOpts = EditorOpts
    { _eoWindowMode :: WindowMode
    , _eoCopyJSOutputPath :: Maybe FilePath
    , _eoWindowTitle :: String
    , _eoSubpixelEnabled :: Bool
    }

data Command
    = DeleteDb
    | Undo Int
    | Import FilePath
    | Export FilePath
    | Editor EditorOpts

data Parsed = Parsed
    { _pCommand :: Command
    , _pLamduDB :: Maybe FilePath
    }

Lens.makeLenses ''EditorOpts
Lens.makePrisms ''Command
Lens.makePrisms ''WindowMode
Lens.makeLenses ''Parsed

subcommands :: P.Parser Command
subcommands =
    mconcat
    [ P.command "deletedb"
      (P.info (pure DeleteDb) (P.progDesc "Irreversibly delete the lamdu database"))
    , P.command "undo"
      (P.info
       (P.argument (Undo <$> P.auto) (P.metavar "COUNT"))
       (P.progDesc "Perform undos on the database"))
    , P.command "import"
      (P.info
       (P.argument (Import <$> P.str) (P.metavar "IMPORTPATH"))
       (P.progDesc "Import from a given JSON file path into the database"))
    , P.command "export"
      (P.info
       (P.argument (Export <$> P.str) (P.metavar "EXPORTPATH"))
       (P.progDesc "Export the database into a JSON file")
      )
    ] & P.hsubparser

maybePath :: P.Mod P.OptionFields String -> P.Parser (Maybe FilePath)
maybePath m = optional (P.option P.str (P.metavar "PATH" <> m))

editorOpts :: P.Parser EditorOpts
editorOpts =
    EditorOpts
    <$> windowMode
    <*> maybePath
        (P.long "copyjsoutput" <>
         P.help "Output the internal executed JS to a file")
    <*> P.option P.str
        ( P.long "windowtitle"
          <> P.value "Lamdu"
          <> P.metavar "TITLE"
          <> P.showDefault
          <> P.help "Override window title"
        )
    <*> P.flag True False
        (P.long "disable-lcd-rendering"
         <> P.help "Disables LCD subpixel font rendering")

command :: P.Parser Command
command = (Editor <$> editorOpts) <|> subcommands

windowMode :: P.Parser WindowMode
windowMode =
    P.flag' FullScreen
    ( P.long "fullscreen"
      <> P.short 'f'
      <> P.help "Run Lamdu in a fullscreen window"
    )
    <|> pure VideoModeSize

parser :: P.Parser Parsed
parser =
    Parsed
    <$> command
    <*> maybePath (P.long "lamduDB" <> P.help "Override path to lamdu DB")

get :: IO Parsed
get =
    P.info
    (P.helper <*> parser)
    (
        P.progDesc "Lamdu - The Next Generation IDE"

{-
   Here, I use "<>" to insert a header. It goes on top of the help text
   displayed when someone types "lamdu --help".
   We're using the "Options.Applicative" module for command-line parameters and
   help text. Unfortunately, it strips out any newlines or leading space I try
   to give this header. So it doesn't print out too pretty. I tried
   chaining multiple headers with "<>", but it only keeps the last one.
 -}
     <> P.header "Pressing F1 while in the Lamdu environment gives help in the\
                 \ lower-right of the environment's screen. This help changes\
                 \ based on what's selected.\
                 \ For tutorials, please see the README."
    )
    & P.execParser
