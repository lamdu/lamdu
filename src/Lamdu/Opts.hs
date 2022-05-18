{-# LANGUAGE CPP, TemplateHaskell #-}
module Lamdu.Opts
    ( EditorOpts(..)
      , eoWindowMode, eoJSDebugPaths, eoWindowTitle, eoSubpixelEnabled
      , eoEkgPort, eoAnnotationsMode
    , ImportOpts(..), importPath, importImplicitPrelude
    , Command(..), _DeleteDb, _Undo, _Editor
    , CommandWithDb(..), cCommand, cLamduDB
    , Parsed(..), _ParsedCommand
    , get
    ) where

import           Control.Applicative (optional)
import qualified Control.Lens as Lens
import           Data.List.Split (splitOn)
import           Data.Vector.Vector2 (Vector2(..))
import           Data.Word (Word16)
import qualified GUI.Momentu as M
import qualified Lamdu.Annotations as Annotations
import           Lamdu.Eval.JS.Types (JSDebugPaths(..))
import           Options.Applicative ((<|>))
import qualified Options.Applicative as P

import           Lamdu.Prelude

data EditorOpts = EditorOpts
    { _eoWindowMode :: M.MonitorInfo -> M.WindowMode
    , _eoJSDebugPaths :: JSDebugPaths FilePath
    , _eoWindowTitle :: String
    , _eoSubpixelEnabled :: Bool
    , _eoEkgPort :: Maybe Word16
    , _eoAnnotationsMode :: Annotations.Mode
    }

data ImportOpts = ImportOpts
    { _importImplicitPrelude :: Bool
    , _importPath :: FilePath
    }

data Command
    = DeleteDb
    | Undo Int
    | Import ImportOpts
    | Export FilePath
    | Editor EditorOpts

data CommandWithDb = CommandWithDb
    { _cCommand :: Command
    , _cLamduDB :: Maybe FilePath
    }

newtype Parsed = ParsedCommand CommandWithDb

traverse Lens.makeLenses [''CommandWithDb, ''EditorOpts, ''ImportOpts] <&> concat
traverse Lens.makePrisms [''Command, ''Parsed] <&> concat

inverseSwitch :: P.Mod P.FlagFields Bool -> P.Parser Bool
inverseSwitch m = P.switch m <&> not

implicitPrelude :: P.Parser Bool
implicitPrelude =
    inverseSwitch
    (P.long "no-implicit-prelude" <> P.help "Do not implicitly import freshdb")

subcommands :: P.Parser Command
subcommands =
    mconcat
    [ P.info (pure DeleteDb) (P.progDesc "Irreversibly delete the lamdu database")
        & P.command "deletedb"
    , P.info
        (P.argument P.auto (P.metavar "COUNT"))
        (P.progDesc "Perform undos on the database")
        <&> Undo & P.command "undo"
    , P.info
        (ImportOpts
            <$> implicitPrelude
            <*> P.strArgument (P.metavar "IMPORTPATH"))
        (P.progDesc "Import from a given JSON file path into the database")
        <&> Import & P.command "import"
    , P.info
        (P.strArgument (P.metavar "EXPORTPATH"))
        (P.progDesc "Export the database into a JSON file")
        <&> Export & P.command "export"
    ] & P.hsubparser

jsDebugOpts :: P.Parser (JSDebugPaths FilePath)
jsDebugOpts =
    optional
    (P.option (P.eitherReader readPaths)
     (P.metavar "JSPATH[:OUTPATH[:INTERACTIVEPATH]]" <>
      P.long "jsdebug" <>
      P.help "Output the executed JS and nodejs output to files"))
    <&> fromMaybe emptyJSDebugPaths
    where
        emptyJSDebugPaths = JSDebugPaths Nothing Nothing Nothing
        readPaths str
            | length parts > 3 = Left "Too many file paths"
            | otherwise =
                Right JSDebugPaths
                { _jsDebugCodePath = parts ^? Lens.ix 0
                , _jsDebugNodeOutputPath = parts ^? Lens.ix 1
                , _jsDebugInteractivePath = parts ^? Lens.ix 2
                }
            where
                parts = splitOn ":" str

annotationsMode :: P.Parser Annotations.Mode
annotationsMode =
    P.flag' Annotations.Types
    ( P.long "types"
      <> P.help "Start Lamdu with type annotations"
    )
    <|> P.flag' Annotations.None
    ( P.long "concise"
      <> P.help "Start Lamdu without annotations"
    )
    <|> pure Annotations.Evaluation

editorOpts :: P.Parser EditorOpts
editorOpts =
    EditorOpts
    <$> windowMode
    <*> jsDebugOpts
    <*> P.option P.str
        ( P.long "windowtitle"
          <> P.value "Lamdu"
          <> P.metavar "TITLE"
          <> P.showDefault
          <> P.help "Override window title"
        )
    <*> inverseSwitch
         (P.long "disable-lcd-rendering"
          <> P.help "Disables LCD subpixel font rendering")
    <*> optional
        (P.option P.auto
            ( P.long "with-ekg"
            <> P.metavar "PORT"
            <> P.help
                ("Enable ekg monitoring of lamdu on given port"
#ifndef WITH_EKG
                <> " (DISABLED: Recompile with -fekg for ekg support)"
#endif
                )
            )
        )
    <*> annotationsMode

command :: P.Parser Command
command = Editor <$> editorOpts <|> subcommands

windowMode :: P.Parser (M.MonitorInfo -> M.WindowMode)
windowMode =
    P.flag' (const M.FullScreen)
    ( P.long "fullscreen"
      <> P.short 'f'
      <> P.help "Run Lamdu in a fullscreen window"
    )
    <|> P.option (P.maybeReader parseWindowSize) (P.long "window-size" <> P.metavar "WxH")
    <|> pure defaultSize
    where
        parseWindowSize s =
            case splitOn "x" s <&> (^? Lens._Show) of
            [Just w, Just h] -> Vector2 w h & M.Windowed & const & Just
            _ -> Nothing
        defaultSize monitorInfo
            | monitorInfo ^. M.monitorSizeMillimeters . _1 < 350 =
                monitorInfo ^. M.monitorSizeOSLogicalPixels & M.Windowed
            | otherwise =
                ( mediumWindowMillimeters
                    / (monitorInfo ^. M.monitorSizeMillimeters <&> fromIntegral)
                    <&> max (1/3)
                )
                * (monitorInfo ^. M.monitorSizeOSLogicalPixels <&> fromIntegral)
                <&> round
                & M.Windowed
        mediumWindowMillimeters :: Vector2 Double
        mediumWindowMillimeters = Vector2 233 280

commandWithDb :: P.Parser CommandWithDb
commandWithDb =
    CommandWithDb
    <$> command
    <*> optional
        (P.option P.str
            (P.metavar "PATH" <> P.long "lamduDB" <>
             P.help "Override path to lamdu DB"))

parser :: P.Parser Parsed
parser = ParsedCommand <$> commandWithDb

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
     <> P.header
        ( concat
            [ "Pressing F1 while in the Lamdu environment gives help in the"
            , " lower-right of the environment's screen. This help changes"
            , " based on what's selected."
            , " For tutorials, please see the README."
            ]
        )
    )
    & P.execParser
