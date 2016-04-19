{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, FlexibleContexts, RecordWildCards #-}
module Lamdu.Opts
    ( Parsed(..)
    , WindowMode(..)
    , poShouldDeleteDB, poUndoCount, poWindowMode, poCopyJSOutputPath
    , parse, get
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Monad.Trans.State (execStateT)
import           Data.Vector.Vector2 (Vector2(..))
import           System.Environment (getArgs)

import           Prelude.Compat

data WindowMode = VideoModeSize | WindowSize (Vector2 Int) | FullScreen

data Parsed = Parsed
    { _poShouldDeleteDB :: Bool
    , _poUndoCount :: Int
    , _poWindowMode :: WindowMode
    , _poCopyJSOutputPath :: Maybe FilePath
    }
Lens.makeLenses ''Parsed

parse :: [String] -> Either String Parsed
parse =
    (`execStateT` Parsed False 0 VideoModeSize Nothing) . go
    where
        go [] = return ()
        go ("-deletedb" : args) = poShouldDeleteDB .= True >> go args
        go ("-copyjsoutput" : path : args) = poCopyJSOutputPath ?= path >> go args
        go ("-windowsize" : wstr : hstr : args) =
            readOrFail "Invalid window width" wstr $ \w ->
            readOrFail "Invalid window height" hstr $ \h ->
            let set VideoModeSize = WindowSize (Vector2 w h)
                set _ = failUsage "Duplicate -windowsize / -fullscreen options specified"
            in poWindowMode %= set >> go args
        go ("-fullscreen" : args) =
           let set VideoModeSize = FullScreen
               set _ = failUsage "Duplicate -windowsize / -fullscreen options specified"
           in poWindowMode %= set >> go args
        go ["-undo"] = failUsage "-undo must be followed by an undo count"
        go ("-undo" : countStr : args) =
            readOrFail "Invalid undo count" countStr $
            \count -> poUndoCount += count >> go args
        go (arg : _) = failUsage $ "Unexpected arg: " ++ show arg
        failUsage msg = error $ unlines [ msg, usage ]
        usage = "Usage: lamdu [-copyjsoutput <filename>] [-deletedb] [-font <filename>] [-undo <N>] [-windowsize <w> <h> | -fullscreen]"
        readOrFail msg str k =
            case reads str of
            [(x, "")] -> k x
            _ -> failUsage $ msg ++ ": " ++ str

get :: IO (Either String Parsed)
get = getArgs <&> parse
