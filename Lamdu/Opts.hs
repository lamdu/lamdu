{-# LANGUAGE FlexibleContexts, RecordWildCards #-}
module Lamdu.Opts
    ( Parsed(..)
    , WindowMode(..)
    , poShouldDeleteDB, poMFontPath, poUndoCount, poWindowMode
    , parse, get
    ) where

import Control.Lens (Lens')
import Control.Lens.Operators
import Control.Monad.Trans.State (execStateT)
import Data.Vector.Vector2 (Vector2(..))
import System.Environment (getArgs)

data WindowMode = VideoModeSize | WindowSize (Vector2 Int) | FullScreen

data Parsed = Parsed
    { _poShouldDeleteDB :: Bool
    , _poUndoCount :: Int
    , _poMFontPath :: Maybe FilePath
    , _poWindowMode :: WindowMode
    }
poShouldDeleteDB :: Lens' Parsed Bool
poShouldDeleteDB f Parsed{..} = f _poShouldDeleteDB <&> \_poShouldDeleteDB -> Parsed{..}
poMFontPath :: Lens' Parsed (Maybe FilePath)
poMFontPath f Parsed{..} = f _poMFontPath <&> \_poMFontPath -> Parsed{..}
poUndoCount :: Lens' Parsed Int
poUndoCount f Parsed{..} = f _poUndoCount <&> \_poUndoCount -> Parsed{..}
poWindowMode :: Lens' Parsed WindowMode
poWindowMode f Parsed{..} = f _poWindowMode <&> \_poWindowMode -> Parsed{..}

parse :: [String] -> Either String Parsed
parse =
    (`execStateT` Parsed False 0 Nothing VideoModeSize) . go
    where
        go [] = return ()
        go ("-deletedb" : args) = poShouldDeleteDB .= True >> go args
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
        go ["-font"] = failUsage "-font must be followed by a font name"
        go ("-font" : fn : args) = poMFontPath %= setPath >> go args
            where
                setPath Nothing = Just fn
                setPath Just {} = failUsage "Duplicate -font arguments"
        go ["-undo"] = failUsage "-undo must be followed by an undo count"
        go ("-undo" : countStr : args) =
            readOrFail "Invalid undo count" countStr $
            \count -> poUndoCount += count >> go args
        go (arg : _) = failUsage $ "Unexpected arg: " ++ show arg
        failUsage msg = error $ unlines [ msg, usage ]
        usage = "Usage: lamdu [-deletedb] [-font <filename>] [-undo <N>] [-windowsize <w> <h> | -fullscreen]"
        readOrFail msg str k =
            case reads str of
            [(x, "")] -> k x
            _ -> failUsage $ msg ++ ": " ++ str

get :: IO (Either String Parsed)
get = getArgs <&> parse
