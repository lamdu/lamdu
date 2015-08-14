{-# LANGUAGE FlexibleContexts, RecordWildCards #-}
module Lamdu.Opts
    ( Parsed(..)
    , poShouldDeleteDB, poMFontPath, poUndoCount, poWindowSize
    , parse, get
    ) where

import Control.Lens (Lens')
import Control.Lens.Operators
import Control.Monad.Trans.State (execStateT)
import Data.Vector.Vector2 (Vector2(..))
import System.Environment (getArgs)

data Parsed = Parsed
    { _poShouldDeleteDB :: Bool
    , _poUndoCount :: Int
    , _poMFontPath :: Maybe FilePath
    , _poWindowSize :: Maybe (Vector2 Int)
    }
poShouldDeleteDB :: Lens' Parsed Bool
poShouldDeleteDB f Parsed{..} = f _poShouldDeleteDB <&> \_poShouldDeleteDB -> Parsed{..}
poMFontPath :: Lens' Parsed (Maybe FilePath)
poMFontPath f Parsed{..} = f _poMFontPath <&> \_poMFontPath -> Parsed{..}
poUndoCount :: Lens' Parsed Int
poUndoCount f Parsed{..} = f _poUndoCount <&> \_poUndoCount -> Parsed{..}
poWindowSize :: Lens' Parsed (Maybe (Vector2 Int))
poWindowSize f Parsed{..} = f _poWindowSize <&> \_poWindowSize -> Parsed{..}

parse :: [String] -> Either String Parsed
parse =
    (`execStateT` Parsed False 0 Nothing Nothing) . go
    where
        go [] = return ()
        go ("-deletedb" : args) = poShouldDeleteDB .= True >> go args
        go ("-windowsize" : wstr : hstr : args) =
            readOrFail "Invalid window width" wstr $ \w ->
            readOrFail "Invalid window height" hstr $ \h ->
            poWindowSize .= Just (Vector2 w h) >> go args
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
        failUsage msg = fail $ unlines [ msg, usage ]
        usage = "Usage: lamdu [-deletedb] [-font <filename>] [-undo <N>] [-windowsize <w> <h>]"
        readOrFail msg str k =
            case reads str of
            [(x, "")] -> k x
            _ -> failUsage $ msg ++ ": " ++ str

get :: IO (Either String Parsed)
get = getArgs <&> parse
