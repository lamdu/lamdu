{-# LANGUAGE NoImplicitPrelude, DeriveTraversable #-}
module GUI.Momentu.Widgets.StdKeys (DirKeys(..), stdDirKeys) where

import qualified GUI.Momentu.MetaKey as MetaKey

import           Lamdu.Prelude

data DirKeys key = DirKeys
    { keysLeft, keysRight, keysUp, keysDown :: [key]
    } deriving (Functor, Foldable, Traversable)

stdDirKeys :: DirKeys MetaKey.Key
stdDirKeys = DirKeys
    { keysLeft  = [MetaKey.Key'Left]
    , keysRight = [MetaKey.Key'Right]
    , keysUp    = [MetaKey.Key'Up]
    , keysDown  = [MetaKey.Key'Down]
    }
