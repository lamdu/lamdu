{-# LANGUAGE NoImplicitPrelude, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module GUI.Momentu.Widgets.StdKeys (DirKeys(..), stdDirKeys) where

import qualified GUI.Momentu.MetaKey as MetaKey

import           Lamdu.Prelude

data DirKeys key = DirKeys
    { keysLeft, keysRight, keysUp, keysDown :: [key]
    } deriving (Functor, Foldable, Traversable)

stdDirKeys :: DirKeys MetaKey.Key
stdDirKeys = DirKeys
    { keysLeft  = [MetaKey.Key'Left,  MetaKey.Key'H]
    , keysRight = [MetaKey.Key'Right, MetaKey.Key'L]
    , keysUp    = [MetaKey.Key'Up,    MetaKey.Key'K]
    , keysDown  = [MetaKey.Key'Down,  MetaKey.Key'J]
    }
