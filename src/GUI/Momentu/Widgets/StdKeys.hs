module GUI.Momentu.Widgets.StdKeys
    ( DirKeys(..)
    , stdDirKeys
    , dirKey
    ) where

import           GUI.Momentu.Direction (Orientation(..), Order(..))
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.MetaKey as MetaKey

import           GUI.Momentu.Prelude

data DirKeys key = DirKeys
    { keysLeft :: [key]
    , keysRight :: [key]
    , keysUp :: [key]
    , keysDown :: [key]
    } deriving (Functor, Foldable, Traversable)

dirKey :: Dir.Layout -> Orientation -> Order -> DirKeys key -> [key]
dirKey _ Vertical Backward = keysUp
dirKey _ Vertical Forward = keysDown
dirKey Dir.LeftToRight Horizontal Backward = keysLeft
dirKey Dir.LeftToRight Horizontal Forward = keysRight
dirKey Dir.RightToLeft Horizontal Backward = keysRight
dirKey Dir.RightToLeft Horizontal Forward = keysLeft

stdDirKeys :: DirKeys MetaKey.Key
stdDirKeys = DirKeys
    { keysLeft  = [MetaKey.Key'Left]
    , keysRight = [MetaKey.Key'Right]
    , keysUp    = [MetaKey.Key'Up]
    , keysDown  = [MetaKey.Key'Down]
    }
