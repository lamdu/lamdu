module GUI.Momentu.Widgets.StdKeys
    ( DirKeys(..)
    , stdDirKeys
    , dirKey
    ) where

import           GUI.Momentu.Direction (Orientation(..), Order(..))
import           GUI.Momentu.Element (LayoutDir(..))
import qualified GUI.Momentu.MetaKey as MetaKey

import           Lamdu.Prelude

data DirKeys key = DirKeys
    { keysLeft :: [key]
    , keysRight :: [key]
    , keysUp :: [key]
    , keysDown :: [key]
    } deriving (Functor, Foldable, Traversable)

dirKey :: LayoutDir -> Orientation -> Order -> DirKeys key -> [key]
dirKey _ Vertical Backward = keysUp
dirKey _ Vertical Forward = keysDown
dirKey LeftToRight Horizontal Backward = keysLeft
dirKey LeftToRight Horizontal Forward = keysRight
dirKey RightToLeft Horizontal Backward = keysRight
dirKey RightToLeft Horizontal Forward = keysLeft

stdDirKeys :: DirKeys MetaKey.Key
stdDirKeys = DirKeys
    { keysLeft  = [MetaKey.Key'Left]
    , keysRight = [MetaKey.Key'Right]
    , keysUp    = [MetaKey.Key'Up]
    , keysDown  = [MetaKey.Key'Down]
    }
