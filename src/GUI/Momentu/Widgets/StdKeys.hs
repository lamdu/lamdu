module GUI.Momentu.Widgets.StdKeys
    ( DirKeys(..)
    , stdDirKeys
    , dirKey
    ) where

import           GUI.Momentu.Direction (Orientation(..), Order(..))
import qualified GUI.Momentu.MetaKey as MetaKey

import           Lamdu.Prelude

data DirKeys key = DirKeys
    { keysLeft :: [key]
    , keysRight :: [key]
    , keysUp :: [key]
    , keysDown :: [key]
    } deriving (Functor, Foldable, Traversable)

dirKey :: Orientation -> Order -> DirKeys key -> [key]
dirKey Horizontal Backward = keysLeft
dirKey Horizontal Forward = keysRight
dirKey Vertical Backward = keysUp
dirKey Vertical Forward = keysDown

stdDirKeys :: DirKeys MetaKey.Key
stdDirKeys = DirKeys
    { keysLeft  = [MetaKey.Key'Left]
    , keysRight = [MetaKey.Key'Right]
    , keysUp    = [MetaKey.Key'Up]
    , keysDown  = [MetaKey.Key'Down]
    }
