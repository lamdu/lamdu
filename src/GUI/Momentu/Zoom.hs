{-# LANGUAGE TemplateHaskell, DerivingVia, FlexibleInstances #-}
module GUI.Momentu.Zoom
    ( Zoom, make, eventMap, getZoomFactor
    , Config(..), defaultConfig
    , makeUnscaled
    , Texts(..), view, zoom, enlarge
    , HasTexts(..)
    ) where

import qualified Control.Lens as Lens
import           Data.Aeson.TH (deriveJSON)
import qualified Data.Aeson.Types as Aeson
import           Data.IORef
import           Data.List.Lens (prefixed)
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.MetaKey (MetaKey)
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.State (Gui)
import qualified GUI.Momentu.Widget as Widget
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.UI.GLFW.Utils as GLFW.Utils

import           Lamdu.Prelude

data Texts a = Texts
    { _view :: a
    , _zoom :: a
    , _enlarge :: a
    , _shrink :: a
    }
    deriving stock (Generic, Generic1, Eq, Ord, Show, Functor, Foldable, Traversable)
    deriving Applicative via (Generically1 Texts)

Lens.makeLenses ''Texts

class HasTexts env where texts :: Lens' env (Texts Text)
deriveJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = (^?! prefixed "_")} ''Texts

instance HasTexts (Texts Text) where texts = id

data Config = Config
    { _shrinkKeys :: [MetaKey]
    , _enlargeKeys :: [MetaKey]
    , _enlargeFactor :: Double
    , _shrinkFactor :: Double
    } deriving (Eq, Show)
deriveJSON Aeson.defaultOptions
    {Aeson.fieldLabelModifier = (^?! prefixed "_")}
    ''Config

Lens.makeLenses ''Config

defaultConfig :: Config
defaultConfig =
    Config
    { _shrinkKeys = [MetaKey.cmd MetaKey.Key'Minus]
    , _enlargeKeys = [MetaKey.cmd MetaKey.Key'Equal]
    , _shrinkFactor = 1.1
    , _enlargeFactor = 1.1
    }

newtype Zoom = Zoom
    { _scaleFactorRef :: IORef Widget.R
    }

doc :: HasTexts env => env -> [Lens.ALens' (Texts Text) Text] -> E.Doc
doc env subtitleLenses = map ((env ^#) . (texts .)) subtitleLenses & E.Doc

eventMap :: HasTexts env => Zoom -> env -> Config -> Gui EventMap IO
eventMap (Zoom ref) env config =
    mconcat
    [ modifyIORef ref (* config ^. enlargeFactor)
        & E.keysEventMap (config ^. enlargeKeys)
        (doc env [view, zoom, enlarge])
    , modifyIORef ref (/ config ^. shrinkFactor)
        & E.keysEventMap (config ^. shrinkKeys)
        (doc env [view, zoom, shrink])
    ]

getZoomFactor :: Fractional a => Zoom -> IO a
getZoomFactor (Zoom ref) = readIORef ref <&> realToFrac

make :: GLFW.Window -> IO Zoom
make win =
    do
        displayScale <- GLFW.Utils.getDisplayScale win
        winSize <- GLFW.Utils.windowSize win
        let winSizeFactor = (winSize ^. _2) / 1080 & max 1
        newIORef (displayScale ^. _2 * winSizeFactor) <&> Zoom

-- | Useful mainly for tests
makeUnscaled :: Widget.R -> IO Zoom
makeUnscaled = fmap Zoom . newIORef
