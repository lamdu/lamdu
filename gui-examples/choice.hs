{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TemplateHaskell #-}

module Main where

import           Control.Lens.Operators ((&), (^.))
import qualified Control.Lens as Lens
import           Control.Monad.IO.Class (MonadIO(..))
import           Data.IORef
import           Data.MRUMemo (memoIO)
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Main as MainLoop
import           Graphics.UI.Bottle.MetaKey (MetaKey(..), noMods, toModKey)
import qualified Graphics.UI.Bottle.View as View
import qualified Graphics.UI.Bottle.Widget as Widget
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widgets.Choice as Choice
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import qualified Graphics.UI.Bottle.Zoom as Zoom
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.UI.GLFW.Utils as GLFWUtils

import           Prelude.Compat

data Env = Env
    { _eCursor :: Widget.Id
    , _eAnimIdPrefix :: AnimId
    , _eTextStyle :: TextView.Style
    }
Lens.makeLenses ''Env

instance TextView.HasStyle Env where style = eTextStyle
instance View.HasAnimIdPrefix Env where animIdPrefix = eAnimIdPrefix
instance Widget.HasCursor Env where cursor = eCursor

fontPath :: FilePath
fontPath = "fonts/DejaVuSans.ttf"

main :: IO ()
main =
    do
        win <- GLFWUtils.createWindow "Hello World" Nothing (Vector2 800 400)
        cachedOpenFont <- memoIO (`Draw.openFont` fontPath)
        choiceRef <- newIORef 0
        MainLoop.defaultOptions fontPath
            >>= MainLoop.mainLoopWidget win (makeWidget choiceRef cachedOpenFont)
    & GLFWUtils.withGLFW

makeWidget ::
    MonadIO m =>
    IORef Int -> (Float -> IO Draw.Font) -> MainLoop.Env -> IO (Widget (m Widget.EventResult))
makeWidget choiceRef getFont mainEnv =
    do
        sizeFactor <- Zoom.getSizeFactor (mainEnv ^. MainLoop.eZoom)
        font <- getFont (sizeFactor * 20)
        let env =
                Env
                { _eTextStyle = TextView.whiteText font
                , _eCursor = mainEnv ^. Widget.cursor
                , _eAnimIdPrefix = []
                }
        let choices =
                traverse TextView.makeFocusableLabel
                ["Black", "Blue", "Green", "Teal", "Red", "Purple", "Brown", "Grey"]
                env
                & zip [0..]
        choice <- readIORef choiceRef
        Choice.make env (liftIO . writeIORef choiceRef)
            choices choice choiceConfig (Widget.Id [])
            & E.strongerEvents MainLoop.quitEventMap
            & return

choiceConfig :: Choice.Config
choiceConfig =
    Choice.Config
    { Choice.cwcFDConfig =
        FocusDelegator.Config
        { FocusDelegator.focusChildKeys = [MetaKey noMods GLFW.Key'Enter]
        , FocusDelegator.focusChildDoc = E.Doc ["Color", "Select"]
        , FocusDelegator.focusParentKeys = [MetaKey noMods GLFW.Key'Enter]
        , FocusDelegator.focusParentDoc = E.Doc ["Color", "Choose selected"]
        }
    , Choice.cwcOrientation = Choice.Vertical
    , Choice.cwcExpandMode = Choice.ExplicitEntry
    }
    