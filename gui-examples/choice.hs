{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TemplateHaskell, DisambiguateRecordFields #-}

module Main where

import qualified Control.Lens as Lens
import           Control.Lens.Operators ((&), (^.))
import           Control.Monad.IO.Class (MonadIO(..))
import           Data.IORef
import           Data.MRUMemo (memoIO)
import           Data.Text (Text)
import           Data.Vector.Vector2 (Vector2(..))
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.Animation (AnimId)
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/-/))
import qualified GUI.Momentu.Main as MainLoop
import           GUI.Momentu.MetaKey (MetaKey(..), noMods)
import qualified GUI.Momentu.Draw as MDraw
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Choice as Choice
import qualified GUI.Momentu.Widgets.FocusDelegator as FocusDelegator
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified GUI.Momentu.Zoom as Zoom
import qualified Graphics.DrawingCombinators as Draw
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
instance Element.HasAnimIdPrefix Env where animIdPrefix = eAnimIdPrefix
instance Widget.HasCursor Env where cursor = eCursor

fontPath :: FilePath
fontPath = "fonts/DejaVuSans.ttf"

main :: IO ()
main =
    do
        win <- GLFWUtils.createWindow "Hello World" Nothing (Vector2 800 400)
        cachedOpenFont <- memoIO (`Draw.openFont` fontPath)
        choiceRef <- newIORef "blue"
        MainLoop.defaultOptions fontPath
            >>= MainLoop.mainLoopWidget win (makeWidget choiceRef cachedOpenFont)
    & GLFWUtils.withGLFW

colors :: [(Text, Draw.Color)]
colors =
    [ ("black" , Draw.Color 0 0 0 1)
    , ("blue"  , Draw.Color 0 0 1 1)
    , ("green" , Draw.Color 0 1 0 1)
    , ("teal"  , Draw.Color 0 1 1 1)
    , ("red"   , Draw.Color 1 0 0 1)
    , ("purple", Draw.Color 1 0 1 1)
    , ("brown" , Draw.Color 0 0.5 0.5 1)
    , ("grey"  , Draw.Color 0.5 0.5 0.5 1)
    ]

makeWidget ::
    MonadIO m =>
    IORef Text -> (Float -> IO Draw.Font) -> MainLoop.Env -> IO (Widget (m Widget.EventResult))
makeWidget choiceRef getFont mainEnv =
    do
        sizeFactor <- Zoom.getZoomFactor (mainEnv ^. MainLoop.eZoom)
        font <- getFont (sizeFactor * 20)
        let env =
                Env
                { _eTextStyle = TextView.whiteText font
                , _eCursor = mainEnv ^. Widget.cursor
                , _eAnimIdPrefix = []
                }
        let makeChoice (name, _color) =
                (name, TextView.makeFocusableLabel name env ^. Align.tValue)
        choice <- readIORef choiceRef
        let choiceWidget =
                Choice.make env (liftIO . writeIORef choiceRef)
                (map makeChoice colors) choice
                choiceConfig (Widget.Id [])
        let Just color = lookup choice colors
        let box =
                MDraw.unitSquare ["square"]
                & Element.tint color
                & Element.scale 100
        choiceWidget /-/ box
            & E.strongerEvents MainLoop.quitEventMap
            & return

choiceConfig :: Choice.Config
choiceConfig =
    Choice.Config
    { cwcFDConfig =
        FocusDelegator.Config
        { focusChildKeys = [MetaKey noMods GLFW.Key'Enter]
        , focusChildDoc = E.Doc ["Color", "Select"]
        , focusParentKeys = [MetaKey noMods GLFW.Key'Enter]
        , focusParentDoc = E.Doc ["Color", "Choose selected"]
        }
    , cwcOrientation = Choice.Vertical
    , cwcExpandMode = Choice.ExplicitEntry
    }
