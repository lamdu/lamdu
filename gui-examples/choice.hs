{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, DisambiguateRecordFields #-}

module Main where

import qualified Control.Lens as Lens
import           Control.Lens.Operators ((&), (^.))
import           Control.Monad.IO.Class (MonadIO(..))
import           Data.Has (Has)
import           Data.IORef
import           Data.MRUMemo (memoIO)
import           Data.Text (Text)
import qualified GUI.Momentu as M
import qualified GUI.Momentu.Widgets.Choice as Choice
import qualified GUI.Momentu.Widgets.FocusDelegator as FocusDelegator
import qualified GUI.Momentu.Widgets.TextView as TextView

import           Prelude.Compat

data Env = Env
    { _eCursor :: M.WidgetId
    , _eAnimIdPrefix :: M.AnimId
    , _eTextStyle :: TextView.Style
    }
Lens.makeLenses ''Env

instance Has TextView.Style Env where style = eTextStyle
instance M.HasAnimIdPrefix Env where animIdPrefix = eAnimIdPrefix
instance M.HasCursor Env where cursor = eCursor

fontPath :: FilePath
fontPath = "fonts/DejaVuSans.ttf"

main :: IO ()
main =
    do
        win <- M.createWindow "Hello World" Nothing (M.Vector2 400 800)
        cachedOpenFont <- memoIO (`M.openFont` fontPath)
        choiceRef <- newIORef "blue"
        M.defaultOptions fontPath
            >>= M.mainLoopWidget win (makeWidget choiceRef cachedOpenFont)
    & M.withGLFW

colors :: [(Text, M.Color)]
colors =
    [ ("black" , M.Color 0 0 0 1)
    , ("blue"  , M.Color 0 0 1 1)
    , ("green" , M.Color 0 1 0 1)
    , ("teal"  , M.Color 0 1 1 1)
    , ("red"   , M.Color 1 0 0 1)
    , ("purple", M.Color 1 0 1 1)
    , ("brown" , M.Color 0.8 0.5 0 1)
    , ("grey"  , M.Color 0.5 0.5 0.5 1)
    ]

makeWidget ::
    MonadIO m =>
    IORef Text -> (Float -> IO M.Font) -> M.MainLoopEnv ->
    IO (M.Widget (m M.EventResult))
makeWidget choiceRef getFont mainEnv =
    do
        sizeFactor <- M.getZoomFactor (mainEnv ^. M.eZoom)
        font <- getFont (sizeFactor * 20)
        let env =
                Env
                { _eTextStyle = TextView.whiteText font
                , _eCursor = mainEnv ^. M.cursor
                , _eAnimIdPrefix = []
                }
        let makeChoice (name, _color) =
                (name, Label.makeFocusable name env ^. M.tValue)
        choice <- readIORef choiceRef
        let choiceWidget =
                Choice.make env (liftIO . writeIORef choiceRef)
                (map makeChoice colors) choice
                (Choice.defaultConfig "Color") (M.WidgetId [])
        let Just color = lookup choice colors
        let box =
                M.unitSquare ["square"]
                & M.tint color
                & M.scale 100
        box `M.above` choiceWidget
            & M.weakerEvents M.quitEventMap
            & pure
