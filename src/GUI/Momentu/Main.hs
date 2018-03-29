{-# LANGUAGE TemplateHaskell, NamedFieldPuns, GeneralizedNewtypeDeriving, StandaloneDeriving, UndecidableInstances #-}
module GUI.Momentu.Main
    ( mainLoopWidget
    , ExecuteInMainThread(..), M
    , Config(..)
    , Env(..), eWindowSize, eZoom, eState
    , HasMainLoopEnv(..)
    , DebugOptions(..), defaultDebugOptions
    , StateStorage(..)
    , Options(..), defaultOptions
    , MainAnim.wakeUp
    , quitEventMap
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.FastWriter (WriterT, runWriterT)
import           Data.IORef
import           Data.MRUMemo (memoIO)
import qualified Data.Text as Text
import           Data.Vector.Vector2 (Vector2)
import qualified GUI.Momentu.Animation as Anim
import qualified GUI.Momentu.Draw as Draw
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Font (Font, openFont)
import qualified GUI.Momentu.Main.Animation as MainAnim
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.Rect (Rect)
import qualified GUI.Momentu.Rect as Rect
import           GUI.Momentu.State (GUIState(..))
import qualified GUI.Momentu.State as State
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Cursor as Cursor
import qualified GUI.Momentu.Widgets.EventMapHelp as EventMapHelp
import           GUI.Momentu.Zoom (Zoom)
import qualified GUI.Momentu.Zoom as Zoom
import qualified Graphics.UI.GLFW as GLFW
import           Graphics.UI.GLFW.Events as GLFWE

import           Lamdu.Prelude

data Config = Config
    { cAnim :: MainAnim.AnimConfig
    , cCursor :: Cursor.Config
    , cZoom :: Zoom.Config
    , cHelpStyle :: EventMapHelp.Config
    }

newtype ExecuteInMainThread m = ExecuteInMainThread (m ())

deriving instance Semigroup (m ()) => Semigroup (ExecuteInMainThread m)
deriving instance Monoid (m ()) => Monoid (ExecuteInMainThread m)

type M m = WriterT (ExecuteInMainThread m) m

data DebugOptions = DebugOptions
    { fpsFont :: Zoom -> IO (Maybe Font)
    , virtualCursorColor :: IO (Maybe Draw.Color)
    }

data StateStorage = StateStorage
    { readState :: IO GUIState
    , writeState :: GUIState -> IO ()
    }

iorefStateStorage :: Widget.Id -> IO StateStorage
iorefStateStorage initialCursor =
    newIORef (GUIState initialCursor mempty)
    <&> \ref ->
    StateStorage
    { readState = readIORef ref
    , writeState = writeIORef ref
    }

data Options = Options
    { tickHandler :: IO Bool
    , getConfig :: Zoom -> IO Config
    , stateStorage :: StateStorage
    , debug :: DebugOptions
    }

defaultDebugOptions :: DebugOptions
defaultDebugOptions =
    DebugOptions
    { fpsFont = const (pure Nothing)
    , virtualCursorColor = pure Nothing
    }

-- TODO: If moving GUI to lib,
-- include a default help font in the lib rather than get a path.
defaultOptions :: FilePath -> IO Options
defaultOptions helpFontPath =
    do
        loadHelpFont <- memoIO $ \size -> openFont size helpFontPath
        -- Note that not every app is necessarily interactive and even uses a cursor,
        -- so an empty value might be fitting.
        stateStorage_ <- iorefStateStorage (Widget.Id [])
        pure Options
            { tickHandler = pure False
            , getConfig =
                \zoom -> do
                    zoomFactor <- Zoom.getZoomFactor zoom
                    helpFont <- loadHelpFont (9 * zoomFactor)
                    pure Config
                        { cAnim =
                            MainAnim.AnimConfig
                            { MainAnim.acTimePeriod = 0.11
                            , MainAnim.acRemainingRatioInPeriod = 0.2
                            }
                        , cCursor =
                            Cursor.Config
                            { Cursor.cursorColor = Draw.Color 0.5 0.5 1 0.5
                            , Cursor.decay = Nothing
                            }
                        , cZoom = Zoom.defaultConfig
                        , cHelpStyle = EventMapHelp.defaultConfig helpFont
                        }
            , stateStorage = stateStorage_
            , debug = defaultDebugOptions
            }

quitEventMap :: Functor f => EventMap (f State.Update)
quitEventMap =
    E.keysEventMap [MetaKey.cmd MetaKey.Key'Q] (E.Doc ["Quit"]) (error "Quit")

data Env = Env
    { _eZoom :: Zoom
    , _eWindowSize :: Widget.Size
    , _eState :: GUIState
    }
Lens.makeLenses ''Env
instance State.HasCursor Env
instance State.HasState Env where state = eState

class State.HasCursor env => HasMainLoopEnv env where mainLoopEnv :: Lens' env Env
instance HasMainLoopEnv Env where mainLoopEnv = id

lookupEvent ::
    IO (Maybe E.Clipboard) -> IORef (Maybe State.VirtualCursor) ->
    Maybe (Vector2 Widget.R -> Widget.EnterResult a) ->
    Maybe (Rect, State.VirtualCursor -> EventMap a) -> Event -> IO (Maybe a)
lookupEvent getClipboard virtCursorRef mEnter mFocus event =
    case (mEnter, mFocus, event) of
    (Just enter, _
        , GLFWE.EventMouseButton
          (GLFWE.MouseButtonEvent GLFW.MouseButton'1
           GLFW.MouseButtonState'Released _ mousePosF _)) ->
        enter mousePosF
        ^. Widget.enterResultEvent & Just & pure
    (_, Just (focalArea, mkEventMap), _) ->
        do
            virtCursorState <- readIORef virtCursorRef
            virtCursor <-
                case virtCursorState of
                Just x -> pure x
                Nothing ->
                    res <$ writeIORef virtCursorRef (Just res)
                    where
                        res = State.VirtualCursor focalArea
            E.lookup getClipboard event (mkEventMap virtCursor)
    _ -> pure Nothing

virtualCursorImage :: Maybe State.VirtualCursor -> DebugOptions -> IO Anim.Frame
virtualCursorImage Nothing _ = pure mempty
virtualCursorImage (Just (State.VirtualCursor r)) debug =
    virtualCursorColor debug
    <&> \case
    Nothing -> mempty
    Just color ->
        Anim.coloredRectangle ["debug-virtual-cursor"] color
        (r ^. Rect.size) & Anim.translate (r ^. Rect.topLeft)

mainLoopWidget ::
    GLFW.Window ->
    (Env -> IO (Widget (M IO State.Update))) ->
    Options ->
    IO ()
mainLoopWidget win mkWidgetUnmemod options =
    do
        addHelp <- EventMapHelp.makeToggledHelpAdder EventMapHelp.HelpNotShown
        zoom <- Zoom.make win
        virtCursorRef <- newIORef Nothing
        let mkW =
                memoIO $ \size ->
                do
                    config <- getConfig zoom
                    let zoomEventMap = cZoom config & Zoom.eventMap zoom <&> liftIO
                    s <- readState stateStorage_
                    mkWidgetUnmemod
                        Env
                        { _eZoom = zoom
                        , _eWindowSize = size
                        , _eState = s
                        }
                        <&> Widget.eventMapMaker . Lens.mapped %~ (zoomEventMap <>)
                        >>= addHelp (cHelpStyle config) size
        mkWidgetRef <- mkW >>= newIORef
        let newWidget = mkW >>= writeIORef mkWidgetRef
        let renderWidget size =
                do
                    virtCursor <- readIORef virtCursorRef
                    vcursorimg <- virtualCursorImage virtCursor debug
                    Cursor.render
                        <$> (getConfig zoom <&> cCursor)
                        <*> (readIORef mkWidgetRef >>= (size &))
                        <&> _1 <>~ vcursorimg
        MainAnim.mainLoop win (fpsFont zoom) (getConfig zoom <&> cAnim) $ \size -> MainAnim.Handlers
            { MainAnim.tickHandler =
                do
                    anyUpdate <- tickHandler
                    when anyUpdate newWidget
                    pure MainAnim.EventResult
                        { MainAnim.erUpdate = anyUpdate ^. Lens._Unwrapped
                        , MainAnim.erExecuteInMainThread = pure ()
                        }
            , MainAnim.eventHandler = \event ->
                do
                    (_, mEnter, mFocus) <- renderWidget size
                    mWidgetRes <- lookupEvent getClipboard virtCursorRef mEnter mFocus event
                    (mRes, ExecuteInMainThread act) <- sequenceA mWidgetRes & runWriterT
                    case mRes of
                        Nothing -> pure ()
                        Just res ->
                            do
                                readState stateStorage_
                                    <&> State.update res
                                    >>= writeState stateStorage_
                                writeIORef virtCursorRef (res ^. State.uVirtualCursor . Lens._Wrapped)
                                newWidget
                    pure MainAnim.EventResult
                        { MainAnim.erUpdate = Lens.has Lens._Just mRes ^. Lens._Unwrapped
                        , MainAnim.erExecuteInMainThread = act
                        }
            , MainAnim.makeFrame = renderWidget size <&> (^. _1)
            }
    where
        stateStorage_ = stateStorage options
        getClipboard = GLFW.getClipboardString win <&> fmap Text.pack
        Options{tickHandler, debug, getConfig} = options
        DebugOptions{fpsFont} = debug
