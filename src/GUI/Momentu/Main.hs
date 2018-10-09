{-# LANGUAGE TemplateHaskell, NamedFieldPuns, GeneralizedNewtypeDeriving, StandaloneDeriving, UndecidableInstances #-}
module GUI.Momentu.Main
    ( mainLoopWidget
    , Config(..)
    , Env(..), eWindowSize, eZoom, eState
    , HasMainLoopEnv(..)
    , DebugOptions(..), defaultDebugOptions
    , PerfCounters(..)
    , Options(..), defaultOptions
    , MainAnim.wakeUp
    , quitEventMap
    ) where

import qualified Control.Lens as Lens
import           Data.IORef
import           Data.MRUMemo (memoIO)
import           Data.Property (MkProperty')
import qualified Data.Property as Property
import qualified Data.Text as Text
import           Data.Vector.Vector2 (Vector2)
import           GHC.Stack (CallStack, getCallStack, SrcLoc)
import qualified GUI.Momentu.Animation as Anim
import qualified GUI.Momentu.Draw as Draw
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Font (Font, openFont, LCDSubPixelEnabled(..))
import           GUI.Momentu.Main.Animation (PerfCounters(..))
import qualified GUI.Momentu.Main.Animation as MainAnim
import           GUI.Momentu.MetaKey (MetaKey)
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.Rect (Rect)
import qualified GUI.Momentu.Rect as Rect
import           GUI.Momentu.State (GUIState(..), Gui)
import qualified GUI.Momentu.State as State
import           GUI.Momentu.Widget (Widget, R)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Cursor as Cursor
import qualified GUI.Momentu.Widgets.EventMapHelp as EventMapHelp
import           GUI.Momentu.Zoom (Zoom)
import qualified GUI.Momentu.Zoom as Zoom
import qualified Graphics.UI.GLFW as GLFW
import           Graphics.UI.GLFW.Events as GLFWE

import           Lamdu.Prelude

data Config = Config
    { cAnim :: IO MainAnim.AnimConfig
    , cCursor :: Zoom -> IO Cursor.Config
    , cZoom :: IO Zoom.Config
    , cHelpEnv :: Maybe (Zoom -> IO EventMapHelp.Env)
    }

data DebugOptions = DebugOptions
    { fpsFont :: Zoom -> IO (Maybe Font)
    , virtualCursorColor :: IO (Maybe Draw.Color)
    , reportPerfCounters :: PerfCounters -> IO ()
    , jumpToSourceKeys :: IO [MetaKey]
    , jumpToSource :: SrcLoc -> IO ()
    }

iorefStateStorage :: Widget.Id -> IO (MkProperty' IO GUIState)
iorefStateStorage initialCursor =
    newIORef (GUIState initialCursor mempty) <&> Property.fromIORef

data Options = Options
    { tickHandler :: IO Bool
    , config :: Config
    , stateStorage :: MkProperty' IO GUIState
    , debug :: DebugOptions
    }

defaultDebugOptions :: DebugOptions
defaultDebugOptions =
    DebugOptions
    { fpsFont = const (pure Nothing)
    , virtualCursorColor = pure Nothing
    , reportPerfCounters = const (pure ())
    , jumpToSourceKeys = pure []
    , jumpToSource = \_ -> pure ()
    }

-- TODO: If moving GUI to lib,
-- include a default help font in the lib rather than get a path.
defaultOptions :: FilePath -> IO Options
defaultOptions helpFontPath =
    do
        loadHelpFont <- memoIO $ \size -> openFont LCDSubPixelDisabled size helpFontPath
        -- Note that not every app is necessarily interactive and even uses a cursor,
        -- so an empty value might be fitting.
        stateStorage_ <- iorefStateStorage (Widget.Id [])
        pure Options
            { tickHandler = pure False
            , config = Config
                { cAnim =
                    pure MainAnim.AnimConfig
                    { MainAnim.acTimePeriod = 0.11
                    , MainAnim.acRemainingRatioInPeriod = 0.2
                    }
                , cCursor =
                    \_zoom -> pure Cursor.Config
                    { Cursor.cursorColor = Draw.Color 0.5 0.5 1 0.5
                    , Cursor.decay = Nothing
                    }
                , cZoom = pure Zoom.defaultConfig
                , cHelpEnv =
                    Just $ \zoom ->
                    do
                        zoomFactor <- Zoom.getZoomFactor zoom
                        helpFont <- loadHelpFont (9 * zoomFactor)
                        EventMapHelp.defaultEnv helpFont & pure
                }
            , stateStorage = stateStorage_
            , debug = defaultDebugOptions
            }

quitEventMap :: Functor f => Gui EventMap f
quitEventMap =
    E.keysEventMap [MetaKey.cmd MetaKey.Key'Q] (E.Doc ["Quit"]) (error "Quit")

mkJumpToSourceEventMap :: Functor f => DebugOptions -> f () -> IO (Gui EventMap f)
mkJumpToSourceEventMap debug act =
    jumpToSourceKeys debug
    <&>
    \keys -> E.keysEventMap keys (E.Doc ["Debug", "Jump to source"]) act

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

jumpToTopOfCallStack :: DebugOptions -> CallStack -> IO ()
jumpToTopOfCallStack debug callStack =
    case getCallStack callStack of
    [] -> pure ()
    ((_func, topFrame):_) -> jumpToSource debug topFrame

data LookupMode = ApplyEvent | JumpToSource

lookupEvent ::
    DebugOptions -> IORef LookupMode -> IO (Maybe E.Clipboard) ->
    IORef (Maybe State.VirtualCursor) ->
    Maybe (Vector2 R -> Widget.EnterResult a) ->
    Maybe (Rect, State.VirtualCursor -> EventMap a) -> Event -> IO (Maybe a)
lookupEvent debug lookupModeRef getClipboard virtCursorRef mEnter mFocus event =
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
            mDocHandler <- E.lookup getClipboard event (mkEventMap virtCursor)
            case mDocHandler of
                Nothing -> pure Nothing
                Just docHandler ->
                    do
                        lookupMode <- readIORef lookupModeRef
                        writeIORef lookupModeRef ApplyEvent
                        case lookupMode of
                            ApplyEvent -> docHandler ^. E.dhHandler & Just & pure
                            JumpToSource ->
                                docHandler ^. E.dhFileLocation
                                & jumpToTopOfCallStack debug
                                & (Nothing <$)
    _ -> pure Nothing

virtualCursorImage :: Maybe State.VirtualCursor -> DebugOptions -> IO Anim.Frame
virtualCursorImage Nothing _ = pure mempty
virtualCursorImage (Just (State.VirtualCursor r)) debug =
    virtualCursorColor debug
    <&> \case
    Nothing -> mempty
    Just color ->
        Anim.coloredRectangle ["debug-virtual-cursor"] color
        & Anim.scale (r ^. Rect.size) & Anim.translate (r ^. Rect.topLeft)

type AddHelp =
    EventMapHelp.Env -> Widget.Size -> Widget (IO State.Update) ->
    IO (Widget (IO State.Update))

wrapMakeWidget ::
    Zoom -> AddHelp -> Options -> IORef LookupMode ->
    (Env -> IO (Gui Widget IO)) ->
    Widget.Size -> IO (Gui Widget IO)
wrapMakeWidget zoom addHelp options lookupModeRef mkWidgetUnmemod size =
    do
        s <- Property.getP stateStorage
        let env = Env
                { _eZoom = zoom
                , _eWindowSize = size
                , _eState = s
                }
        zoomEventMap <- cZoom config <&> Zoom.eventMap (env ^. eZoom)
        jumpToSourceEventMap <-
            writeIORef lookupModeRef JumpToSource
            & mkJumpToSourceEventMap debug
        let moreEvents = zoomEventMap <> jumpToSourceEventMap
        helpEnv <- cHelpEnv config ?? env ^. eZoom & sequenceA
        mkWidgetUnmemod
            env
            <&> Widget.eventMapMaker . Lens.mapped %~ (moreEvents <>)
            >>= maybe pure (addHelp ?? env ^. eWindowSize) helpEnv
    where
        Options{stateStorage, debug, config} = options

mainLoopWidget ::
    GLFW.Window ->
    (Env -> IO (Gui Widget IO)) ->
    Options ->
    IO ()
mainLoopWidget win mkWidgetUnmemod options =
    do
        addHelp <- EventMapHelp.makeToggledHelpAdder EventMapHelp.HelpNotShown
        zoom <- Zoom.make win
        lookupModeRef <- newIORef ApplyEvent
        virtCursorRef <- newIORef Nothing
        let mkW =
                wrapMakeWidget zoom addHelp options lookupModeRef mkWidgetUnmemod
                & memoIO
        mkWidgetRef <- mkW >>= newIORef
        let newWidget = mkW >>= writeIORef mkWidgetRef
        let renderWidget size =
                do
                    virtCursor <- readIORef virtCursorRef
                    vcursorimg <- virtualCursorImage virtCursor debug
                    Cursor.render
                        <$> (readIORef mkWidgetRef >>= (size &))
                        <&> _1 . Lens.mapped %~ (vcursorimg <>)
        MainAnim.mainLoop (reportPerfCounters debug) win (fpsFont zoom)
            (cAnim config) $
            \size ->
            MainAnim.Handlers
            { MainAnim.tickHandler =
                do
                    anyUpdate <- tickHandler
                    when anyUpdate newWidget
                    pure MainAnim.EventResult
                        { MainAnim.erUpdate = anyUpdate ^. Lens._Unwrapped
                        }
            , MainAnim.eventHandler = \event ->
                do
                    (_, mEnter, mFocus) <- renderWidget size
                    mWidgetRes <-
                        lookupEvent debug lookupModeRef getClipboard virtCursorRef
                        mEnter mFocus event
                    mRes <- sequenceA mWidgetRes
                    case mRes of
                        Nothing -> pure ()
                        Just res ->
                            do
                                Property.modP stateStorage (State.update res)
                                writeIORef virtCursorRef (res ^. State.uVirtualCursor . Lens._Wrapped)
                                newWidget
                    pure MainAnim.EventResult
                        { MainAnim.erUpdate = Lens.has Lens._Just mRes ^. Lens._Unwrapped
                        }
            , MainAnim.makeFrame =
                (renderWidget size <&> (^. _1))
                <*> cCursor config zoom
            }
    where
        getClipboard = GLFW.getClipboardString win <&> fmap Text.pack
        Options{stateStorage, tickHandler, debug, config} = options
        DebugOptions{fpsFont} = debug
