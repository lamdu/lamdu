{-# LANGUAGE TemplateHaskell, DeriveTraversable, NoImplicitPrelude, NamedFieldPuns, OverloadedStrings, LambdaCase #-}
module GUI.Momentu.Main
    ( mainLoopWidget
    , Config(..), EventResult(..), M(..), m
    , Env(..), eWindowSize, eZoom
    , HasMainLoopEnv(..)
    , DebugOptions(..), defaultDebugOptions
    , CursorStorage(..)
    , Options(..), defaultOptions
    , quitEventMap
    ) where

import           Control.Applicative (liftA2)
import qualified Control.Lens as Lens
import           Control.Monad.IO.Class (MonadIO(..))
import           Data.IORef
import           Data.MRUMemo (memoIO)
import qualified Data.Text as Text
import qualified GUI.Momentu.Animation as Anim
import           GUI.Momentu.Direction (Direction)
import qualified GUI.Momentu.Direction as Direction
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Font (Font, openFont)
import qualified GUI.Momentu.Main.Animation as MainAnim
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.Rect (Rect)
import qualified GUI.Momentu.Rect as Rect
import           GUI.Momentu.Widget (Widget, VirtualCursor(..))
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.EventMapHelp as EventMapHelp
import           GUI.Momentu.Zoom (Zoom)
import qualified GUI.Momentu.Zoom as Zoom
import qualified Graphics.UI.GLFW as GLFW
import           Graphics.UI.GLFW.Events as GLFWE

import           Lamdu.Prelude

data Config = Config
    { cAnim :: MainAnim.AnimConfig
    , cCursor :: Widget.CursorConfig
    , cZoom :: Zoom.Config
    }

data EventResult a = EventResult
    { erExecuteInMainThread :: IO ()
    , erVal :: a
    } deriving (Functor, Foldable, Traversable)

instance Applicative EventResult where
    pure x = EventResult { erExecuteInMainThread = return (), erVal = x }
    EventResult am ar <*> EventResult bm br = EventResult (am >> bm) (ar br)

newtype M a = M { _m :: IO (EventResult a) }
    deriving Functor
Lens.makeLenses ''M

instance Applicative M where
    pure = M . pure . pure
    M f <*> M x = (liftA2 . liftA2) ($) f x & M

instance Monad M where
    x >>= f =
        do
            EventResult ax rx <- x ^. m
            EventResult af rf <- f rx ^. m
            EventResult (ax >> af) rf & return
        & M

instance MonadIO M where
    liftIO = M . fmap pure

data DebugOptions = DebugOptions
    { fpsFont :: Zoom -> IO (Maybe Font)
    , virtualCursorColor :: IO (Maybe Draw.Color)
    }

data CursorStorage = CursorStorage
    { readCursor :: IO Widget.Id
    , writeCursor :: Widget.Id -> IO ()
    }

iorefCursorStorage :: Widget.Id -> IO CursorStorage
iorefCursorStorage initialCursor =
    newIORef initialCursor
    <&> \ref ->
    CursorStorage
    { readCursor = readIORef ref
    , writeCursor = writeIORef ref
    }

data Options = Options
    { tickHandler :: IO Bool
    , getConfig :: IO Config
    , getHelpStyle :: Zoom -> IO EventMapHelp.Config
    , cursorStorage :: CursorStorage
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
        cursorStorage_ <- iorefCursorStorage (Widget.Id [])
        return Options
            { tickHandler = pure False
            , getConfig =
                return Config
                { cAnim =
                    MainAnim.AnimConfig
                    { MainAnim.acTimePeriod = 0.11
                    , MainAnim.acRemainingRatioInPeriod = 0.2
                    }
                , cCursor =
                    Widget.CursorConfig
                    { Widget.cursorColor = Draw.Color 0.5 0.5 1 0.5
                    }
                , cZoom = Zoom.defaultConfig
                }
            , getHelpStyle =
                \zoom -> do
                    zoomFactor <- Zoom.getZoomFactor zoom
                    helpFont <- loadHelpFont (9 * zoomFactor)
                    EventMapHelp.defaultConfig helpFont & return
            , cursorStorage = cursorStorage_
            , debug = defaultDebugOptions
            }

quitEventMap :: Functor f => Widget.EventMap (f Widget.EventResult)
quitEventMap =
    Widget.keysEventMap [MetaKey.cmd MetaKey.Key'Q] (E.Doc ["Quit"]) (error "Quit")

data Env = Env
    { _eZoom :: Zoom
    , _eWindowSize :: Widget.Size
    , _eCursor :: Widget.Id
    }
Lens.makeLenses ''Env
instance Widget.HasCursor Env where cursor = eCursor

class Widget.HasCursor env => HasMainLoopEnv env where mainLoopEnv :: Lens' env Env
instance HasMainLoopEnv Env where mainLoopEnv = id

lookupEvent ::
    IO (Maybe E.Clipboard) -> IORef (Maybe Widget.VirtualCursor) ->
    Maybe (Direction -> Widget.EnterResult a) ->
    Maybe (Rect, VirtualCursor -> Widget.EventMap a) -> Event -> IO (Maybe a)
lookupEvent getClipboard virtCursorRef mEnter mFocus event =
    case (mEnter, mFocus, event) of
    (Just enter, _
        , GLFWE.EventMouseButton
          (GLFWE.MouseButtonEvent GLFW.MouseButton'1
           GLFW.MouseButtonState'Released _ mousePosF _)) ->
        enter (Direction.Point mousePosF)
        ^. Widget.enterResultEvent & Just & pure
    (_, Just (focalArea, mkEventMap), _) ->
        do
            virtCursorState <- readIORef virtCursorRef
            virtCursor <-
                case virtCursorState of
                Just x -> return x
                Nothing ->
                    res <$ writeIORef virtCursorRef (Just res)
                    where
                        res = VirtualCursor focalArea
            E.lookup getClipboard event (mkEventMap virtCursor)
    _ -> pure Nothing

virtualCursorImage :: Maybe VirtualCursor -> DebugOptions -> IO Anim.Frame
virtualCursorImage Nothing _ = pure mempty
virtualCursorImage (Just (VirtualCursor r)) debug =
    virtualCursorColor debug
    <&> \case
    Nothing -> mempty
    Just color ->
        Anim.backgroundColor ["debug-virtual-cursor"] color
        (r ^. Rect.size) & Anim.translate (r ^. Rect.topLeft)

mainLoopWidget ::
    GLFW.Window ->
    (Env -> IO (Widget (M Widget.EventResult))) ->
    Options ->
    IO ()
mainLoopWidget win mkWidgetUnmemod options =
    do
        addHelp <- EventMapHelp.makeToggledHelpAdder EventMapHelp.HelpNotShown
        zoom <- Zoom.make win
        let mkZoomEventMap =
                do
                    zoomConfig <- getConfig <&> cZoom
                    Zoom.eventMap zoom zoomConfig <&> liftIO & return
        virtCursorRef <- newIORef Nothing
        let mkW =
                memoIO $ \size ->
                do
                    zoomEventMap <- mkZoomEventMap
                    helpStyle <- getHelpStyle zoom
                    cursor <- readCursor cursorStorage_
                    mkWidgetUnmemod
                        Env
                        { _eZoom = zoom
                        , _eWindowSize = size
                        , _eCursor = cursor
                        }
                        <&> E.strongerEvents zoomEventMap
                        >>= addHelp helpStyle size
        mkWidgetRef <- mkW >>= newIORef
        let newWidget = mkW >>= writeIORef mkWidgetRef
        let renderWidget size =
                do
                    virtCursor <- readIORef virtCursorRef
                    vcursorimg <- virtualCursorImage virtCursor debug
                    Widget.renderWithCursor
                        <$> (getConfig <&> cCursor)
                        <*> (readIORef mkWidgetRef >>= (size &))
                        <&> _1 <>~ vcursorimg
        MainAnim.mainLoop win (fpsFont zoom) (getConfig <&> cAnim) $ \size -> MainAnim.Handlers
            { MainAnim.tickHandler =
                do
                    anyUpdate <- tickHandler
                    when anyUpdate newWidget
                    return MainAnim.EventResult
                        { MainAnim.erAnimIdMapping =
                            -- TODO: nicer way to communicate whether widget
                            -- requires updating?
                            if anyUpdate then Just mempty else Nothing
                        , MainAnim.erExecuteInMainThread = return ()
                        }
            , MainAnim.eventHandler = \event ->
                do
                    (_, mEnter, mFocus) <- renderWidget size
                    mWidgetRes <- lookupEvent getClipboard virtCursorRef mEnter mFocus event
                    EventResult runInMainThread mRes <- sequenceA mWidgetRes ^. m
                    case mRes of
                        Nothing -> return ()
                        Just res ->
                            do
                                traverse_ (writeCursor cursorStorage_) (res ^. Widget.eCursor)
                                writeIORef virtCursorRef (res ^. Widget.eVirtualCursor . Lens._Wrapped)
                                newWidget
                    return MainAnim.EventResult
                        { MainAnim.erAnimIdMapping = mRes <&> (^. Widget.eAnimIdMapping)
                        , MainAnim.erExecuteInMainThread = runInMainThread
                        }
            , MainAnim.makeFrame = renderWidget size <&> (^. _1)
            }
    where
        cursorStorage_ = cursorStorage options
        getClipboard = GLFW.getClipboardString win <&> fmap Text.pack
        Options{tickHandler, debug, getConfig, getHelpStyle} = options
        DebugOptions{fpsFont} = debug
