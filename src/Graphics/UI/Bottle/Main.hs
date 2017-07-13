{-# LANGUAGE TemplateHaskell, DeriveFunctor, DeriveFoldable, DeriveTraversable, NoImplicitPrelude, NamedFieldPuns, OverloadedStrings #-}
module Graphics.UI.Bottle.Main
    ( mainLoopWidget
    , Config(..), EventResult(..), M(..), m, Env(..), eWindowSize, eZoom
    , Options(..), defaultOptions
    , quitEventMap
    ) where

import           Control.Applicative (liftA2)
import qualified Control.Lens as Lens
import           Control.Monad.IO.Class (MonadIO(..))
import           Data.IORef
import           Data.MRUMemo (memoIO)
import qualified Data.Text as Text
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Direction as Direction
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Main.Animation as MainAnim
import qualified Graphics.UI.Bottle.MetaKey as MetaKey
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.EventMapHelp as EventMapHelp
import           Graphics.UI.Bottle.Zoom (Zoom)
import qualified Graphics.UI.Bottle.Zoom as Zoom
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

data Options = Options
    { tickHandler :: IO Bool
    , getConfig :: IO Config
    , getHelpStyle :: Zoom -> IO EventMapHelp.Config
    , cursorStartPos :: Widget.Id
    }

-- TODO: If moving GUI to lib,
-- include a default help font in the lib rather than get a path.
defaultOptions :: FilePath -> IO Options
defaultOptions helpFontPath =
    do
        loadHelpFont <- memoIO $ \size -> Draw.openFont size helpFontPath
        return Options
            { tickHandler = return False
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
                    zoomFactor <- Zoom.getSizeFactor zoom
                    helpFont <- loadHelpFont (9 * zoomFactor)
                    EventMapHelp.defaultConfig helpFont & return
            , cursorStartPos =
                -- Note that not every app is necessarily interactive and even uses a cursor,
                -- so an empty value might be fitting.
                Widget.Id []
            }

quitEventMap :: Functor f => Widget.EventMap (f Widget.EventResult)
quitEventMap =
    Widget.keysEventMap [MetaKey.cmd GLFW.Key'Q] (E.Doc ["Quit"]) (error "Quit")

data Env = Env
    { _eZoom :: Zoom
    , _eWindowSize :: Widget.Size
    , _eCursor :: Widget.Id
    }
Lens.makeLenses ''Env
instance Widget.HasCursor Env where cursor = eCursor

data Cursor = Cursor
    { _cursorId :: !Widget.Id
    , _cursorVirtual :: !Widget.VirtualCursorUpdate
    }
Lens.makeLenses ''Cursor

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
        cursorRef <-
            newIORef Cursor
            { _cursorId = cursorStartPos options
            , _cursorVirtual =
                -- Will update when have a focal area
                Widget.ResetVirtualCursor
            }
        let mkW =
                memoIO $ \size ->
                do
                    zoomEventMap <- mkZoomEventMap
                    helpStyle <- getHelpStyle zoom
                    cursor <- readIORef cursorRef
                    mkWidgetUnmemod
                        Env
                        { _eZoom = zoom
                        , _eWindowSize = size
                        , _eCursor = cursor ^. cursorId
                        }
                        <&> E.strongerEvents zoomEventMap
                        >>= addHelp helpStyle size
        mkWidgetRef <- mkW >>= newIORef
        let newWidget = mkW >>= writeIORef mkWidgetRef
        let renderWidget size =
                Widget.renderWithCursor
                <$> (getConfig <&> cCursor)
                <*> (readIORef mkWidgetRef >>= (size &))
        let lookupEvent mEnter _ (GLFWE.EventMouseButton
                (GLFWE.MouseButtonEvent GLFW.MouseButton'1
                    GLFW.MouseButtonState'Released _ mousePosF _)) =
                case mEnter of
                Nothing -> return Nothing
                Just enter -> enter (Direction.Point mousePosF) ^. Widget.enterResultEvent & Just & return
            lookupEvent _ Nothing _ = return Nothing
            lookupEvent _ (Just (focalArea, mkEventMap)) event =
                do
                    virtCursorState <- readIORef cursorRef <&> (^. cursorVirtual)
                    virtCursor <-
                        case virtCursorState of
                        Widget.NewVirtualCursor x -> return x
                        Widget.ResetVirtualCursor ->
                            res <$ modifyIORef cursorRef (cursorVirtual .~ Widget.NewVirtualCursor res)
                            where
                                res = Widget.VirtualCursor focalArea
                    E.lookup (GLFW.getClipboardString win <&> fmap Text.pack) event
                        (mkEventMap virtCursor)
        MainAnim.mainLoop win (getConfig <&> cAnim) $ \size -> MainAnim.Handlers
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
                    mWidgetRes <- lookupEvent mEnter mFocus event
                    EventResult runInMainThread mRes <- sequenceA mWidgetRes ^. m
                    case mRes of
                        Nothing -> return ()
                        Just res ->
                            do
                                traverse_ (modifyIORef cursorRef . (cursorId .~)) (res ^. Widget.eCursor)
                                traverse_ (modifyIORef cursorRef . (cursorVirtual .~)) (res ^. Widget.eVirtualCursor)
                                newWidget
                    return MainAnim.EventResult
                        { MainAnim.erAnimIdMapping = mRes <&> (^. Widget.eAnimIdMapping)
                        , MainAnim.erExecuteInMainThread = runInMainThread
                        }
            , MainAnim.makeFrame = renderWidget size <&> (^. _1)
            }
    where
        Options{tickHandler, getConfig, getHelpStyle} = options
