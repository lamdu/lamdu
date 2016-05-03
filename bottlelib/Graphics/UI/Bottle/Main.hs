{-# LANGUAGE DeriveFunctor, NoImplicitPrelude, TemplateHaskell #-}
module Graphics.UI.Bottle.Main
    ( mainLoopWidget, Config(..), EventResult(..), M(..), m, mLiftWidget
    ) where

import           Control.Applicative (liftA2)
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Monad (when, unless)
import           Control.Monad.IO.Class (MonadIO(..))
import           Data.IORef
import           Data.MRUMemo (memoIO)
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Main.Animation as MainAnim
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.GLFW as GLFW

import           Prelude.Compat

data Config = Config
    { cAnim :: MainAnim.AnimConfig
    , cCursor :: Widget.CursorConfig
    }

data EventResult a = EventResult
    { erExecuteInMainThread :: IO ()
    , erVal :: a
    } deriving Functor

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

mLiftWidget :: Widget (IO a) -> Widget (M a)
mLiftWidget = Widget.events %~ liftIO

mainLoopWidget ::
    GLFW.Window -> IO Bool ->
    (Widget.Size -> IO (Widget (M Widget.EventResult))) -> IO Config ->
    IO ()
mainLoopWidget win widgetTickHandler mkWidgetUnmemod getConfig =
    do
        mkWidgetRef <- newIORef =<< memoIO mkWidgetUnmemod
        let newWidget = writeIORef mkWidgetRef =<< memoIO mkWidgetUnmemod
        let getWidget size = ($ size) =<< readIORef mkWidgetRef
        MainAnim.mainLoop win (getConfig <&> cAnim) $ \size -> MainAnim.Handlers
            { MainAnim.tickHandler =
                do
                    anyUpdate <- widgetTickHandler
                    when anyUpdate newWidget
                    widget <- getWidget size
                    EventResult runInMainThread tickResults <-
                        sequenceA (widget ^. Widget.eventMap . E.emTickHandlers)
                        ^. m
                    unless (null tickResults) newWidget
                    return MainAnim.EventResult
                        { MainAnim.erAnimIdMapping =
                            case (tickResults, anyUpdate) of
                            ([], False) -> Nothing
                            _ -> Just . mconcat $ map (^. Widget.eAnimIdMapping) tickResults
                        , MainAnim.erExecuteInMainThread = runInMainThread
                        }
            , MainAnim.eventHandler = \event ->
                do
                    widget <- getWidget size
                    let eventMap = widget ^. Widget.eventMap
                    mWidgetRes <- E.lookup (GLFW.getClipboardString win) event eventMap
                    EventResult runInMainThread mAnimIdMapping <-
                        (sequenceA mWidgetRes <&> fmap (^. Widget.eAnimIdMapping)) ^. m
                    case mAnimIdMapping of
                        Nothing -> return ()
                        Just _ -> newWidget
                    return MainAnim.EventResult
                        { MainAnim.erAnimIdMapping = mAnimIdMapping
                        , MainAnim.erExecuteInMainThread = runInMainThread
                        }
            , MainAnim.makeFrame =
                Widget.renderWithCursor
                <$> (getConfig <&> cCursor)
                <*> getWidget size
            }
