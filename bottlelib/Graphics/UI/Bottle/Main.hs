{-# LANGUAGE DeriveFunctor, NoImplicitPrelude, TemplateHaskell #-}
module Graphics.UI.Bottle.Main
    ( Looper(..), newLooper
    , Config(..), EventResult(..), M(..), m
    ) where

import           Control.Applicative (liftA2)
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Monad (when)
import           Control.Monad.IO.Class (MonadIO(..))
import           Data.IORef
import           Data.MRUMemo (memoIO)
import qualified Data.Text as Text
import qualified Graphics.UI.Bottle.Direction as Direction
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Main.Animation as MainAnim
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.GLFW as GLFW
import           Graphics.UI.GLFW.Events as GLFWE

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

newtype Looper = Looper
    { runLooper ::
          GLFW.Window -> IO Bool ->
          (Widget.Size -> IO (Widget (M Widget.EventResult))) ->
          IO Config -> IO ()
    }

newLooper :: IO Looper
newLooper =
    MainAnim.newLooper
    <&> \(MainAnim.Looper loop) ->
    Looper $ \win widgetTickHandler mkWidgetUnmemod getConfig ->
    do
        mkWidgetRef <- newIORef =<< memoIO mkWidgetUnmemod
        let newWidget = writeIORef mkWidgetRef =<< memoIO mkWidgetUnmemod
        let getWidget size = ($ size) =<< readIORef mkWidgetRef
        let lookupEvent widget (GLFWE.EventMouseButton
                (GLFWE.MouseButtonEvent GLFW.MouseButton'1
                    GLFW.MouseButtonState'Released _ mousePosF _)) =
                case widget ^. Widget.mEnter of
                Nothing -> return Nothing
                Just enter -> enter (Direction.Point mousePosF) ^. Widget.enterResultEvent & Just & return
            lookupEvent widget event =
                E.lookup (GLFW.getClipboardString win <&> fmap Text.pack) event
                (widget ^. Widget.eventMap)
        loop win (getConfig <&> cAnim) $ \size -> MainAnim.Handlers
            { MainAnim.tickHandler =
                do
                    anyUpdate <- widgetTickHandler
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
                    widget <- getWidget size
                    mWidgetRes <- lookupEvent widget event
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
