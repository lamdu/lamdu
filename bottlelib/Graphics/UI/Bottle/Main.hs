{-# LANGUAGE NoImplicitPrelude #-}
module Graphics.UI.Bottle.Main
    ( mainLoopWidget
    ) where

import           Control.Lens.Operators
import           Control.Monad (when, unless)
import           Data.IORef
import           Data.MRUMemo (memoIO)
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Main.Animation as MainAnim
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.GLFW as GLFW

import           Prelude.Compat

mainLoopWidget :: GLFW.Window -> IO Bool -> (Widget.Size -> IO (Widget IO)) -> IO MainAnim.AnimConfig -> IO ()
mainLoopWidget win widgetTickHandler mkWidgetUnmemod getAnimationConfig =
    do
        mkWidgetRef <- newIORef =<< memoIO mkWidgetUnmemod
        let newWidget = writeIORef mkWidgetRef =<< memoIO mkWidgetUnmemod
        let getWidget size = ($ size) =<< readIORef mkWidgetRef
        MainAnim.mainLoop win getAnimationConfig $ \size -> MainAnim.Handlers
            { MainAnim.tickHandler =
                do
                    anyUpdate <- widgetTickHandler
                    when anyUpdate newWidget
                    widget <- getWidget size
                    tickResults <-
                        sequenceA (widget ^. Widget.eventMap . E.emTickHandlers)
                    unless (null tickResults) newWidget
                    return $
                        case (tickResults, anyUpdate) of
                        ([], False) -> Nothing
                        _ -> Just . mconcat $ map (^. Widget.eAnimIdMapping) tickResults
            , MainAnim.eventHandler = \event ->
                do
                    widget <- getWidget size
                    let eventMap = widget ^. Widget.eventMap
                    mWidgetRes <- E.lookup (GLFW.getClipboardString win) event eventMap
                    mAnimIdMapping <- sequenceA mWidgetRes <&> fmap (^. Widget.eAnimIdMapping)
                    case mAnimIdMapping of
                        Nothing -> return ()
                        Just _ -> newWidget
                    return mAnimIdMapping
            , MainAnim.makeFrame = getWidget size <&> (^. Widget.animFrame)
            }
