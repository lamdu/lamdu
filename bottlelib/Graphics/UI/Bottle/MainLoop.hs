{-# LANGUAGE CPP, RecordWildCards #-}
module Graphics.UI.Bottle.MainLoop
    ( mainLoopAnim
    , mainLoopImage
    , mainLoopWidget
    ) where

import           Control.Applicative ((<$>))
import           Control.Concurrent (threadDelay)
import           Control.Lens (Lens')
import           Control.Lens.Operators
import           Control.Monad (when, unless, guard)
import           Data.IORef
import           Data.MRUMemo (memoIO)
import           Data.Monoid (Monoid(..), (<>))
import qualified Data.Monoid as Monoid
import           Data.Time.Clock (DiffTime, UTCTime, getCurrentTime, diffUTCTime)
import           Data.Traversable (traverse, sequenceA)
import           Data.Vector.Vector2 (Vector2(..))
import           Graphics.DrawingCombinators ((%%))
import           Graphics.DrawingCombinators.Utils (Image)
import qualified Graphics.DrawingCombinators.Utils as DrawUtils
import           Graphics.Rendering.OpenGL.GL (($=))
import qualified Graphics.Rendering.OpenGL.GL as GL
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.GLFW as GLFW
import           Graphics.UI.GLFW.Events (KeyEvent, Event(..), Result(..), eventLoop)

data EventResult =
    ERNone | ERRefresh | ERQuit
    deriving (Eq, Ord, Show)
instance Monoid EventResult where
    mempty = ERNone
    mappend = max

data ImageHandlers = ImageHandlers
  { imageEventHandler :: KeyEvent -> IO EventResult
  , imageUpdate :: IO (Maybe Image)
  , imageRefresh :: IO Image
  }

windowSize :: GLFW.Window -> IO Widget.Size
windowSize win =
    do
        (x, y) <- GLFW.getFramebufferSize win
        return $ fromIntegral <$> Vector2 x y

mainLoopImage :: GLFW.Window -> (Widget.Size -> ImageHandlers) -> IO ()
mainLoopImage win imageHandlers =
    eventLoop win handleEvents
    where
        handleEvent handlers (EventKey keyEvent) = imageEventHandler handlers keyEvent
        handleEvent _ EventWindowClose = return ERQuit
        handleEvent _ EventWindowRefresh = return ERRefresh

        handleEvents events =
            do
                winSize <- windowSize win
                let handlers = imageHandlers winSize
                eventResult <- mconcat <$> traverse (handleEvent handlers) events
                case eventResult of
                    ERQuit -> return ResultQuit
                    ERRefresh -> imageRefresh handlers >>= draw winSize
                    ERNone -> imageUpdate handlers >>= maybe delay (draw winSize)
        delay =
            do
                -- TODO: If we can verify that there's sync-to-vblank, we
                -- need no sleep here
                threadDelay 10000
                return ResultNone
        draw winSize@(Vector2 winSizeX winSizeY) image =
            do
                GL.viewport $=
                    (GL.Position 0 0,
                     GL.Size (round winSizeX) (round winSizeY))
                image
                    & (DrawUtils.translate (Vector2 (-1) 1) <>
                       DrawUtils.scale (Vector2 (2/winSizeX) (-2/winSizeY)) %%)
                    & let Vector2 glPixelRatioX glPixelRatioY = winSize / 2 -- GL range is -1..1
                      in DrawUtils.clearRenderSized (glPixelRatioX, glPixelRatioY)
                return ResultDidDraw

data AnimHandlers = AnimHandlers
    { animTickHandler :: IO (Maybe (Monoid.Endo AnimId))
    , animEventHandler :: KeyEvent -> IO (Maybe (Monoid.Endo AnimId))
    , animMakeFrame :: IO Anim.Frame
    }

data IsAnimating = Animating | NotAnimating
    deriving Eq

data AnimState = AnimState
    { _asIsAnimating :: IsAnimating
    , _asCurTime :: UTCTime
    , _asCurFrame :: Anim.Frame
    , _asDestFrame :: Anim.Frame
    }

asIsAnimating :: Lens' AnimState IsAnimating
asIsAnimating f AnimState {..} = f _asIsAnimating <&> \_asIsAnimating -> AnimState {..}

asCurTime :: Lens' AnimState UTCTime
asCurTime f AnimState {..} = f _asCurTime <&> \_asCurTime -> AnimState {..}

asCurFrame :: Lens' AnimState Anim.Frame
asCurFrame f AnimState {..} = f _asCurFrame <&> \_asCurFrame -> AnimState {..}

asDestFrame :: Lens' AnimState Anim.Frame
asDestFrame f AnimState {..} = f _asDestFrame <&> \_asDestFrame -> AnimState {..}

newAnimState :: Anim.Frame -> IO AnimState
newAnimState initialFrame =
    do
        curTime <- getCurrentTime
        return AnimState
            { _asIsAnimating = NotAnimating
            , _asCurTime = curTime
            , _asCurFrame = initialFrame
            , _asDestFrame = initialFrame
            }

atomicModifyIORef_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ ioref f = atomicModifyIORef ioref (flip (,) () . f)

mainLoopAnim :: GLFW.Window -> IO DiffTime -> (Widget.Size -> AnimHandlers) -> IO ()
mainLoopAnim win getAnimationHalfLife animHandlers =
    do
        frameStateVar <-
            windowSize win <&> animHandlers >>= animMakeFrame >>= newAnimState
            >>= newIORef
        let handleResult Nothing = return ERNone
            handleResult (Just animIdMapping) =
                do
                    atomicModifyIORef_ frameStateVar $
                        asCurFrame %~ Anim.mapIdentities (Monoid.appEndo animIdMapping)
                    return ERRefresh
            refreshIfNeeded ERRefresh aHandlers state =
                do
                    destFrame <- animMakeFrame aHandlers
                    state
                        & asIsAnimating .~ Animating
                        & asDestFrame .~ destFrame
                        & return
            refreshIfNeeded _ _ state = return state
            advanceAnimation state =
                do
                    curTime <- getCurrentTime
                    case state ^. asIsAnimating of
                        Animating ->
                            do
                                animationHalfLife <- getAnimationHalfLife
                                let elapsed = curTime `diffUTCTime` (state ^. asCurTime)
                                    progress = 1 - 0.5 ** (realToFrac elapsed / realToFrac animationHalfLife)
                                return $
                                    case Anim.nextFrame progress (state ^. asDestFrame) (state ^. asCurFrame) of
                                    Nothing ->
                                        state
                                        & asIsAnimating .~ NotAnimating
                                        & asCurFrame .~ state ^. asDestFrame
                                    Just newFrame ->
                                        state
                                        & asCurFrame .~ newFrame
                        NotAnimating -> return state
                        <&> asCurTime .~ curTime
            updateFrameState er aHandlers =
                do
                    tickEr <- animTickHandler aHandlers >>= handleResult
                    state <-
                        readIORef frameStateVar
                        >>= refreshIfNeeded (mappend er tickEr) aHandlers
                        >>= advanceAnimation
                    writeIORef frameStateVar state
                    return state

        let makeHandlers size =
                ImageHandlers
                { imageEventHandler = \event ->
                    animEventHandler aHandlers event >>= handleResult
                , imageRefresh =
                    updateFrameState ERRefresh aHandlers <&> (^. asCurFrame) <&> Anim.draw
                , imageUpdate =
                    updateFrameState ERNone aHandlers
                    <&> \state ->
                        do
                            guard $ Animating == state ^. asIsAnimating
                            Just $ Anim.draw $ state ^. asCurFrame
                }
                where
                    aHandlers = animHandlers size
        mainLoopImage win makeHandlers

mainLoopWidget :: GLFW.Window -> IO Bool -> (Widget.Size -> IO (Widget IO)) -> IO DiffTime -> IO ()
mainLoopWidget win widgetTickHandler mkWidgetUnmemod getAnimationHalfLife =
    do
        mkWidgetRef <- newIORef =<< memoIO mkWidgetUnmemod
        let newWidget = writeIORef mkWidgetRef =<< memoIO mkWidgetUnmemod
            getWidget size = ($ size) =<< readIORef mkWidgetRef
        mainLoopAnim win getAnimationHalfLife $ \size -> AnimHandlers
            { animTickHandler =
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
            , animEventHandler = \event ->
                do
                    widget <- getWidget size
                    mAnimIdMapping <-
                        (traverse . fmap) (^. Widget.eAnimIdMapping) .
                        E.lookup event $ widget ^. Widget.eventMap
                    case mAnimIdMapping of
                        Nothing -> return ()
                        Just _ -> newWidget
                    return mAnimIdMapping
            , animMakeFrame = getWidget size <&> (^. Widget.animFrame)
            }
