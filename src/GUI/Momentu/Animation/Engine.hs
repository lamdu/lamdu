{-# LANGUAGE TemplateHaskell #-}
module GUI.Momentu.Animation.Engine
    ( Config(..)
    , currentFrame
    , State
    , initialState
    , AdvancedAnimation(..), _AnimationComplete, _NewState
    , advanceAnimation, clockedAdvanceAnimation
    ) where

import           Control.Applicative (liftA2)
import qualified Control.Lens as Lens
import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import           Data.Time.Clock (NominalDiffTime, UTCTime, getCurrentTime, addUTCTime, diffUTCTime)
import qualified Data.Vector.Vector2 as Vector2
import           GUI.Momentu.Animation (Image, iRect, iAnimId, iUnitImage, Frame(..), frameImages, images, R)
import           GUI.Momentu.Rect (Rect(Rect))
import qualified GUI.Momentu.Rect as Rect
import qualified Graphics.DrawingCombinators as Draw

import           GUI.Momentu.Prelude

data Config = Config
    { acTimePeriod :: NominalDiffTime
    , acRemainingRatioInPeriod :: R
    }

data Interpolation
    = Deleting Image
      -- ^ An image that is interpolating from its current state to nothingness
    | Modifying {-cur-}Image {-dest-}Rect
      -- ^ An image that is interpolating from the cur Rect towards the dest Image
    | Final Image
      -- ^ An image that finished interpolating
Lens.makePrisms ''Interpolation

interpolationImage :: Lens' Interpolation Image
interpolationImage f (Deleting img) = f img <&> Deleting
interpolationImage f (Modifying curImg destRect) = f curImg <&> (`Modifying` destRect)
interpolationImage f (Final img) = f img <&> Final

data State = State
    { _asCurSpeedHalfLife :: !NominalDiffTime
    , _asCurTime :: !UTCTime
    , _asInterpolations :: [Interpolation]
    }
Lens.makeLenses ''State

initialState :: IO State
initialState =
    getCurrentTime <&>
    \curTime -> State
    { _asCurSpeedHalfLife = 0
    , _asCurTime = curTime
    , _asInterpolations = []
    }

data AdvancedAnimation = AnimationComplete | NewState State
Lens.makePrisms ''AdvancedAnimation

advanceInterpolation :: R -> Interpolation -> Maybe Interpolation
advanceInterpolation _ x@Final{} = Just x
advanceInterpolation movement (Modifying curImage destRect)
    | rectDistance (curImage ^. iRect) destRect < equalityThreshold =
        curImage & iRect .~ destRect & Final & Just
    | otherwise =
        curImage
        & iRect .~
            Rect
            (animSpeed * destTopLeft + (1 - animSpeed) * curTopLeft)
            (animSpeed * destSize    + (1 - animSpeed) * curSize   )
        & (`Modifying` destRect) & Just
    where
        equalityThreshold = 0.2
        animSpeed = pure movement
        Rect destTopLeft destSize = destRect
        Rect curTopLeft curSize = curImage ^. iRect
advanceInterpolation movement (Deleting img)
    | Vector2.sqrNorm (img ^. iRect . Rect.size) < 1 = Nothing
    | otherwise =
        img
        & iRect . Rect.centeredSize *~ pure (1 - movement)
        & Deleting & Just

advanceInterpolations :: R -> [Interpolation] -> [Interpolation]
advanceInterpolations = mapMaybe . advanceInterpolation

nextInterpolations :: R -> Maybe Frame -> [Interpolation] -> Maybe [Interpolation]
nextInterpolations movement Nothing interpolations
    | all (Lens.has _Final) interpolations = Nothing
    | otherwise = advanceInterpolations movement interpolations & Just
nextInterpolations movement (Just dest) interpolations =
    setNewDest dest interpolations & advanceInterpolations movement & Just

advanceAnimation ::
    Real a => a -> Maybe Frame -> UTCTime -> State -> AdvancedAnimation
advanceAnimation elapsed mNewDestFrame curTime animState =
    nextInterpolations progress mNewDestFrame (animState ^. asInterpolations)
    <&> (\newInterpolations -> animState & asInterpolations .~ newInterpolations)
    <&> asCurTime .~ curTime
    & maybe AnimationComplete NewState
    where
        progress = 1 - 0.5 ** (realToFrac elapsed / realToFrac (animState ^. asCurSpeedHalfLife))

desiredFrameRate :: Double
desiredFrameRate = 60

clockedAdvanceAnimation ::
    Config -> Maybe (UTCTime, Frame) -> State -> IO AdvancedAnimation
clockedAdvanceAnimation (Config timePeriod ratio) mNewFrame animState =
    getCurrentTime <&>
    \curTime ->
    case mNewFrame of
    Just (userEventTime, newDestFrame) ->
        animState
        & asCurSpeedHalfLife .~ timeRemaining / realToFrac (logBase 0.5 ratio)
        & advanceAnimation elapsed (Just newDestFrame) curTime
        where
            -- Retroactively pretend animation started a little bit
            -- sooner so there's already a change in the first frame
            elapsed = 1.0 / desiredFrameRate
            timeRemaining =
                max 0 $
                diffUTCTime
                (addUTCTime timePeriod userEventTime)
                curTime
    Nothing ->
        advanceAnimation (curTime `diffUTCTime` (animState ^. asCurTime))
        Nothing curTime animState

frameOfInterpolations :: [Interpolation] -> Frame
frameOfInterpolations interpolations =
    interpolations ^.. traverse . interpolationImage & Frame

currentFrame :: State -> Frame
currentFrame = frameOfInterpolations . _asInterpolations

rectDistance :: Rect -> Rect -> R
rectDistance ra rb =
    liftA2 max
    (abs (ra ^. Rect.topLeft - rb ^. Rect.topLeft))
    (abs (ra ^. Rect.bottomRight - rb ^. Rect.bottomRight))
    & Vector2.uncurry max

setNewDest :: Frame -> [Interpolation] -> [Interpolation]
setNewDest destFrame interpolations =
    go (curFrame ^. frameImages) (destFrame ^. frameImages)
    where
        go (c:cs) (d:ds)
            | c ^. iAnimId == d ^. iAnimId =
                modifying d (c ^. iRect) : go cs ds
        go (c:cs) ds
            | destIds ^. Lens.contains (c ^. iAnimId) =
                go cs ds -- c will be treated in its new position
            | otherwise =
                Deleting c : go cs ds
        go [] ds = map goDest ds
        goDest d =
            curRects ^. Lens.at (d ^. iAnimId)
            & fromMaybe (Rect (d ^. iRect . Rect.center) 0)
            & modifying d
        modifying destImage prevRect =
            Modifying (rImg & iRect .~ prevRect) (destImage ^. iRect)
            where
                rImg
                    | duplicateDestIds ^. Lens.contains (destImage ^. iAnimId)
                        = destImage & iUnitImage %~ mappend redX
                    | otherwise = destImage
                redX = Draw.tint red unitX
        curFrame = frameOfInterpolations interpolations
        curRects =
            do
                img <- curFrame ^. frameImages
                [(img ^. iAnimId, img ^. iRect)]
            & Map.fromList
        sortedDestIds = destFrame ^.. images . iAnimId & List.sort
        duplicateDestIds =
            List.group sortedDestIds <&> tail & concat & Set.fromAscList
        destIds = Set.fromAscList sortedDestIds

unitX :: Draw.Image ()
unitX =
    Draw.line (0, 0) (1, 1) <>
    Draw.line (1, 0) (0, 1)
    & void

red :: Draw.Color
red = Draw.Color 1 0 0 1

