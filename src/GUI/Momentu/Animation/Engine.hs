{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}

module GUI.Momentu.Animation.Engine
    ( initialState, nextState, currentFrame
    , State, stateMapIdentities
    ) where

import           Control.Applicative (liftA2)
import qualified Control.Lens as Lens
import qualified Data.List as List
import           Data.List.Utils (groupOn)
import           Data.Map ((!))
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import qualified Data.Vector.Vector2 as Vector2
import qualified Graphics.DrawingCombinators as Draw
import           GUI.Momentu.Animation (Image, iRect, iAnimId, iUnitImage, Frame(..), frameImages, images, R, AnimId)
import           GUI.Momentu.Rect (Rect(Rect))
import qualified GUI.Momentu.Rect as Rect

import           Lamdu.Prelude

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

newtype State = State
    { _stateInterpolations :: [Interpolation]
    }
Lens.makeLenses ''State

currentFrame :: State -> Frame
currentFrame (State interpolations) =
    interpolations ^.. traverse . interpolationImage & Frame

initialState :: State
initialState = State []

rectDistance :: Rect -> Rect -> R
rectDistance ra rb =
    liftA2 max
    (abs (ra ^. Rect.topLeft - rb ^. Rect.topLeft))
    (abs (ra ^. Rect.bottomRight - rb ^. Rect.bottomRight))
    & Vector2.uncurry max

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

advanceState :: R -> State -> State
advanceState speed = stateInterpolations %~ mapMaybe (advanceInterpolation speed)

setNewDest :: Frame -> State -> State
setNewDest destFrame state =
    go (curFrame ^. frameImages) (destFrame ^. frameImages) & State
    where
        go (c:cs) (d:ds)
            | c ^. iAnimId == d ^. iAnimId =
                modifying d (c ^. iRect) : go cs ds
        go (c:cs) ds
            | (c ^. iAnimId) `Set.member` destIds =
                go cs ds -- c will be treated in its new position
            | otherwise =
                Deleting c : go cs ds
        go [] ds = map goDest ds
        goDest d =
            case findPrefix (d ^. iAnimId) curPrefixMap of
            Nothing -> Rect (d ^. iRect . Rect.center) 0
            Just (prefix, curRect) ->
                relocateSubRect (d ^. iRect) (destPrefixMap ! prefix) curRect
            & modifying d
        modifying destImage prevRect =
            Modifying (rImg & iRect .~ prevRect) (destImage ^. iRect)
            where
                rImg
                    | (destImage ^. iAnimId) `Set.member` duplicateDestIds
                        = destImage & iUnitImage %~ mappend redX
                    | otherwise = destImage
                redX = Draw.tint red unitX
        curFrame = currentFrame state
        sortedDestIds = destFrame ^.. images . iAnimId & List.sort
        duplicateDestIds =
            List.group sortedDestIds <&> tail & concat & Set.fromAscList
        destIds = Set.fromAscList sortedDestIds
        curPrefixMap = prefixRects curFrame
        destPrefixMap = prefixRects destFrame

stateMapIdentities :: (AnimId -> AnimId) -> State -> State
stateMapIdentities mapping =
    stateInterpolations . traverse . interpolationImage . iAnimId %~ mapping

nextState :: R -> Maybe Frame -> State -> Maybe State
nextState movement Nothing state
    | all (Lens.has _Final) (state ^. stateInterpolations) = Nothing
    | otherwise = advanceState movement state & Just
nextState movement (Just dest) state =
    setNewDest dest state & advanceState movement & Just

findPrefix :: Ord a => [a] -> Map [a] b -> Maybe ([a], b)
findPrefix key dict =
    (List.inits key & reverse <&> f) ^? traverse . Lens._Just
    where
        f prefix = Map.lookup prefix dict <&> (,) prefix

relocateSubRect :: Rect -> Rect -> Rect -> Rect
relocateSubRect srcSubRect srcSuperRect dstSuperRect =
    Rect
    { Rect._topLeft =
              dstSuperRect ^. Rect.topLeft +
              sizeRatio *
              (srcSubRect ^. Rect.topLeft -
                srcSuperRect ^. Rect.topLeft)
    , Rect._size = sizeRatio * srcSubRect ^. Rect.size
    }
    where
        sizeRatio =
            dstSuperRect ^. Rect.size /
            fmap (max 1) (srcSuperRect ^. Rect.size)

unitX :: Draw.Image ()
unitX =
    Draw.line (0, 0) (1, 1)
    <> Draw.line (1, 0) (0, 1)
    & void

red :: Draw.Color
red = Draw.Color 1 0 0 1

prefixRects :: Frame -> Map AnimId Rect
prefixRects srcFrame =
    do
        img <- srcFrame ^. frameImages
        prefix <- List.inits (img ^. iAnimId)
        return (prefix, img ^. iRect)
    & List.sortOn fst
    & groupOn fst
    <&> perGroup
    & filter (not . null . fst)
    & Map.fromList
    where
        perGroup xs =
            (fst (head xs), List.foldl1' unionRects (map snd xs))
        unionRects a b =
            Rect
            { Rect._topLeft = tl
            , Rect._size = br - tl
            }
            where
                tl = liftA2 min (a ^. Rect.topLeft) (b ^. Rect.topLeft)
                br = liftA2 max (a ^. Rect.bottomRight) (b ^. Rect.bottomRight)
