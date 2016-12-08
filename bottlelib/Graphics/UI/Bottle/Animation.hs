{-# LANGUAGE NoImplicitPrelude, DeriveGeneric, TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}

module Graphics.UI.Bottle.Animation
    ( R, Size
    , Image(..), iAnimId, iUnitImage, iRect
    , Frame(..), frameImages, unitImages
    , draw
    , initialState, nextState, currentFrame
    , mapIdentities
    , unitSquare, emptyRectangle
    , backgroundColor
    , translate, scale
    , unitIntoRect
    , simpleFrame, sizedFrame
    , State, stateMapIdentities, stateClearImages
    , module Graphics.UI.Bottle.Animation.Id
    ) where

import           Control.Applicative (liftA2)
import           Control.Lens (Lens')
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Monad (void)
import qualified Data.List as List
import           Data.List.Utils (groupOn)
import           Data.Map (Map, (!))
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import           Data.Vector.Vector2 (Vector2(..))
import qualified Data.Vector.Vector2 as Vector2
import           GHC.Generics (Generic)
import           Graphics.DrawingCombinators (R, (%%))
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.DrawingCombinators.Utils as DrawUtils
import           Graphics.UI.Bottle.Animation.Id
import           Graphics.UI.Bottle.Rect (Rect(Rect))
import qualified Graphics.UI.Bottle.Rect as Rect

import           Prelude.Compat

type Size = Vector2 R

data Image = Image
    { _iAnimId :: AnimId
    , _iUnitImage :: !(Draw.Image ())
        -- iUnitImage always occupies (0,0)..(1,1),
        -- the translation/scaling occurs when drawing
    , _iRect :: !Rect
    } deriving (Generic)
Lens.makeLenses ''Image

newtype Frame = Frame
    { _frameImages :: [Image]
    } deriving (Generic)
Lens.makeLenses ''Frame

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
        sortedDestIds = destFrame ^.. frameImages . traverse . iAnimId & List.sort
        duplicateDestIds =
            List.group sortedDestIds <&> tail & concat & Set.fromAscList
        destIds = Set.fromAscList sortedDestIds
        curPrefixMap = prefixRects curFrame
        destPrefixMap = prefixRects destFrame

stateMapIdentities :: (AnimId -> AnimId) -> State -> State
stateMapIdentities mapping =
    stateInterpolations . traverse . interpolationImage . iAnimId %~ mapping

-- When images are based on stale data (unloaded Font) we must clear them.
stateClearImages :: State -> State
stateClearImages =
    stateInterpolations . traverse . interpolationImage . iUnitImage .~ mempty

nextState :: R -> Maybe Frame -> State -> Maybe State
nextState movement Nothing state
    | all (Lens.has _Final) (state ^. stateInterpolations) = Nothing
    | otherwise = advanceState movement state & Just
nextState movement (Just dest) state =
    setNewDest dest state & advanceState movement & Just

{-# INLINE images #-}
images :: Lens.Traversal' Frame Image
images = frameImages . Lens.traversed

{-# INLINE unitImages #-}
unitImages :: Lens.Traversal' Frame (Draw.Image ())
unitImages = images . iUnitImage

simpleFrame :: AnimId -> Draw.Image () -> Frame
simpleFrame animId image = Frame [Image animId image (Rect 0 1)]

sizedFrame :: AnimId -> Size -> Draw.Image () -> Frame
sizedFrame animId size =
    scale size .
    simpleFrame animId .
    (DrawUtils.scale (1 / size) %%)

instance Monoid Frame where
    mempty = Frame mempty
    mappend (Frame m0) (Frame m1) = Frame (m0 ++ m1)

unitX :: Draw.Image ()
unitX = void $ mconcat
    [ Draw.line (0, 0) (1, 1)
    , Draw.line (1, 0) (0, 1)
    ]

red :: Draw.Color
red = Draw.Color 1 0 0 1

draw :: Frame -> Draw.Image ()
draw frame =
    frame
    ^. frameImages
    <&> posImage
    & mconcat
    where
        posImage (Image _ img rect) =
            DrawUtils.translate (rect ^. Rect.topLeft) %%
            DrawUtils.scale (rect ^. Rect.size) %%
            img

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

mapIdentities :: (AnimId -> AnimId) -> Frame -> Frame
mapIdentities f = frameImages . traverse . iAnimId %~ f

unitSquare :: AnimId -> Frame
unitSquare animId = simpleFrame animId DrawUtils.square

emptyRectangle :: Vector2 R -> Vector2 R -> AnimId -> Frame
emptyRectangle (Vector2 fX fY) totalSize@(Vector2 sX sY) animId =
    mconcat
    [ rect 0                      (Vector2 sX fY)
    , rect (Vector2 0 (sY - fY))  (Vector2 sX fY)
    , rect (Vector2 0 fY)         (Vector2 fX (sY - fY*2))
    , rect (Vector2 (sX - fX) fY) (Vector2 fX (sY - fY*2))
    ]
    & sizedFrame animId totalSize
    where
        rect origin size =
            DrawUtils.square
            & (DrawUtils.scale size %%)
            & (DrawUtils.translate origin %%)

backgroundColor :: AnimId -> Draw.Color -> Vector2 R -> Frame
backgroundColor animId color size =
    unitSquare animId
    & images . iUnitImage %~ Draw.tint color
    & scale size

translate :: Vector2 R -> Frame -> Frame
translate pos = images . iRect . Rect.topLeft +~ pos

scale :: Vector2 R -> Frame -> Frame
scale factor = images . iRect . Rect.topLeftAndSize *~ factor

-- Scale/translate a Unit-sized frame into a given rect
unitIntoRect :: Rect -> Frame -> Frame
unitIntoRect r =
    translate (r ^. Rect.topLeft) .
    scale (r ^. Rect.size)
