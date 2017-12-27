{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, DeriveTraversable, FlexibleContexts, RankNTypes, DisambiguateRecordFields #-}
module GUI.Momentu.Widgets.Grid
    ( make, makeWithKeys
    , Keys(..), stdKeys
    ) where

import           Control.Applicative (liftA2)
import qualified Control.Lens as Lens
import           Control.Monad (msum)
import           Data.Foldable (toList)
import           Data.List (foldl', transpose, sortOn)
import           Data.List.Utils (groupOn, minimumOn)
import           Data.MRUMemo (memo)
import           Data.Maybe.Utils (unionMaybeWith)
import           Data.Vector.Vector2 (Vector2(..))
import qualified Data.Vector.Vector2 as Vector2
import           GUI.Momentu.Align (Aligned(..))
import           GUI.Momentu.Direction (Direction(..))
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as EventMap
import           GUI.Momentu.MetaKey (MetaKey(..), noMods)
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.ModKey (ModKey)
import           GUI.Momentu.Rect (Rect(..))
import qualified GUI.Momentu.Rect as Rect
import qualified GUI.Momentu.State as State
import           GUI.Momentu.Widget (R, Widget(Widget))
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.GridView as GridView
import           GUI.Momentu.Widgets.StdKeys (DirKeys(..), stdDirKeys)

import           Lamdu.Prelude

type Cursor = Vector2 Int

length2d :: (Foldable vert, Foldable horiz) => vert (horiz a) -> Vector2 Int
length2d xs = Vector2 (foldl' max 0 (toList xs <&> length)) (length xs)

capCursor :: Vector2 Int -> Vector2 Int -> Vector2 Int
capCursor size = fmap (max 0) . liftA2 min (subtract 1 <$> size)

data NavDests a = NavDests
    { leftOfCursor
    , aboveCursor
    , rightOfCursor
    , belowCursor
    , topCursor
    , leftMostCursor
    , bottomCursor
    , rightMostCursor :: Maybe (Widget.EnterResult a)
    }

mkNavDests ::
    Functor f =>
    Cursor -> State.VirtualCursor ->
    [[Maybe (Direction -> Widget.EnterResult (f State.Update))]] ->
    NavDests (f State.Update)
mkNavDests cursor@(Vector2 cursorX cursorY) virtCursor mEnterss =
    NavDests
    { leftOfCursor    = take cursorX curRow    & reverse & enterHoriz FromRight
    , aboveCursor     = take cursorY curColumn & reverse & enterVert  FromBelow
    , rightOfCursor   = drop (cursorX+1) curRow          & enterHoriz FromLeft
    , belowCursor     = drop (cursorY+1) curColumn       & enterVert  FromAbove

    , topCursor       = take (min 1 cursorY) curColumn                & enterVert  FromAbove
    , leftMostCursor  = take (min 1 cursorX) curRow                   & enterHoriz FromLeft
    , bottomCursor    = drop (cursorY+1) curColumn & reverse & take 1 & enterVert  FromBelow
    , rightMostCursor = drop (cursorX+1) curRow    & reverse & take 1 & enterHoriz FromRight
    }
    where
        enterHoriz cons x = enterFrom (cons (prevArea ^. Rect.verticalRange  )) x <&> setHVirt
        enterVert  cons x = enterFrom (cons (prevArea ^. Rect.horizontalRange)) x <&> setVVirt
        setHVirt = setVirt Rect.verticalRange
        setVVirt = setVirt Rect.horizontalRange
        setVirt axis enterResult =
            enterResult
            & Widget.enterResultEvent . Lens.mapped . State.uVirtualCursor . Lens._Wrapped ?~
            ( enterResult ^. Widget.enterResultRect
                & Lens.cloneLens axis .~ prevArea ^. Lens.cloneLens axis
                & State.VirtualCursor
            )
        curRow = fromMaybe [] $ mEnterss ^? Lens.ix cappedY
        curColumn = fromMaybe [] $ transpose mEnterss ^? Lens.ix cappedX
        Vector2 cappedX cappedY = capCursor size cursor
        size = length2d mEnterss
        prevArea = virtCursor ^. State.virtualCursor
        enterFrom dir mEnters = mEnters & msum ?? dir

data Keys key = Keys
    { keysDir :: DirKeys key
    , keysMoreLeft :: [key]
    , keysMoreRight :: [key]
    , keysLeftMost :: [key]
    , keysRightMost :: [key]
    , keysTop :: [key]
    , keysBottom :: [key]
    } deriving (Functor, Foldable, Traversable)

stdKeys :: Keys MetaKey
stdKeys = Keys
    { keysDir = k <$> stdDirKeys
    , keysMoreLeft = [k MetaKey.Key'Home]
    , keysMoreRight = [k MetaKey.Key'End]
    , keysLeftMost = [MetaKey.cmd MetaKey.Key'Home]
    , keysRightMost = [MetaKey.cmd MetaKey.Key'End]
    , keysTop = [k MetaKey.Key'PageUp]
    , keysBottom = [k MetaKey.Key'PageDown]
    }
    where
        k = MetaKey noMods

addNavEventmap :: Keys ModKey -> NavDests a -> EventMap a -> EventMap a
addNavEventmap keys navDests eMap =
    strongMap <> eMap <> weakMap
    where
        dir = keysDir keys
        weakMap =
            [ movement "left"       (keysLeft  dir)      leftOfCursor
            , movement "right"      (keysRight dir)      rightOfCursor
            , movement "up"         (keysUp    dir)      aboveCursor
            , movement "down"       (keysDown  dir)      belowCursor
            , movement "more left"  (keysMoreLeft keys)  leftMostCursor
            , movement "more right" (keysMoreRight keys) rightMostCursor
            ] ^. Lens.traverse . Lens._Just
        strongMap =
            [ movement "top"       (keysTop keys)       topCursor
            , movement "bottom"    (keysBottom keys)    bottomCursor
            , movement "leftmost"  (keysLeftMost keys)  leftMostCursor
            , movement "rightmost" (keysRightMost keys) rightMostCursor
            ] ^. Lens.traverse . Lens._Just
        movement dirName events f =
            (EventMap.keyPresses
              events
              (EventMap.Doc ["Navigation", "Move", dirName]) .
              (^. Widget.enterResultEvent)) <$>
            f navDests

make ::
    (Traversable vert, Traversable horiz, Functor f) =>
    vert (horiz (Aligned (Widget (f State.Update)))) ->
    (vert (horiz (Aligned ())), Widget (f State.Update))
make = makeWithKeys (stdKeys <&> MetaKey.toModKey)

makeWithKeys ::
    (Traversable vert, Traversable horiz, Functor f) =>
    Keys ModKey ->
    vert (horiz (Aligned (Widget (f State.Update)))) ->
    (vert (horiz (Aligned ())), Widget (f State.Update))
makeWithKeys keys children =
    ( content & each2d %~ void
    , toList content <&> toList
      & each2d %~ (\(Aligned _ (rect, widget)) -> (rect, widget))
      & toWidgetWithKeys keys size
    )
    where
        (size, content) = GridView.makePlacements children

each2d :: (Traversable vert, Traversable horiz) => Lens.IndexedTraversal Cursor (vert (horiz a)) (vert (horiz b)) a b
each2d = Lens.traversed <.> Lens.traversed & Lens.reindexed (uncurry (flip Vector2))

-- TODO: We assume that the given Cursor selects a focused
-- widget. Prove it by passing the Focused data of that widget
toWidgetWithKeys ::
    Functor f =>
    Keys ModKey -> Widget.Size ->
    [[(Rect, Widget (f State.Update))]] ->
    Widget (f State.Update)
toWidgetWithKeys keys size sChildren =
    Widget
    { _wSize = size
    , _wState =
        case states ^@? each2d <. Widget._StateFocused of
        Nothing ->
            Widget.StateUnfocused Widget.Unfocused
            { _uLayers = unfocusedLayers
            , _uMEnter = combineMEnters unfocusedMEnters
            }
        Just (cursor, makeFocusedChild) ->
            Widget.StateFocused $
            \surrounding ->
            let focusedChild = makeFocusedChild surrounding
                addNavDests eventContext =
                    mkNavDests cursor (eventContext ^. Widget.eVirtualCursor) unfocusedMEnters
                    & addNavEventmap keys
            in
            Widget.Focused
            { Widget._fLayers = focusedChild ^. Widget.fLayers <> unfocusedLayers
            , Widget._fFocalAreas = focusedChild ^. Widget.fFocalAreas
            , Widget._fMEnterPoint =
                focusedChild ^. Widget.fMEnterPoint
                & unionMaybeWith Widget.combineEnterPoints (combineMEnters unfocusedMEnters <&> (. Point))
            , Widget._fEventMap = focusedChild ^. Widget.fEventMap & Lens.imapped %@~ addNavDests
            }
    }
    where
        translateChildWidget (rect, widget) =
            widget
            -- Each child is set to the size of the entire grid and
            -- then translated to its place in order to fix the
            -- Surrounding parameters of all children
            & Element.size .~ size
            & Widget.translate (rect ^. Rect.topLeft)
        states = sChildren & each2d %~ translateChildWidget
        unfocused = states & each2d %~ (^? Widget._StateUnfocused)
        unfocusedMEnters = unfocused & each2d %~ (>>= (^. Widget.uMEnter))
        unfocusedLayers = unfocused ^. each2d . Lens._Just . Widget.uLayers

groupSortOn :: Ord b => (a -> b) -> [a] -> [[a]]
groupSortOn f = groupOn f . sortOn f

combineMEnters ::
    [[Maybe (Direction -> Widget.EnterResult a)]] ->
    Maybe (Direction -> Widget.EnterResult a)
combineMEnters children =
    chooseClosest childEnters
    where
        childEnters = children ^@.. each2d . Lens._Just

        chooseClosest [] = Nothing
        chooseClosest _ = Just byDirection

        byDirection dir =
            filteredByEdge edge <&> (dir &) & minimumOn score
            where
                score enter
                    | dist > 0 = dist
                    | otherwise = enter ^. Widget.enterResultLayer & negate & fromIntegral
                    where
                        dist =
                            enter ^. Widget.enterResultRect
                            & Rect.distances dirRect
                            & removeUninterestingAxis
                            & Vector2.sqrNorm
                removeUninterestingAxis :: Vector2 R -> Vector2 R
                removeUninterestingAxis = ((1 - abs (fromIntegral <$> edge)) *)
                dirRect =
                    Rect 0 0 &
                    case dir of
                    Outside -> id
                    Point x -> Rect.topLeft .~ x
                    FromAbove x -> Rect.horizontalRange .~ x
                    FromBelow x -> Rect.horizontalRange .~ x
                    FromLeft  x -> Rect.verticalRange .~ x
                    FromRight x -> Rect.verticalRange .~ x
                edge =
                    case dir of
                    Point{} -> Vector2 0 0 -- Check all widgets for mouse movements (for hovers)
                    Outside -> Vector2 0 0
                    FromAbove{} -> Vector2 0 (-1)
                    FromBelow{} -> Vector2 0 1
                    FromLeft{}  -> Vector2 (-1) 0
                    FromRight{} -> Vector2 1 0

        filteredByEdge =
            memo $ \(Vector2 hEdge vEdge) ->
            map snd .
            safeHead . groupSortOn ((* (-hEdge)) . (^._1._1)) .
            safeHead . groupSortOn ((* (-vEdge)) . (^._1._2)) $
            childEnters

safeHead :: Monoid a => [a] -> a
safeHead = mconcat . take 1
