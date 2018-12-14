{-# LANGUAGE FlexibleContexts, DisambiguateRecordFields #-}
module GUI.Momentu.Widgets.Grid
    ( make, makeWithKeys
    , Keys(..), stdKeys
    ) where

import qualified Control.Lens as Lens
import           Control.Monad (msum)
import           Data.Foldable (toList)
import           Data.List.Extended (foldl', transpose, sortOn, groupOn, minimumOn)
import           Data.MRUMemo (memo)
import           Data.Maybe.Extended (unionMaybeWith)
import           Data.Vector.Vector2 (Vector2(..))
import qualified Data.Vector.Vector2 as Vector2
import           GUI.Momentu.Align (Aligned(..))
import           GUI.Momentu.Direction (Orientation(..), Order(..))
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as EventMap
import           GUI.Momentu.FocusDirection (FocusDirection(..), GeometricOrigin(..))
import           GUI.Momentu.MetaKey (MetaKey(..), noMods)
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.ModKey (ModKey)
import           GUI.Momentu.Rect (Rect(..))
import qualified GUI.Momentu.Rect as Rect
import           GUI.Momentu.State (Gui)
import qualified GUI.Momentu.State as State
import           GUI.Momentu.Widget (R, Widget(Widget))
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widget.Instances as WidgetGlue
import qualified GUI.Momentu.Widgets.GridView as GridView
import           GUI.Momentu.Widgets.StdKeys (stdDirKeys, DirKeys(..))

import           Lamdu.Prelude

type Cursor = Vector2 Int

data NavDests a = NavDests
    { cursorLeft
    , cursorUp
    , cursorRight
    , cursorDown
    , cursorTop
    , cursorLeftMost
    , cursorBottom
    , cursorRightMost :: Maybe (Widget.EnterResult a)
    }

mkNavDests ::
    Functor f =>
    Cursor -> State.VirtualCursor ->
    [[Maybe (FocusDirection -> Gui Widget.EnterResult f)]] ->
    Gui NavDests f
mkNavDests (Vector2 cursorX cursorY) virtCursor rows =
    NavDests
    { cursorLeft    = reverse colsLeft  & enter Horizontal Forward
    , cursorUp     = reverse rowsAbove & enter Vertical Forward
    , cursorRight   = colsRight         & enter Horizontal Backward
    , cursorDown     = rowsBelow         & enter Vertical Backward

    , cursorTop       = take 1 rowsAbove           & enter Vertical   Backward
    , cursorLeftMost  = take 1 colsLeft            & enter Horizontal Backward
    , cursorBottom    = reverse rowsBelow & take 1 & enter Vertical   Forward
    , cursorRightMost = reverse colsRight & take 1 & enter Horizontal Forward
    }
    where
        columns = transpose rows
        colsLeft = take cursorX columns
        colsRight = drop (cursorX+1) columns
        rowsAbove = take cursorY rows
        rowsBelow = drop (cursorY+1) rows
        enter o = enterFrom o (Dir.rectRange o)
        setVirt axis enterResult =
            enterResult
            & Widget.enterResultEvent . Lens.mapped . State.uVirtualCursor . Lens._Wrapped ?~
            ( enterResult ^. Widget.enterResultRect
                & Lens.cloneLens axis .~ prevArea ^. Lens.cloneLens axis
                & State.VirtualCursor
            )
        prevArea = virtCursor ^. State.vcRect
        enterFrom orientation axis dir lns =
            lns
            <&> foldl' (WidgetGlue.combineMEnters (Dir.perpendicular orientation))
                Nothing
            & msum
            ?? FromGeometric (GeometricOrigin orientation dir (prevArea ^# axis))
            <&> setVirt axis

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
    { keysDir = stdDirKeys <&> k
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
            [ movement "left"       (keysLeft  dir)      cursorLeft
            , movement "right"      (keysRight dir)      cursorRight
            , movement "up"         (keysUp    dir)      cursorUp
            , movement "down"       (keysDown  dir)      cursorDown
            , movement "more left"  (keysMoreLeft keys)  cursorLeftMost
            , movement "more right" (keysMoreRight keys) cursorRightMost
            ] ^. Lens.traverse . Lens._Just
        strongMap =
            [ movement "top"       (keysTop keys)       cursorTop
            , movement "bottom"    (keysBottom keys)    cursorBottom
            , movement "leftmost"  (keysLeftMost keys)  cursorLeftMost
            , movement "rightmost" (keysRightMost keys) cursorRightMost
            ] ^. Lens.traverse . Lens._Just
        movement dirName events f =
            f navDests
            <&> (^. Widget.enterResultEvent)
            <&> EventMap.keyPresses
                events
                (EventMap.Doc ["Navigation", "Move", dirName])

make ::
    (Traversable vert, Traversable horiz, Applicative f) =>
    vert (horiz (Aligned (Gui Widget f))) ->
    (vert (horiz (Aligned ())), Gui Widget f)
make = makeWithKeys (stdKeys <&> MetaKey.toModKey)

makeWithKeys ::
    (Traversable vert, Traversable horiz, Applicative f) =>
    Keys ModKey ->
    vert (horiz (Aligned (Gui Widget f))) ->
    (vert (horiz (Aligned ())), Gui Widget f)
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
    Applicative f =>
    Keys ModKey -> Widget.Size ->
    [[(Rect, Gui Widget f)]] ->
    Gui Widget f
toWidgetWithKeys keys size sChildren =
    Widget
    { _wSize = size
    , _wState =
        case states ^@? each2d <. Widget._StateFocused of
        Nothing ->
            Widget.StateUnfocused Widget.Unfocused
            { _uLayers = unfocusedLayers
            , _uMStroll = mconcat (unfocused ^.. each2d . Lens._Just . Widget.uMStroll)
            , _uMEnter = combineMEnters unfocusedMEnters
            }
        Just (cursor, makeFocusedChild) ->
            Widget.StateFocused $
            \surrounding ->
            let focusedChild = makeFocusedChild surrounding
                addNavDests eventContext =
                    mkNavDests cursor (eventContext ^. Widget.eVirtualCursor) unfocusedMEnters
                    & addNavEventmap keys
                (before, after) = break ((>= cursor) . fst) (unfocused ^@.. each2d)
                -- TODO: DRY with Widget's Glue instance
                addEventStroll events =
                    case strollAheadDst of
                    Nothing -> events
                    Just (dst, _) ->
                        events <&> Lens.mapped %~
                        \e ->
                        if e ^. State.uPreferStroll . Lens._Wrapped
                        then
                            e
                            & State.uCursor .~ (Just (dst ^. Lens._Wrapped) ^. Lens._Unwrapped)
                            & State.uPreferStroll .~ mempty
                        else e
                strollAheadDst = after ^. traverse . _2 . Lens._Just . Widget.uMStroll
                strollBefore =
                    before ^. traverse . _2 . Lens._Just . Widget.uMStroll
                    & foldMap
                        ( EventMap.keysEventMapMovesCursor [MetaKey.shift MetaKey.Key'Tab]
                            (EventMap.Doc ["Navigation", "Stroll", "Back"])
                            . pure . (^. _2 . Lens._Wrapped)
                        )
                strollAfter =
                    foldMap
                    ( EventMap.keysEventMapMovesCursor [MetaKey MetaKey.noMods MetaKey.Key'Tab]
                        (EventMap.Doc ["Navigation", "Stroll", "Ahead"])
                        . pure . (^. _1 . Lens._Wrapped)
                    ) strollAheadDst
            in
            Widget.Focused
            { Widget._fLayers = focusedChild ^. Widget.fLayers <> unfocusedLayers
            , Widget._fFocalAreas = focusedChild ^. Widget.fFocalAreas
            , Widget._fPreEvents = focusedChild ^. Widget.fPreEvents
            , Widget._fMEnterPoint =
                focusedChild ^. Widget.fMEnterPoint
                & unionMaybeWith Widget.combineEnterPoints (combineMEnters unfocusedMEnters <&> (. Point))
            , Widget._fEventMap =
                (focusedChild ^. Widget.fEventMap
                    <&> addEventStroll
                    & Lens.imapped %@~ addNavDests)
                <&> (<> (strollBefore <> strollAfter))
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
    [[Maybe (FocusDirection -> Widget.EnterResult a)]] ->
    Maybe (FocusDirection -> Widget.EnterResult a)
combineMEnters children
    | null childEnters = Nothing
    | otherwise = Just byDirection
    where
        childEnters = children ^@.. each2d <. Lens._Just

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
                    Point x -> Rect.topLeft .~ x
                    FromOutside -> id
                    FromGeometric (GeometricOrigin o _ x) ->
                        Dir.rectRange (Dir.perpendicular o) .~ x
                edge =
                    case dir of
                    Point{} -> Vector2 0 0 -- Check all widgets for mouse movements (for hovers)
                    FromOutside -> Vector2 0 0
                    FromGeometric (GeometricOrigin Vertical Backward _) ->
                        Vector2 0 (-1)
                    FromGeometric (GeometricOrigin Vertical Forward _) ->
                        Vector2 0 1
                    FromGeometric (GeometricOrigin Horizontal Backward _) ->
                        Vector2 (-1) 0
                    FromGeometric (GeometricOrigin Horizontal Forward _) ->
                        Vector2 1 0

        -- | Take only the first/last enterable row/column
        filteredByEdge =
            memo $ \(Vector2 hEdge vEdge) ->
            childEnters
            & groupSortOn ((* (-vEdge)) . (^._1._2)) & (^. Lens.ix 0)
            & groupSortOn ((* (-hEdge)) . (^._1._1)) & (^. Lens.ix 0)
            <&> snd
