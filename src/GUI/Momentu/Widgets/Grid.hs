{-# LANGUAGE FlexibleContexts, DisambiguateRecordFields #-}
module GUI.Momentu.Widgets.Grid
    ( make, makeWithKeys
    , NavDests(..), stdKeys
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
    { cursorLeft :: !a
    , cursorUp :: !a
    , cursorRight :: !a
    , cursorDown :: !a
    , cursorLeftMore :: !a
    , cursorRightMore :: !a
    , cursorLeftMost :: !a
    , cursorRightMost :: !a
    , cursorTop :: !a
    , cursorBottom :: !a
    } deriving (Functor, Foldable, Traversable)

instance Applicative NavDests where
    pure x = NavDests x x x x x x x x x x
    NavDests f0 f1 f2 f3 f4 f5 f6 f7 f8 f9
        <*> NavDests x0 x1 x2 x3 x4 x5 x6 x7 x8 x9
        = NavDests
            (f0 x0) (f1 x1) (f2 x2) (f3 x3) (f4 x4)
            (f5 x5) (f6 x6) (f7 x7) (f8 x8) (f9 x9)

data EventStrength = EventWeak | EventStrong deriving (Eq)

stdKeys :: NavDests [MetaKey]
stdKeys =
    NavDests
    { cursorLeft      = keysLeft dirKeys
    , cursorUp        = keysUp dirKeys
    , cursorRight     = keysRight dirKeys
    , cursorDown      = keysDown dirKeys
    , cursorLeftMore  = [k MetaKey.Key'Home]
    , cursorRightMore = [k MetaKey.Key'End]
    , cursorLeftMost  = [MetaKey.cmd MetaKey.Key'Home]
    , cursorRightMost = [MetaKey.cmd MetaKey.Key'End]
    , cursorTop       = [k MetaKey.Key'PageUp]
    , cursorBottom    = [k MetaKey.Key'PageDown]
    }
    where
        k = MetaKey noMods
        dirKeys = stdDirKeys <&> k

navDestNames :: NavDests Text
navDestNames =
    NavDests
    { cursorLeft      = "left"
    , cursorUp        = "up"
    , cursorRight     = "right"
    , cursorDown      = "down"
    , cursorLeftMore  = "more left"
    , cursorRightMore = "more right"
    , cursorLeftMost  = "leftmost"
    , cursorRightMost = "rightmost"
    , cursorTop       = "top"
    , cursorBottom    = "bottom"
    }

navDestDocs :: NavDests EventMap.Doc
navDestDocs = navDestNames <&> \dirName -> EventMap.Doc ["Navigation", "Move", dirName]

keysStrength :: NavDests EventStrength
keysStrength =
    NavDests
    { cursorLeft      = EventWeak
    , cursorUp        = EventWeak
    , cursorRight     = EventWeak
    , cursorDown      = EventWeak
    , cursorLeftMore  = EventWeak
    , cursorRightMore = EventWeak
    , cursorLeftMost  = EventStrong
    , cursorRightMost = EventStrong
    , cursorTop       = EventStrong
    , cursorBottom    = EventStrong
    }

directions :: NavDests (Orientation, Order)
directions =
    NavDests
    { cursorLeft      = (Horizontal, Backward)
    , cursorUp        = (Vertical, Backward)
    , cursorRight     = (Horizontal, Forward)
    , cursorDown      = (Vertical, Forward)
    , cursorLeftMore  = (Horizontal, Backward)
    , cursorRightMore = (Horizontal, Forward)
    , cursorLeftMost  = (Horizontal, Backward)
    , cursorRightMost = (Horizontal, Forward)
    , cursorTop    = (Vertical, Backward)
    , cursorBottom  = (Vertical, Forward)
    }

-- enter *from* is inverse of direction we're entering to:
enterDirections :: NavDests (Orientation, Order)
enterDirections = directions & Lens.mapped . _2 %~ Dir.reverseOrder

mkNavDests ::
    Functor f =>
    Cursor -> State.VirtualCursor ->
    [[Maybe (FocusDirection -> Gui Widget.EnterResult f)]] ->
    NavDests (Maybe (Widget.EnterResult (f State.Update)))
mkNavDests (Vector2 cursorX cursorY) virtCursor rows =
    uncurry enter
    <$> enterDirections
    <*> NavDests
    { cursorLeft      = reverse colsLeft
    , cursorUp        = reverse rowsAbove
    , cursorRight     = colsRight
    , cursorDown      = rowsBelow

    , cursorTop       = take 1 rowsAbove
    , cursorLeftMore  = take 1 colsLeft
    , cursorLeftMost  = take 1 colsLeft -- same dests, uses strong event map
    , cursorBottom    = reverse rowsBelow & take 1
    , cursorRightMore = reverse colsRight & take 1
    , cursorRightMost = reverse colsRight & take 1 -- same dest use strong event map
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

addNavEventmap ::
    NavDests [ModKey] -> NavDests (Maybe (Widget.EnterResult a)) ->
    EventMap a -> EventMap a
addNavEventmap navKeys enters eMap =
    eventMapOf EventStrong <> eMap <> eventMapOf EventWeak
    where
        eventMaps =
            (,)
            <$> keysStrength
            <*> (movement <$> navKeys <*> navDestDocs <*> enters)
        eventMapOf strength =
            eventMaps ^.. Lens.folded & filter ((== strength) . fst) & foldMap snd
        movement keys doc =
            foldMap $ \enter ->
            enter ^. Widget.enterResultEvent & EventMap.keyPresses keys doc

make ::
    (Traversable vert, Traversable horiz, Applicative f) =>
    vert (horiz (Aligned (Gui Widget f))) ->
    (vert (horiz (Aligned ())), Gui Widget f)
make = makeWithKeys (stdKeys <&> map MetaKey.toModKey)

makeWithKeys ::
    (Traversable vert, Traversable horiz, Applicative f) =>
    NavDests [ModKey] ->
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
    NavDests [ModKey] -> Widget.Size ->
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
                        ( EventMap.keysEventMapMovesCursor Widget.strollBackKeys
                            (EventMap.Doc ["Navigation", "Stroll", "Back"])
                            . pure . (^. _2 . Lens._Wrapped)
                        )
                strollAfter =
                    foldMap
                    ( EventMap.keysEventMapMovesCursor Widget.strollAheadKeys
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
