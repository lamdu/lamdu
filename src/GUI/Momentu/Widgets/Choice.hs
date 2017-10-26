{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}
-- | A vertical-expand (combo-like) choice widget

module GUI.Momentu.Widgets.Choice
    ( make
    , Config(..)
    , ExpandMode(..)
    , Orientation(..)
    ) where

import qualified Control.Lens as Lens
import qualified GUI.Momentu.Draw as MDraw
import           GUI.Momentu.Glue (Orientation(..))
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.State as State
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.DrawingCombinators as Draw

import           Lamdu.Prelude

data ExpandMode
    -- Cursor is on expanded widget, need to show selected choice with a
    -- color:
    = AutoExpand Draw.Color
    | ExplicitEntry
Lens.makePrisms ''ExpandMode

data Config = Config
    { cwcFDConfig :: FocusDelegator.Config
    , cwcOrientation :: Orientation
    , cwcExpandMode :: ExpandMode
    }

data IsSelected = Selected | NotSelected
    deriving Eq

toBox ::
    Applicative f => Config -> Bool ->
    Widget.Id -> [(IsSelected, f (), Widget (f State.Update))] ->
    Widget (f State.Update)
toBox config selfFocused myId childrenRecords =
    childrenRecords
    <&> applyAction
    & filterVisible
    <&> colorize
    & Glue.box (cwcOrientation config)
    where
        filterVisible
            | anyChildFocused || (autoExpand && selfFocused) = id
            | otherwise = filter ((== Selected) . fst)
        autoExpand = cwcExpandMode config & Lens.has _AutoExpand
        colorize (isSelected, widget)
            | anyChildFocused = widget -- focus shows selection already
            | otherwise = -- need to show selection even as focus is elsewhere
                widget
                & case cwcExpandMode config of
                    AutoExpand color
                        | isSelected == Selected ->
                            MDraw.backgroundColor (Widget.toAnimId myId) color
                    _ -> id
        applyAction (isSelected, action, widget) =
            ( isSelected
            , widget
                & Widget.wState . Widget._StateUnfocused . Widget.uMEnter
                    . Lens._Just . Lens.mapped . Widget.enterResultEvent
                    %~ (action *>)
            )
        anyChildFocused =
            childrenRecords
            & Lens.orOf (Lens.traversed . _3 . Lens.to Widget.isFocused)

makeInternal ::
    (MonadReader env m, State.HasCursor env, Applicative f) =>
    m (Config -> [(IsSelected, f (), Widget (f State.Update))] ->
       Widget.Id -> Widget (f State.Update))
makeInternal =
    do
        sub <- State.subId
        fd <- FocusDelegator.make
        pure $ \config children myId ->
            let selfFocused = sub myId & Lens.has Lens._Just
                childrenBox = toBox config selfFocused myId children
            in  fd (cwcFDConfig config) FocusDelegator.FocusEntryParent myId childrenBox

make ::
    (Eq a, MonadReader env m, Applicative f, State.HasCursor env) =>
    m
    ((a -> f ()) -> [(a, Widget (f State.Update))] -> a ->
     Config -> Widget.Id -> Widget (f State.Update))
make =
    makeInternal <&> f
    where
        f mk choose children curChild choiceConfig =
            mk choiceConfig (children <&> annotate)
            where
                annotate (item, widget) =
                    ( if item == curChild then Selected else NotSelected
                    , choose item
                    , widget
                    )
