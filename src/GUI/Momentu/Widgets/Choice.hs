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

makeInner ::
    (Applicative f, Eq childId) =>
    (Widget.Id -> Bool) ->
    (FocusDelegator.Config -> FocusDelegator.FocusEntryTarget ->
     Widget.Id -> Widget (f State.Update) -> fd) -> (childId -> f ()) ->
    [(childId, Widget (f State.Update))] -> childId -> Config -> Widget.Id -> fd
makeInner cursorOn fd choose children curChild config myId =
    children <&> annotate
    <&> prependEntryAction
    & filterVisible
    <&> colorize
    & Glue.box (cwcOrientation config)
    & fd (cwcFDConfig config) FocusDelegator.FocusEntryParent myId
    where
        filterVisible
            | anyChildFocused || (autoExpand && cursorOn myId) = id
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
        prependEntryAction (isSelected, action, widget) =
            ( isSelected
            , widget
                & Widget.wState . Widget._StateUnfocused . Widget.uMEnter
                    . Lens._Just . Lens.mapped . Widget.enterResultEvent
                    %~ (action *>)
            )
        anyChildFocused =
            Lens.orOf (Lens.traversed . _2 . Lens.to Widget.isFocused) children
        annotate (item, widget) =
            ( if item == curChild then Selected else NotSelected
            , choose item
            , widget
            )

make ::
    (Eq a, MonadReader env m, Applicative f, State.HasCursor env) =>
    m
    ((a -> f ()) -> [(a, Widget (f State.Update))] -> a ->
     Config -> Widget.Id -> Widget (f State.Update))
make =
    do
        cursorOn <- State.isSubCursor
        fd <- FocusDelegator.make
        makeInner cursorOn fd & pure
