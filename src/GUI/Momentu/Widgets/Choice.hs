{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}
-- | A vertical-expand (combo-like) choice widget

module GUI.Momentu.Widgets.Choice
    ( make
    , Config(..)
    , ExpandMode(..)
    , Orientation(..)
    ) where

import qualified Control.Lens as Lens
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.Glue (Orientation(..))
import qualified GUI.Momentu.Glue as Glue
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
    Widget.Id -> [(IsSelected, f (), Widget (f Widget.EventResult))] ->
    Widget (f Widget.EventResult)
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
                            Element.backgroundColor (Widget.toAnimId myId) color
                    _ -> id
        applyAction (isSelected, action, widget) =
            ( isSelected
            , widget
                & Widget.mEnter . Lens._Just . Lens.mapped .
                    Widget.enterResultEvent %~ (action *>)
            )
        anyChildFocused =
            childrenRecords
            & Lens.orOf (Lens.traversed . _3 . Lens.to Widget.isFocused)

makeInternal ::
    (MonadReader env m, Widget.HasCursor env, Applicative f) =>
    m (Config -> [(IsSelected, f (), Widget (f Widget.EventResult))] ->
       Widget.Id -> Widget (f Widget.EventResult))
makeInternal =
    do
        sub <- Widget.subId
        fd <- FocusDelegator.make
        pure $ \config children myId ->
            let selfFocused = sub myId & Lens.has Lens._Just
                childrenBox = toBox config selfFocused myId children
            in  fd (cwcFDConfig config) FocusDelegator.FocusEntryParent myId childrenBox

make ::
    (Eq a, MonadReader env m, Applicative f, Widget.HasCursor env) =>
    m
    ((a -> f ()) -> [(a, Widget (f Widget.EventResult))] -> a ->
     Config -> Widget.Id -> Widget (f Widget.EventResult))
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
