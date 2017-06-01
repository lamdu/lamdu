{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, RecordWildCards #-}
-- | A vertical-expand (combo-like) choice widget

module Graphics.UI.Bottle.Widgets.Choice
    ( make
    , IsSelected(..)
    , Config(..)
    , ExpandMode(..)
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Reader (MonadReader)
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator

import           Lamdu.Prelude

data ExpandMode
    -- Cursor is on expanded widget, need to show selected choice with a
    -- color:
    = AutoExpand Draw.Color
    | ExplicitEntry
Lens.makePrisms ''ExpandMode

data Config = Config
    { cwcFDConfig :: FocusDelegator.Config
    , cwcOrientation :: Box.Orientation
    , cwcExpandMode :: ExpandMode
    }

data IsSelected = Selected | NotSelected
    deriving Eq

toBox ::
    Applicative f => Config -> Bool ->
    Widget.Id -> [(IsSelected, f (), Widget (f Widget.EventResult))] ->
    Widget (f Widget.EventResult)
toBox Config{..} selfFocused myId childrenRecords =
    childrenRecords
    <&> applyAction
    & filterVisible
    <&> colorize
    & Box.makeAlign 0 cwcOrientation
    & snd
    where
        filterVisible
            | anyChildFocused || (autoExpand && selfFocused) = id
            | otherwise = filter ((== Selected) . fst)
        autoExpand = Lens.has _AutoExpand cwcExpandMode
        colorize (isSelected, widget)
            | anyChildFocused = widget -- focus shows selection already
            | otherwise = -- need to show selection even as focus is elsewhere
                widget
                & case cwcExpandMode of
                    AutoExpand color
                        | isSelected == Selected ->
                            Widget.backgroundColor (Widget.toAnimId myId) color
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

make ::
    (MonadReader env m, Widget.HasCursor env, Applicative f) =>
    Config -> [(IsSelected, f (), Widget (f Widget.EventResult))] ->
    Widget.Id -> m (Widget (f Widget.EventResult))
make Config{..} children myId =
    do
        selfFocused <- Widget.subId myId <&> Lens.has Lens._Just
        let childrenBox = toBox Config{..} selfFocused myId children
        FocusDelegator.make cwcFDConfig
            FocusDelegator.FocusEntryParent myId ?? childrenBox
