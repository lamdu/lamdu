{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, OverloadedStrings #-}
-- | A vertical-expand (combo-like) choice widget

module GUI.Momentu.Widgets.Choice
    ( make
    , Config(..), defaultFdConfig
    , ExpandMode(..)
    , Orientation(..)
    ) where

import qualified Control.Lens as Lens
import qualified GUI.Momentu.Draw as MDraw
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue (Orientation(..))
import qualified GUI.Momentu.Glue as Glue
import           GUI.Momentu.Hover (Hover, AnchoredWidget)
import qualified GUI.Momentu.Hover as Hover
import           GUI.Momentu.MetaKey (MetaKey(..), noMods)
import qualified GUI.Momentu.MetaKey as MetaKey
import qualified GUI.Momentu.State as State
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.DrawingCombinators as Draw

import           Lamdu.Prelude

data ExpandMode
    -- Cursor is on expanded widget, need to show selected choice with a
    -- color: (TODO: Remove this?)
    = AutoExpand Draw.Color
    | ExplicitEntry
Lens.makePrisms ''ExpandMode

defaultFdConfig :: E.Subtitle -> FocusDelegator.Config
defaultFdConfig helpPrefix =
    FocusDelegator.Config
    { FocusDelegator.focusChildKeys = [MetaKey noMods MetaKey.Key'Enter]
    , FocusDelegator.focusChildDoc = E.Doc [helpPrefix, "Select"]
    , FocusDelegator.focusParentKeys = [MetaKey noMods MetaKey.Key'Enter]
    , FocusDelegator.focusParentDoc = E.Doc [helpPrefix, "Choose selected"]
    }

data Config = Config
    { cwcFDConfig :: FocusDelegator.Config
    , cwcOrientation :: Orientation
    , cwcExpandMode :: ExpandMode
    }

data IsSelected = Selected | NotSelected
    deriving Eq

type HoverFunc f =
    AnchoredWidget (f State.Update) -> Hover (AnchoredWidget (f State.Update))

makeInner ::
    (Applicative f, Eq childId) =>
    HoverFunc f -> (Widget.Id -> Bool) ->
    (FocusDelegator.Config -> FocusDelegator.FocusEntryTarget ->
     Widget.Id -> Widget (f State.Update) -> Widget (f State.Update)) ->
    (childId -> f ()) ->
    [(childId, Widget (f State.Update))] -> childId -> Config -> Widget.Id ->
    Widget (f State.Update)
makeInner hover cursorOn fd choose children curChild config myId =
    widget True
    & if expanded
    then hoverAsClosed
    else id
    where
        hoverAsClosed open =
            [hover (Hover.anchor open)]
            `Hover.hoverInPlaceOf` Hover.anchor (widget False)
        expanded = anyChildFocused || (autoExpand && cursorOn myId)
        widget allowExpand =
            children <&> annotate
            <&> prependEntryAction
            & filterVisible allowExpand
            <&> colorize
            & Glue.box (cwcOrientation config)
            & fd (cwcFDConfig config) FocusDelegator.FocusEntryParent myId
        filterVisible allowExpand
            | allowExpand && expanded = id
            | otherwise = filter ((== Selected) . fst)
        autoExpand = cwcExpandMode config & Lens.has _AutoExpand
        colorize (isSelected, w)
            | anyChildFocused = w -- focus shows selection already
            | otherwise = -- need to show selection even as focus is elsewhere
                w
                & case cwcExpandMode config of
                    AutoExpand color
                        | isSelected == Selected ->
                            MDraw.backgroundColor (Widget.toAnimId myId) color
                    _ -> id
        prependEntryAction (isSelected, action, w) =
            ( isSelected
            , w
                & Widget.wState . Widget._StateUnfocused . Widget.uMEnter
                    . Lens._Just . Lens.mapped . Widget.enterResultEvent
                    %~ (action *>)
            )
        anyChildFocused =
            Lens.orOf (Lens.traversed . _2 . Lens.to Widget.isFocused) children
        annotate (item, w) =
            ( if item == curChild then Selected else NotSelected
            , choose item
            , w
            )

make ::
    ( Eq a, MonadReader env m, Applicative f
    , State.HasCursor env, Hover.HasStyle env, Element.HasAnimIdPrefix env
    ) =>
    m
    ((a -> f ()) -> [(a, Widget (f State.Update))] -> a ->
     Config -> Widget.Id -> Widget (f State.Update))
make =
    do
        hover <- Hover.hover
        cursorOn <- State.isSubCursor
        fd <- FocusDelegator.make
        makeInner hover cursorOn fd & pure
