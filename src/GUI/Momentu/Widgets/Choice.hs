-- | A vertical-expand (combo-like) choice widget

module GUI.Momentu.Widgets.Choice
    ( make
    , defaultFdConfig
    , Config(..), defaultConfig
    , Orientation(..)
    ) where

import qualified Control.Lens as Lens
import           Data.Property (Property(..))
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Direction (Orientation(..), perpendicular, axis)
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Glue as Glue
import           GUI.Momentu.Hover (Hover, AnchoredWidget)
import qualified GUI.Momentu.Hover as Hover
import           GUI.Momentu.MetaKey (MetaKey(..), noMods)
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.State (Gui)
import qualified GUI.Momentu.State as State
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.FocusDelegator as FocusDelegator

import           Lamdu.Prelude

defaultFdConfig :: E.Subtitle -> FocusDelegator.Config
defaultFdConfig helpPrefix =
    FocusDelegator.Config
    { FocusDelegator.focusChildKeys = [MetaKey noMods MetaKey.Key'Enter]
    , FocusDelegator.focusChildDoc = E.Doc [helpPrefix, "Select"]
    , FocusDelegator.focusParentKeys = [MetaKey.Key'Enter, MetaKey.Key'Escape] <&> MetaKey noMods
    , FocusDelegator.focusParentDoc = E.Doc [helpPrefix, "Choose selected"]
    }

data Config = Config
    { cwcFDConfig :: FocusDelegator.Config
    , cwcOrientation :: Orientation
    }

defaultConfig :: E.Subtitle -> Config
defaultConfig helpPrefix =
    Config
    { cwcFDConfig = defaultFdConfig helpPrefix
    , cwcOrientation = Vertical
    }

data IsSelected = Selected | NotSelected
    deriving Eq

type HoverFunc f = Gui AnchoredWidget f -> Hover (Gui AnchoredWidget f)

makeInner ::
    (Applicative f, Eq childId) =>
    HoverFunc f ->
    (FocusDelegator.Config -> FocusDelegator.FocusEntryTarget ->
     Widget.Id -> Gui Widget f -> Gui Widget f) ->
    Property f childId ->
    [(childId, Gui Widget f)] -> Config -> Widget.Id ->
    Gui Widget f
makeInner hover fd (Property curChild choose) children config myId =
    widget True
    & (if anyChildFocused then hoverAsClosed else id)
    & Element.padToSizeAlign (0 & perp .~ maxDim) 0
    where
        orientation = cwcOrientation config
        perp :: Lens' (Vector2 a) a
        perp = axis (perpendicular orientation)
        maxDim = children <&> (^. _2 . Element.size . perp) & maximum
        hoverAsClosed open =
            [hover (Hover.anchor open)]
            `Hover.hoverInPlaceOf` Hover.anchor (widget False)
        widget allowExpand =
            children <&> annotate
            <&> prependEntryAction
            & filterVisible allowExpand
            <&> snd
            & Glue.box orientation
            & fd (cwcFDConfig config) FocusDelegator.FocusEntryParent myId
        filterVisible allowExpand
            | allowExpand && anyChildFocused = id
            | otherwise = filter ((== Selected) . fst)
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
    ( Eq childId, MonadReader env m, Applicative f
    , State.HasCursor env, Hover.HasStyle env, Element.HasAnimIdPrefix env
    ) =>
    m
    (Property f childId -> [(childId, Gui Widget f)] ->
     Config -> Widget.Id -> Gui Widget f)
make = makeInner <$> Hover.hover <*> FocusDelegator.make
