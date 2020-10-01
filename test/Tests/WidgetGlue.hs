{-# LANGUAGE TypeFamilies #-}
module Tests.WidgetGlue
    ( module X, module Tests.WidgetGlue, module Lens, module Widget, encodeS
    ) where

import           Control.Applicative ((<|>))
import qualified Control.Lens as Lens
import           Data.Binary.Extended (encodeS)
import           Data.Semigroup (First(..), Last(..))
import           Data.Vector.Vector2 (Vector2)
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.EventMap as EventMap
import           GUI.Momentu.Glue as X
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.Main.Events as Events
import           GUI.Momentu.Rect as X (Rect(..))
import qualified GUI.Momentu.State as GUIState
import           GUI.Momentu.Widget (R, Widget(..))
import qualified GUI.Momentu.Widget as Widget
import           Generic.Random
import qualified Graphics.UI.GLFW as GLFW
import qualified Test.Momentu.Env as TestEnv
import           Test.QuickCheck

import           Test.Lamdu.Prelude as X

data GlueOrder = FocusedFirst | FocusedLast
    deriving (Eq, Ord, Generic, Show)

data HoverMode = NoHover | FocusInHover | FocusInAnchor
    deriving (Eq, Ord, Generic, Show)

instance Arbitrary GlueOrder where arbitrary = genericArbitrary uniform
instance Arbitrary HoverMode where arbitrary = genericArbitrary uniform

data FocusedWidget a
    = FocusedLeaf (Vector2 R)
    | FocusedGlue HoverMode GlueOrder Orientation (FocusedWidget a) (UnfocusedWidget a)
    deriving (Show, Generic, Functor, Foldable, Traversable)

data UnfocusedWidget a
    = UnfocusedLeaf (Vector2 R) a
    | UnfocusedGlue Orientation (UnfocusedWidget a) (UnfocusedWidget a)
    deriving (Show, Generic, Functor, Foldable, Traversable)

instance Arbitrary a => Arbitrary (FocusedWidget a) where
    arbitrary = genericArbitraryRec uniform `withBaseCase` (FocusedLeaf <$> arbitrary)

instance Arbitrary a => Arbitrary (UnfocusedWidget a) where
    arbitrary = genericArbitraryRec uniform `withBaseCase` (UnfocusedLeaf <$> arbitrary <*> arbitrary)

toWidgetUnfocused :: Applicative f => UnfocusedWidget (Maybe Widget.Id) -> Widget f
toWidgetUnfocused (UnfocusedLeaf size x) =
    Widget.StateUnfocused Widget.Unfocused
    { Widget._uMEnter = Nothing -- TODO
    , Widget._uMStroll =
        case x of
        Nothing -> Nothing
        Just myId -> Just (First myId, Last myId)
    , Widget._uLayers = mempty
    } & Widget size
toWidgetUnfocused (UnfocusedGlue orientation x y) =
    glue TestEnv.env orientation (toWidgetUnfocused x) (toWidgetUnfocused y)

-- TODO: Test Dir.Layout
glueFocused :: GluesTo TestEnv.Env a b c => GlueOrder -> Orientation -> a -> b -> c
glueFocused FocusedFirst = glue TestEnv.env
glueFocused FocusedLast = glue TestEnv.env <&> flip


toWidgetFocused :: Applicative f => FocusedWidget (Maybe Widget.Id) -> Widget f
toWidgetFocused (FocusedLeaf size) =
    const Widget.Focused
    { Widget._fFocalAreas = [Rect (pure 0) size]
    , Widget._fEventMap = mempty
    , Widget._fPreEvents = mempty
    , Widget._fMEnterPoint = Nothing
    , Widget._fLayers = mempty
    }
    & Widget.StateFocused
    & Widget size
toWidgetFocused (FocusedGlue hoverMode glueOrder orientation foc unf) =
    case hoverMode of
    NoHover -> glueFocused glueOrder orientation focW unfW
    FocusInHover -> mkHover focW unfW
    FocusInAnchor -> mkHover unfW focW
    where
        focW = toWidgetFocused foc
        unfW = toWidgetUnfocused unf
        mkHover pop anc =
            Hover.hoverInPlaceOf
            [glueFocused glueOrder orientation (hov pop)
                (Hover.anchor Dir.LeftToRight anc)]
            (Hover.anchor Dir.LeftToRight anc)
        hov = Hover.hover TestEnv.env

propFocusedWidgetHasFocus :: FocusedWidget () -> Bool
propFocusedWidgetHasFocus tree =
    Widget.isFocused
    (toWidgetFocused (Nothing <$ tree) :: Widget Identity)

propUnfocusedWidgetIsntFocused :: UnfocusedWidget () -> Bool
propUnfocusedWidgetIsntFocused tree =
    (toWidgetUnfocused (Nothing <$ tree) :: Widget Identity)
    & Widget.isFocused & not

expectedStrollDests :: FocusedWidget (Maybe a) -> (Maybe a, Maybe a)
expectedStrollDests FocusedLeaf{} = (Nothing, Nothing)
expectedStrollDests (FocusedGlue hoverMode glueOrder _ foc unf) =
    case (hoverMode, glueOrder) of
    (NoHover, FocusedFirst) -> unfAfter
    (NoHover, FocusedLast) -> unfBefore
    (FocusInHover, _) -> unfBefore
    (FocusInAnchor, _) -> unfAfter
    where
        internal = expectedStrollDests foc
        unfAfter = internal & _2 %~ (<|> unf ^? Lens.folded . Lens._Just)
        unfBefore = internal & _1 %~ (<|> Lens.lastOf (Lens.folded . Lens._Just) unf)

propStrollsCorrectly :: FocusedWidget (Maybe ()) -> Bool
propStrollsCorrectly tree =
    expectedStrollDests treeWithIds ==
    (lookupStroll mempty {GLFW.modifierKeysShift = True}, lookupStroll mempty)
    where
        lookupStroll mods =
            EventMap.lookup Nothing
            (EventMap.EventKey (Events.KeyEvent GLFW.Key'Tab 0 GLFW.KeyState'Pressed mods))
            eventMap
            & join
            >>= (^. EventMap.dhHandler . Lens._Wrapped . GUIState.uCursor . Lens._Wrapped)
        eventMap =
            mkEventMap Widget.EventContext
            { Widget._eVirtualCursor = Rect (pure 0) (pure 0) & GUIState.VirtualCursor
            , Widget._ePrevTextRemainder = ""
            }
        mkEventMap =
            mkFocused (Widget.Surrounding 0 0 0 0) ^. Widget.fEventMap
        mkFocused =
            (toWidgetFocused treeWithIds :: Widget Identity)
            ^?! Widget.wState . Widget._StateFocused
        treeWithIds =
            tree & Lens.traversed . Lens._Just %@~ (\idx () -> Widget.Id [encodeS idx])

test :: Test
test =
    testGroup "glue-tests"
    [ testProperty "focused-has-focus" propFocusedWidgetHasFocus
    , testProperty "unfocused-isnt-focused" propUnfocusedWidgetIsntFocused
    , testProperty "stroll" propStrollsCorrectly
    ]
