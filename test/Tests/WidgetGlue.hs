{-# LANGUAGE StandaloneDeriving, TemplateHaskell, TypeFamilies, DeriveTraversable #-}
module Tests.WidgetGlue
    ( test
    ) where

import qualified Control.Lens as Lens
import           Data.Semigroup (First(..), Last(..))
import           GUI.Momentu
import qualified GUI.Momentu.Draw as Draw
import           GUI.Momentu.Glue
import qualified GUI.Momentu.Hover as Hover
import           GUI.Momentu.Rect (Rect(..))
import           GUI.Momentu.Widget (R, Widget(..))
import qualified GUI.Momentu.Widget as Widget
import           Generic.Random
import           Test.QuickCheck

import           Test.Lamdu.Prelude

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

toWidgetUnfocused :: Applicative f => UnfocusedWidget (Maybe Widget.Id) -> Gui Widget f
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
    glue orientation (toWidgetUnfocused x) (toWidgetUnfocused y)

glueFocused :: GluesTo a b c => GlueOrder -> Orientation -> a -> b -> c
glueFocused FocusedFirst = glue
glueFocused FocusedLast = glue <&> flip

data Env = Env
    { _eAnimId :: AnimId
    , _eHoverStyle :: Hover.Style
    }
Lens.makeLenses ''Env

instance HasAnimIdPrefix Env where
    animIdPrefix = eAnimId
instance Hover.HasStyle Env where
    style = eHoverStyle

toWidgetFocused :: Applicative f => FocusedWidget (Maybe Widget.Id) -> Gui Widget f
toWidgetFocused (FocusedLeaf size) =
    const Widget.Focused
    { Widget._fFocalAreas = [Rect (pure 0) size]
    , Widget._fEventMap = const mempty
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
            [glueFocused glueOrder orientation (hov pop) (Hover.anchor anc)]
            (Hover.anchor anc)
        hov =
            Env ["foo"]
            Hover.Style
            { Hover._frameColor = Draw.Color 1 1 1 1
            , Hover._framePadding = pure 0
            , Hover._bgColor = Draw.Color 0 0 0 0
            , Hover._bgPadding = pure 0
            }
            & Hover.hover
propFocusedWidgetHasFocus :: FocusedWidget () -> Bool
propFocusedWidgetHasFocus tree =
    Widget.isFocused
    (toWidgetFocused (Nothing <$ tree) :: Gui Widget Identity)

propUnfocusedWidgetIsntFocused :: UnfocusedWidget () -> Bool
propUnfocusedWidgetIsntFocused tree =
    (toWidgetUnfocused (Nothing <$ tree) :: Gui Widget Identity)
    & Widget.isFocused & not

test :: Test
test =
    testGroup "glue-tests"
    [ testProperty "focused-has-focus" propFocusedWidgetHasFocus
    , testProperty "unfocused-isnt-focused" propUnfocusedWidgetIsntFocused
    ]
