{-# LANGUAGE TemplateHaskell, TypeFamilies, DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses, ConstraintKinds #-}
module GUI.Momentu.Widgets.EventMapHelp
    ( make
    , IsHelpShown(..)
    , toggledHelpAdder
    , addHelpView
    , Style(..), styleText, styleInputDocColor, styleBGColor, styleTint
    , Config(..), configOverlayDocKeys
    , HasStyle
    , defaultStyle
    , defaultConfig
    , Texts(..), textHelp, textKeyBindings, textShow, textHide
    , HasTexts
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import qualified Data.Aeson.TH.Extended as JsonTH
import           Data.Function (on)
import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Property (Property(..))
import qualified Data.Property as Property
import qualified Data.Text as Text
import qualified Data.Tuple as Tuple
import           Data.Vector.Vector2 (Vector2(..))
import           GHC.Stack (CallStack)
import qualified GHC.Stack as Stack
import           GUI.Momentu.Align (Aligned(..), WithTextPos)
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.Animation (AnimId, R)
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.Draw as MDraw
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Font (Font)
import qualified GUI.Momentu.Font as Font
import           GUI.Momentu.Glue ((/|/), (/-/))
import qualified GUI.Momentu.Glue as Glue
import           GUI.Momentu.MetaKey (MetaKey(..), toModKey, noMods)
import qualified GUI.Momentu.MetaKey as MetaKey
import qualified GUI.Momentu.ModKey as ModKey
import qualified GUI.Momentu.State as State
import           GUI.Momentu.View (View(..), vAnimLayers)
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.GridView as GridView
import qualified GUI.Momentu.Widgets.Label as Label
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Graphics.DrawingCombinators as Draw

import           GUI.Momentu.Prelude

data IsHelpShown = HelpShown | HelpNotShown
    deriving (Generic, Eq, Ord, Read, Show)
JsonTH.derivePrefixed "" ''IsHelpShown

data Style = Style
    { _styleText :: TextView.Style
    , _styleInputDocColor :: Draw.Color
    , _styleSrcLocColor :: Maybe Draw.Color
    , _styleBGColor :: Draw.Color
    , _styleTint :: Draw.Color
    }
Lens.makeLenses ''Style

instance Has TextView.Style Style where has = styleText

newtype Config = Config
    { _configOverlayDocKeys :: [MetaKey]
    }
Lens.makeLenses ''Config

type HasStyle env = (Has TextView.Style env, Has Style env)

data Texts a = Texts
    { _textHelp :: a
    , _textKeyBindings :: a
    , _textShow :: a
    , _textHide :: a
    , _textShowHelp :: a
    }
    deriving stock (Generic, Generic1, Eq, Ord, Show, Functor, Foldable, Traversable)
    deriving Applicative via (Generically1 Texts)
Lens.makeLenses ''Texts
JsonTH.derivePrefixed "_text" ''Texts
type HasTexts env = (Has (E.Texts Text) env, Glue.HasTexts env, Has (Texts Text) env)

defaultStyle :: Font -> Style
defaultStyle font =
    Style
    { _styleText =
        TextView.Style
        { TextView._styleColor = Draw.Color 1 1 1 1
        , TextView._styleFont = font
        , TextView._styleUnderline = Nothing
        }
    , _styleInputDocColor = Draw.Color 0.1 0.7 0.7 1
    , _styleSrcLocColor = Nothing
    , _styleBGColor = Draw.Color 0.2 0.15 0.1 0.5
    , _styleTint = Draw.Color 1 1 1 0.8
    }

defaultConfig :: Config
defaultConfig =
    Config
    { _configOverlayDocKeys = [MetaKey noMods MetaKey.Key'F1]
    }

data Tree n l = Leaf l | Branch n [Tree n l]
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

groupTree :: Eq node => [([node], leaf)] -> [Tree node leaf]
groupTree = foldr step []
    where
        step ([], l) rest = Leaf l : rest
        step (at:as, l) b =
            case b of
                Branch bt bs : rest
                    | at == bt -> Branch bt (step (as, l) bs) : rest
                _ -> Branch at (step (as, l) []) : b

-- We also rely on Map.toList returning a sorted list
groupInputDocs :: [([Text], r)] -> [([Text], [r])]
groupInputDocs = Map.toList . Map.fromListWith (++) . (Lens.traversed . _2 %~ (:[]))

makeShortcutKeyView ::
    ( MonadReader env m, Glue.HasTexts env, HasStyle env
    , Element.HasAnimIdPrefix env
    ) => [(CallStack, E.InputDoc)] -> m (WithTextPos View)
makeShortcutKeyView inputDocs =
    do
        inputDocColor <- Lens.view (has . styleInputDocColor)
        (Align.vboxAlign ?? 1) <*> Lens.itraverse makeInputDoc inputDocs
            & setColor inputDocColor
    where
        topOf callStack =
            Stack.getCallStack callStack ^? Lens.ix 0
            <&> snd
        fmtSrcLoc srcLoc =
            Stack.srcLocFile srcLoc <> ":" <> show (Stack.srcLocStartLine srcLoc)
            & Text.pack
        srcStr = maybe "<no call stack>" fmtSrcLoc . topOf
        maybeAddSrcLoc callStack mkInputView =
            Lens.view (has . styleSrcLocColor)
            >>= \case
            Nothing -> mkInputView
            Just color ->
                mkInputView /-/
                (Label.make (srcStr callStack <> " ") & setColor color)
        makeInputDoc i (callStack, inputDoc) =
            inputDoc <> " " & Label.make
            & maybeAddSrcLoc callStack
            & Element.locallyAugmented i
        setColor color = Reader.local $ \env -> env & TextView.color .~ color

makeTextViews ::
    ( MonadReader env m, HasStyle env, Glue.HasTexts env
    , Element.HasAnimIdPrefix env
    ) => Tree Text [(CallStack, E.InputDoc)] ->
    m (Tree (WithTextPos View) (WithTextPos View))
makeTextViews =
    go
    where
        go (Leaf s) =
            Leaf <$> makeShortcutKeyView s & Element.locallyAugmented s
        go (Branch n cs) =
            Branch <$> Label.make n <*> traverse go cs & Element.locallyAugmented n

columns :: R -> (a -> R) -> [a] -> [[a]]
columns maxHeight itemHeight =
    combine . foldr step (0, [], [])
    where
        combine (_, curColumn, doneColumns) = curColumn : doneColumns
        step new (curColumnHeight, curColumn, doneColumns)
            | newHeight + curColumnHeight > maxHeight =
                (newHeight, [new], curColumn : doneColumns)
            | otherwise =
                (newHeight + curColumnHeight, new : curColumn, doneColumns)
            where
                newHeight = itemHeight new

make ::
    ( MonadReader env m, Glue.HasTexts env, Has (E.Texts Text) env
    , HasStyle env, Element.HasAnimIdPrefix env
    ) => Vector2 R -> EventMap a -> m View
make size eventMap =
    do
        mkTreeView <- makeTreeView ?? size
        docs <- E.emDocHandlers
        eventMap ^@.. docs
            <&> fromDocHandler . Tuple.swap
            & groupInputDocs & groupTree
            & traverse makeTextViews
            <&> mkTreeView
    where
        fromDocHandler (docHandler, inputDoc) =
            (docHandler ^. E.dhDoc . E.docStrs,
                (docHandler ^. E.dhFileLocation, inputDoc))

makeFromFocus ::
    ( MonadReader env m, Glue.HasTexts env, Has (E.Texts Text) env, HasStyle env
    , Element.HasAnimIdPrefix env
    ) =>
    Widget.Size -> Widget.Focused (f a) -> m View
makeFromFocus size focus =
    Widget.EventContext
    { Widget._eVirtualCursor =
        focus ^. Widget.fFocalAreas & last & State.VirtualCursor
    , Widget._ePrevTextRemainder = mempty
    } & focus ^. Widget.fEventMap
    & make size

makeTooltip ::
    ( MonadReader env m, Element.HasAnimIdPrefix env, Glue.HasTexts env
    , HasStyle env, Has Config env, Has (Texts Text) env
    ) => m View
makeTooltip =
    do
        helpKeys <- Lens.view (has . configOverlayDocKeys) <&> Lens.mapped %~ toModKey
        txt <- Lens.view (has . textShowHelp)
        Label.make txt
            /|/ makeShortcutKeyView
            (helpKeys <&> (,) Stack.callStack . ModKey.pretty)
            <&> (^. Align.tValue)

mkIndent ::
    (Glue.GluesTo env View e r, MonadReader env m) => m (R -> e -> r)
mkIndent = Glue.mkGlue <&> \glue -> glue Glue.Horizontal . Spacer.makeHorizontal

fontHeight :: (MonadReader env m, Has TextView.Style env) => m R
fontHeight =
    Lens.view (has . TextView.styleFont) <&> Font.height

makeFlatTreeView ::
    (MonadReader env m, Has TextView.Style env, Glue.HasTexts env) =>
    m (Vector2 R -> [(View, View)] -> View)
makeFlatTreeView =
    (,,)
    <$> Align.hboxAlign
    <*> (fontHeight <&> Spacer.makeHorizontal)
    <*> GridView.make
    <&> \(box, space, mkGrid) size pairs ->
    let colViews =
            pairs
            & columns (size ^. _2) pairHeight
            <&> map toRow
            <&> mkGrid
            <&> snd
    in  List.intersperse space colViews & box 1
    where
        toRow (titleView, docView) = [Aligned 0 titleView, Aligned (Vector2 1 0) docView]
        pairHeight (titleView, docView) = (max `on` (^. Element.height)) titleView docView

makeTreeView ::
    (MonadReader env m, Has TextView.Style env, Glue.HasTexts env) =>
    m (Vector2 R -> [Tree (WithTextPos View) (WithTextPos View)] -> View)
makeTreeView =
    do
        indent <- mkIndent
        indentWidth <- fontHeight
        box <- Align.vboxAlign
        let go ts = ts <&> fromTree & mconcat
            fromTree (Leaf inputDocsView) = ([], [inputDocsView])
            fromTree (Branch titleView ts) =
                ( (titleView, box 1 inputDocs) :
                    (Lens.traversed . _1 %~ indent indentWidth) titles
                , [] )
                where
                    (titles, inputDocs) = go ts
        let handleResult (pairs, []) =
                pairs <&> Lens.both %~ (^. Align.tValue)
            handleResult _ = error "Leafs at root of tree!"
        makeFlatTreeView
            <&> \mk size trees -> mk size (handleResult (go trees))

hoverEdge ::
    (MonadReader env m, Element.SizedElement a, Has Dir.Layout env) =>
    Widget.Size -> m (a -> a)
hoverEdge size =
    Element.padToSize ?? size ?? 1 <&> ($)

toggle :: IsHelpShown -> IsHelpShown
toggle HelpShown = HelpNotShown
toggle HelpNotShown = HelpShown

helpAnimId :: AnimId
helpAnimId = ["help box"]

addHelpViewWith ::
    (MonadReader env m, HasStyle env, Glue.HasTexts env) =>
    (Widget.Size -> Widget.Focused (f a) -> m View) -> Widget.Size ->
    Widget.Focused (f a) -> m (Widget.Focused (f a))
addHelpViewWith mkHelpView size focus =
    do
        helpView <-
            (.)
            <$> (Element.tint <$> Lens.view (has . styleTint))
            <*> (MDraw.backgroundColor helpAnimId <$> Lens.view (has . styleBGColor))
            <*> mkHelpView size focus
        atEdge <- hoverEdge size ?? helpView
        focus & Widget.fLayers %~ Element.layeredImageAbove (atEdge ^. vAnimLayers) & pure

addHelpView ::
    ( MonadReader env m, HasStyle env
    , Element.HasAnimIdPrefix env, Glue.HasTexts env, Has (E.Texts Text) env
    ) => Widget.Size -> Widget.Focused (f a) -> m (Widget.Focused (f a))
addHelpView = addHelpViewWith makeFromFocus

toggleEventMap ::
    (MonadReader env m, Monoid a, Monad f, Has Config env, Has (Texts Text) env) =>
    Property f IsHelpShown -> m (EventMap (f a))
toggleEventMap showingHelp =
    Lens.view id
    <&>
    \env ->
    let docStr =
            case showingHelp ^. Property.pVal of
            HelpNotShown -> env ^. has . textShow
            HelpShown -> env ^. has . textHide
    in  Property.pureModify showingHelp toggle
        & E.keysEventMap (env ^. has . configOverlayDocKeys)
            ( E.Doc
                [ env ^. has . textHelp
                , env ^. has . textKeyBindings
                , docStr
                ]
            )

helpViewForState ::
    ( MonadReader env m, Element.HasAnimIdPrefix env
    , HasStyle env, Has Config env, HasTexts env
    ) =>
    IsHelpShown -> Widget.Size -> Widget.Focused (f a) -> m View
helpViewForState HelpNotShown = \_ _ -> makeTooltip
helpViewForState HelpShown = makeFromFocus

toggledHelpAdder ::
    ( MonadReader env m, Monad f
    , Element.HasAnimIdPrefix env
    , Has Config env, HasStyle env, HasTexts env
    ) =>
    m (Property f IsHelpShown -> Widget.Size -> Widget f -> Widget f)
toggledHelpAdder =
    Lens.view id <&> \env prop size widget ->
    widget & Widget.wState %~
    \case
    Widget.StateUnfocused {} -> error "adding help to non-focused root widget!"
    Widget.StateFocused makeFocus ->
        makeFocus
        <&> (addHelpViewWith
                (helpViewForState (prop ^. Property.pVal)) size ?? env)
        <&> Widget.fEventMap . Lens.mapped %~
            (toggleEventMap prop env <>)
        & Widget.StateFocused
