-- | Common utilities for status bar widgets
{-# LANGUAGE TemplateHaskell #-}

module Lamdu.GUI.StatusBar.Common
    ( StatusWidget(..), widget, globalEventMap
    , hoist
    , makeSwitchStatusWidget
    , fromWidget, combineEdges
    ) where

import           Control.Lens.Extended (AnItemLens)
import qualified Control.Lens as Lens
import           Control.Monad.Reader.Extended (pushToReader)
import           Data.Property (Property(..))
import           GUI.Momentu (EventMap, ModKey)
import qualified GUI.Momentu as M
import           GUI.Momentu.Element.Id (ElemIds(..))
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.FocusDirection (FocusDirection(..))
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.Hover as Hover
import           GUI.Momentu.Rect (Rect(..))
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.DropDownList as DropDownList
import qualified GUI.Momentu.Widgets.Label as Label
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.StatusBar as Texts

import           Lamdu.Prelude

data StatusWidget f = StatusWidget
    { _widget :: M.TextWidget f
    , _globalEventMap :: EventMap (f M.Update)
    }
Lens.makeLenses ''StatusWidget

instance Functor f => M.Element (StatusWidget f) where
    setLayeredImage = widget . M.setLayeredImage
    hoverLayeredImage = widget %~ M.hoverLayeredImage
    padImpl x y = widget %~ M.padImpl x y
    scale x = widget %~ M.scale x
    empty = StatusWidget M.empty mempty

hoist :: (f M.Update -> g M.Update) -> StatusWidget f -> StatusWidget g
hoist f (StatusWidget w e) =
    StatusWidget
    { _widget = w & M.tValue . Widget.updates %~ f
    , _globalEventMap = e <&> f
    }

fromWidget :: M.TextWidget f -> StatusWidget f
fromWidget w =
    StatusWidget { _widget = w, _globalEventMap = mempty }

makeDropDownList ::
    _ =>
    AnItemLens t (Text, M.ElemId) -> Property f a -> [(a, M.TextWidget f)] -> m (M.TextWidget f)
makeDropDownList headerText prop choices =
    do
        texts <- Lens.view has
        let (text, elemId) = ((,) <$> texts <*> elemIds) ^# headerText
        defConf <- DropDownList.defaultConfig text
        let myId = "status" <> elemId
        DropDownList.make prop choices defConf myId

labeledDropDownList ::
    _ =>
    AnItemLens t (Text, M.ElemId) -> Property f a -> [(a, M.TextWidget f)] -> M.WithTextPos M.View -> m (M.TextWidget f)
labeledDropDownList categoryTextLens prop choices headerView =
    pure headerView M./|/ makeDropDownList categoryTextLens prop choices

makeSwitchStatusWidget ::
    _ =>
    m (M.WithTextPos M.View) ->
    AnItemLens t (Text, M.ElemId) ->
    AnItemLens Texts.StatusBar Text ->
    Lens.ALens' (Config ModKey) [ModKey] -> Property f a ->
    [(a, M.TextWidget f)] -> m (StatusWidget f)
makeSwitchStatusWidget mkHeaderWidget categoryTextLens switchTextLens keysGetter prop choiceVals =
    do
        w <- mkHeaderWidget >>= labeledDropDownList categoryTextLens prop choiceVals
        keys <- Lens.view (has . Lens.cloneLens keysGetter)
        txt <- Lens.view has
        let e =
                setVal newVal
                & E.keysEventMap keys
                (E.toDoc txt
                    [ Texts.sbStatusBar
                    , switchTextLens
                    ])
        pure StatusWidget
            { _widget = w
            , _globalEventMap = e
            }
    where
        choices = choiceVals <&> fst
        newVal = dropWhile (/= curVal) choices ++ choices & tail & head
        Property curVal setVal = prop

hspacer :: _ => m M.View
hspacer = do
    hSpaceCount <- Lens.view (has . Theme.statusBar . Theme.statusBarHSpaces)
    Spacer.getSpaceSize <&> (^. _1) <&> (* hSpaceCount) <&> Spacer.makeHorizontal

hamburgerText :: Text
hamburgerText = "☰"

hamburgerLabel :: _ => m (M.TextWidget f)
hamburgerLabel =
    Label.make hamburgerText >>= M.tValue (Widget.makeFocusableView WidgetIds.statusBarHamburger)
    & Styled.info

-- | Generate an invisible hamburger that responds to cursor so we
-- don't get a red cursor if the hamburger disappears with the cursor
-- on it
makeInvisibleHamburger :: _ => m (M.TextWidget f)
makeInvisibleHamburger = Widget.respondToCursorPrefix WidgetIds.statusBarHamburger M.empty <&> M.WithTextPos 0

enter :: M.Widget f -> Maybe (f M.Update)
enter w =
    w ^? Widget.wState . Widget._StateUnfocused . Widget.uMEnter . Lens._Just
    ?? FromOutside
    <&> (^. Widget.enterResultEvent)

clickTo :: f M.Update -> M.Widget f -> M.Widget f
clickTo action w =
    w
    & Widget.wFocused . Widget.fMEnterPoint ?~ pure enterResult
    & Widget.wState . Widget._StateUnfocused . Widget.uMEnter %~
    \maybeEnter ->
        Just $
        \case
        Point{} -> enterResult
        other -> maybe enterResult ($ other) maybeEnter
    where
        enterResult = Widget.EnterResult
            { Widget._enterResultRect = Rect 0 0
            , Widget._enterResultLayer = 0
            , Widget._enterResultEvent = action
            }

makeHamburgerMenu :: _ => M.TextWidget f -> [M.TextWidget f] -> m (M.TextWidget f)
makeHamburgerMenu hamburger hiddenWidgets =
    do
        actionKeys <- Lens.view (has . Config.actionKeys)
        texts <- Lens.view has
        let doc = E.toDoc texts [Texts.sbStatusBar, Texts.sbExtraOptions]
        let gotoFirstHiddenItem = foldMap (E.keyPresses actionKeys doc) enterFirst
        menu <- Glue.vbox hiddenWidgets
        anc <- M.tValue Hover.anchor hamburger
        hoverOption <-
            pure (M.Aligned 1 anc)
            M./-/ (Hover.hover menu <&> Hover.sequenceHover <&> M.Aligned 1)
        if Widget.isFocused (menu ^. M.tValue)
            then anc & M.tValue %~ Hover.hoverInPlaceOf [hoverOption ^. M.value . M.tValue] & pure
            else
                hamburger
                & M.tValue %~ maybe id clickTo enterFirst
                & M.tValue %~ M.weakerEvents gotoFirstHiddenItem
                & pure
    where
        enterFirst = head hiddenWidgets ^. M.tValue & enter

combineWidgets :: _ => Double -> [StatusWidget f] -> m (StatusWidget f)
combineWidgets width statusWidgets =
    do
        Glue.Poly (|||) <- Glue.mkPoly Glue.Horizontal
        hamburger <- hamburgerLabel
        hamburgerMenu <- makeHamburgerMenu hamburger & pushToReader
        invisibleHamburger <- makeInvisibleHamburger
        space <- hspacer
        let go _ [] = M.empty
            go remainingWidth [w]
                | w ^. M.width > remainingWidth = hamburgerMenu [w]
                | otherwise = w ||| invisibleHamburger
            go remainingWidth (w:ws)
                | newRemaining < hamburger ^. M.width = hamburgerMenu (w:ws)
                | otherwise = wSpaced ||| go newRemaining ws
                where
                    wSpaced = w ||| space
                    newRemaining = remainingWidth - wSpaced ^. M.width
            combined = go width (statusWidgets ^.. Lens.folded . widget)
            paddingWidth = max 0 (width - combined ^. M.width)
            padding = Spacer.makeHorizontal paddingWidth
        pure StatusWidget
            { _widget = padding ||| combined
            , _globalEventMap = statusWidgets ^. Lens.folded . globalEventMap
            }

combineEdges :: _ => Double -> StatusWidget f -> [StatusWidget f] -> m (StatusWidget f)
combineEdges width (StatusWidget topLeftWidget em) topRightWidgets =
    do
        topLeftSpacedWidget <- pure topLeftWidget M./|/ hspacer
        let topRightMaxWidth = width - topLeftSpacedWidget ^. M.width
        topRightCombined <- combineWidgets topRightMaxWidth topRightWidgets
        w <- pure topLeftSpacedWidget M./|/ pure (topRightCombined ^. widget)
        pure StatusWidget
            { _widget = w
            , _globalEventMap = em <> topRightCombined ^. globalEventMap
            }
