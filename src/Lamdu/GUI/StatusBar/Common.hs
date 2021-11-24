-- | Common utilities for status bar widgets
{-# LANGUAGE TemplateHaskell, RankNTypes, TypeFamilies #-}

module Lamdu.GUI.StatusBar.Common
    ( StatusWidget(..), widget, globalEventMap
    , hoist
    , makeSwitchStatusWidget
    , fromWidget, combine, combineEdges
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Extended (OneOf)
import           Data.Property (Property(..))
import qualified GUI.Momentu as M
import           GUI.Momentu.Animation.Id (ElemIds(..))
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Glue as Glue
import           GUI.Momentu.ModKey (ModKey)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.DropDownList as DropDownList
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import           Lamdu.Config (Config)
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.I18N.StatusBar as Texts

import           Lamdu.Prelude

data StatusWidget m f = StatusWidget
    { _widget :: m (M.TextWidget f)
    , _globalEventMap :: EventMap (f M.Update)
    }
Lens.makeLenses ''StatusWidget

instance (Functor f, Applicative m) => M.Element (StatusWidget m f) where
    setLayeredImage = widget . Lens.mapped . M.setLayeredImage
    hoverLayeredImage = widget . Lens.mapped %~ M.hoverLayeredImage
    padImpl x y = widget . Lens.mapped %~ M.padImpl x y
    scale x = widget . Lens.mapped %~ M.scale x
    empty = StatusWidget (pure M.empty) mempty

hoist :: Functor m => (f M.Update -> g M.Update) -> StatusWidget m f -> StatusWidget m g
hoist f (StatusWidget w e) =
    StatusWidget
    { _widget = w <&> M.tValue . Widget.updates %~ f
    , _globalEventMap = e <&> f
    }

fromWidget :: m (M.TextWidget f) -> StatusWidget m f
fromWidget w =
    StatusWidget { _widget = w, _globalEventMap = mempty }

makeDropDownList :: _ => OneOf t -> Property f a -> [(a, M.TextWidget f)] -> m (M.TextWidget f)
makeDropDownList headerText prop choices =
    do
        defConf <- DropDownList.defaultConfig
        text <- Lens.view (has . headerText)
        DropDownList.make ?? prop ?? choices ?? defConf text ?? myId
    where
        myId = Widget.Id ("status" : elemIds ^# headerText)

labeledDropDownList ::
    _ => OneOf t -> Property f a -> [(a, M.TextWidget f)] -> M.WithTextPos M.View -> m (M.TextWidget f)
labeledDropDownList categoryTextLens prop choices headerView =
    pure headerView M./|/ makeDropDownList categoryTextLens prop choices

makeSwitchStatusWidget ::
    _ =>
    m (M.WithTextPos M.View) -> OneOf t -> OneOf Texts.StatusBar -> Lens' (Config ModKey) [ModKey] -> Property f a ->
    [(a, M.TextWidget f)] -> m (StatusWidget m f)
makeSwitchStatusWidget mkHeaderWidget categoryTextLens switchTextLens keysGetter prop choiceVals =
    do
        keys <- Lens.view (has . keysGetter)
        txt <- Lens.view has
        let e =
                setVal newVal
                & E.keysEventMap keys
                (E.toDoc txt
                    [ Texts.sbStatusBar
                    , switchTextLens
                    ])
        pure StatusWidget
            { _widget = mkHeaderWidget >>= labeledDropDownList categoryTextLens prop choiceVals
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

combine :: _ => [StatusWidget m f] -> StatusWidget m f
combine statusWidgets =
    StatusWidget
    { _widget =
        do
            Glue.Poly (|||) <- Glue.mkPoly ?? Glue.Horizontal
            hbox <- Glue.hbox
            space <- hspacer
            case statusWidgets of
                [] -> pure M.empty
                (x:xs) ->
                    do
                        xw <- x ^. widget
                        xsw <- traverse (^. widget) xs <&> map (space |||) <&> hbox
                        xw ||| xsw & pure
    , _globalEventMap = statusWidgets ^. Lens.folded . globalEventMap
    }

combineEdges :: _ => Double -> StatusWidget m f -> StatusWidget m f -> StatusWidget m f
combineEdges width (StatusWidget mkX xe) (StatusWidget mkY ye) =
    StatusWidget
    { _widget =
        do
            Glue.Poly (|||) <- Glue.mkPoly ?? Glue.Horizontal
            xw <- mkX
            yw <- mkY
            let combinedWidths = xw ^. M.width + yw ^. M.width
            let padding = max 0 (width - combinedWidths)
            xw ||| Spacer.makeHorizontal padding ||| yw & pure
    , _globalEventMap = xe <> ye
    }
