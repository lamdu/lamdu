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
    [(a, M.TextWidget f)] -> m (StatusWidget f)
makeSwitchStatusWidget mkHeaderWidget categoryTextLens switchTextLens keysGetter prop choiceVals =
    do
        w <- mkHeaderWidget >>= labeledDropDownList categoryTextLens prop choiceVals
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

combine :: _ => m ([StatusWidget f] -> StatusWidget f)
combine =
    (,,) <$> (Glue.mkPoly ?? Glue.Horizontal) <*> Glue.hbox <*> hspacer
    <&> \(Glue.Poly (|||), hbox, space) statusWidgets ->
    StatusWidget
    { _widget =
        case statusWidgets of
        [] -> M.empty
        (x:xs) ->
            xs
            <&> (^. widget)
            <&> (space |||)
            & hbox
            & ((x ^. widget) |||)
    , _globalEventMap = statusWidgets ^. Lens.folded . globalEventMap
    }

combineEdges :: _ => m (Double -> StatusWidget f -> StatusWidget f -> StatusWidget f)
combineEdges =
    Glue.mkPoly ?? Glue.Horizontal
    <&> \(Glue.Poly (|||)) width (StatusWidget xw xe) (StatusWidget yw ye) ->
    let padding = max 0 (width - combinedWidths)
        combinedWidths = xw ^. M.width + yw ^. M.width
    in  StatusWidget
        { _widget = xw ||| Spacer.makeHorizontal padding ||| yw
        , _globalEventMap = xe <> ye
        }
