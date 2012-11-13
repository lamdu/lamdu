{-# LANGUAGE OverloadedStrings #-}
module Graphics.UI.Bottle.Widgets.EventMapDoc(make, addHelp, makeToggledHelpAdder) where

import Control.Lens ((^.))
import Data.IORef (newIORef, readIORef, modifyIORef)
import Data.List.Utils (groupOn, sortOn)
import Data.Monoid (mappend)
import Graphics.UI.Bottle.EventMap (EventMap)
import Graphics.UI.Bottle.Widget (Widget)
import qualified Control.Lens as Lens
import qualified Data.ByteString.Char8 as SBS8
import qualified Data.Tuple as Tuple
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.GridView as GridView
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView

groupByKey :: Eq b => (a -> (b, c)) -> [a] -> [(b, [c])]
groupByKey f =
  map perGroup . groupOn fst . map f
  where
    perGroup xs = (fst (head xs), map snd xs)

make :: EventMap a -> TextView.Style -> Anim.AnimId -> (Anim.Size, Anim.Frame)
make eventMap style animId =
  GridView.makeAlign 0 .
  map toRow .
  groupByKey Tuple.swap .
  sortOn snd $ E.eventMapDocs eventMap
  where
    textView uniq str =
      TextView.make style str $
      Anim.joinId animId (map SBS8.pack [str, uniq])
    toRow (eventDoc, eventKeys) =
      [ GridView.makeAlign 0
        [concatMap ((: [Spacer.makeHorizontal 8]) . textView "key") eventKeys]
      , textView "doc" eventDoc]

addHelp :: Widget.Size -> TextView.Style -> Widget f -> Widget f
addHelp size style w =
  Lens.over Widget.wFrame (mappend docFrame) w
  where
    (eventMapSize, eventMapDoc) = make eventMap style ["help box"]
    transparency = Draw.Color 1 1 1
    docFrame =
      (Anim.onImages . Draw.tint . transparency) 0.8 .
      Anim.onDepth (subtract 10) .
      Anim.translate (size - eventMapSize) .
      Anim.backgroundColor
      ["help doc background"] 1 (Draw.Color 0.3 0.2 0.1 1) eventMapSize $
      eventMapDoc
    eventMap = w ^. Widget.wEventMap

makeToggledHelpAdder
  :: [E.ModKey] -> IO (TextView.Style -> Widget.Size -> Widget IO -> IO (Widget IO))
makeToggledHelpAdder overlayDocKeys = do
  showingHelpVar <- newIORef True
  let
    toggle = modifyIORef showingHelpVar not
    addToggleEventMap doc =
      Widget.strongerEvents $
      Widget.keysEventMap overlayDocKeys doc toggle
  return $ \style size widget -> do
    showingHelp <- readIORef showingHelpVar
    return $
      if showingHelp
      then addHelp size style $ addToggleEventMap "Hide Key Bindings" widget
      else addToggleEventMap "Show Key Bindings" widget
