{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.UI.Bottle.Widgets.EventMapDoc(make, makeWidget) where

import Graphics.UI.Bottle.EventMap(EventMap)
import Graphics.UI.Bottle.Sized (Sized(..))
import Graphics.UI.Bottle.Widget (Widget, liftView)
import qualified Data.ByteString.Char8 as SBS8
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widgets.GridView as GridView
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView

make :: EventMap a -> TextView.Style -> Anim.AnimId -> Sized Anim.Frame
make eventMap style animId =
  GridView.make . map toRow . E.eventMapDocs $ eventMap
  where
    textView str uniq =
      TextView.make style str $
      Anim.joinId animId [SBS8.pack str, uniq]
    toRow (eventName, eventDoc) =
      [textView eventName "name",
       textView eventDoc "doc"]

makeWidget :: EventMap a -> TextView.Style -> Anim.AnimId -> Widget f
makeWidget = (fmap . fmap . fmap) liftView make
