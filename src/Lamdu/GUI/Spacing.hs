{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}
module Lamdu.GUI.Spacing
    ( HasStdSpacing(..)
    , getSpaceSize
    , stdHSpaceView
    , stdVSpaceView
    , vspacer
    , hboxCenteredSpaced
    ) where

import qualified Control.Lens as Lens
import           Data.List (intersperse)
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.View (View)
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView

import           Lamdu.Prelude

stdFont :: (MonadReader env m, TextView.HasStyle env) => m Draw.Font
stdFont = Lens.view (TextView.style . TextView.styleFont)

class TextView.HasStyle env => HasStdSpacing env where
    stdSpacing :: Lens' env (Vector2 Double)

-- | Vertical spacer as ratio of line height
vspacer :: (MonadReader env m, TextView.HasStyle env) => Double -> m (Widget f)
vspacer ratio =
    stdFont <&> Draw.fontHeight <&> (ratio *) <&> Spacer.makeVertical
    <&> Widget.fromView

getSpaceSize :: (MonadReader env m, HasStdSpacing env) => m (Vector2 Double)
getSpaceSize =
    do
        font <- stdFont
        factor <- Lens.view stdSpacing
        factor * Vector2 (Draw.textAdvance font " ") (Draw.fontHeight font)
            & pure

stdHSpaceView :: (MonadReader env m, HasStdSpacing env) => m View
stdHSpaceView = getSpaceSize <&> _2 .~ 0 <&> Spacer.make

stdVSpaceView :: (MonadReader env m, HasStdSpacing env) => m View
stdVSpaceView = getSpaceSize <&> _1 .~ 0 <&> Spacer.make

hboxCenteredSpaced ::
    (MonadReader env m, HasStdSpacing env) => [Widget f] -> m (Widget f)
hboxCenteredSpaced widgets =
    stdHSpaceView
    <&> Widget.fromView
    <&> Box.hboxAlign 0.5 . (`intersperse` widgets)
