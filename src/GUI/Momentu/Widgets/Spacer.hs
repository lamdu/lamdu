{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module GUI.Momentu.Widgets.Spacer
    ( make
    , makeHorizontal, makeVertical
    , vspaceLines
    , HasStdSpacing(..), getSpaceSize , stdHSpace, stdVSpace
    ) where

import qualified Control.Lens as Lens
import           Data.Vector.Vector2 (Vector2(..))
import qualified GUI.Momentu.Animation as Anim
import           GUI.Momentu.View (View(..))
import qualified GUI.Momentu.View as View
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Graphics.DrawingCombinators as Draw

import           Lamdu.Prelude

make :: View.Size -> View
make size = View size mempty

makeHorizontal :: Anim.R -> View
makeHorizontal width = make $ Vector2 width 0

makeVertical :: Anim.R -> View
makeVertical height = make $ Vector2 0 height

stdFont :: (MonadReader env m, TextView.HasStyle env) => m Draw.Font
stdFont = Lens.view (TextView.style . TextView.styleFont)

vspaceLines :: (MonadReader env m, TextView.HasStyle env) => Double -> m View
vspaceLines numLines = stdFont <&> Draw.fontHeight <&> (numLines *) <&> makeVertical

class TextView.HasStyle env => HasStdSpacing env where
    stdSpacing :: Lens' env (Vector2 Double)

getSpaceSize :: (MonadReader env m, HasStdSpacing env) => m (Vector2 Double)
getSpaceSize =
    do
        font <- stdFont
        factor <- Lens.view stdSpacing
        factor * Vector2 (Draw.textAdvance font " ") (Draw.fontHeight font)
            & pure

stdHSpace :: (MonadReader env m, HasStdSpacing env) => m View
stdHSpace = getSpaceSize <&> _2 .~ 0 <&> make

stdVSpace :: (MonadReader env m, HasStdSpacing env) => m View
stdVSpace = getSpaceSize <&> _1 .~ 0 <&> make
