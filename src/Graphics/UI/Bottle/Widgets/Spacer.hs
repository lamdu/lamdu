{-# LANGUAGE NoImplicitPrelude #-}
module Graphics.UI.Bottle.Widgets.Spacer
    ( make
    , makeHorizontal, makeVertical
    ) where

import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.UI.Bottle.Animation as Anim
import           Graphics.UI.Bottle.View (View(..))
import qualified Graphics.UI.Bottle.View as View

import           Lamdu.Prelude

make :: View.Size -> View
make size = View size mempty

makeHorizontal :: Anim.R -> View
makeHorizontal width = make $ Vector2 width 0

makeVertical :: Anim.R -> View
makeVertical height = make $ Vector2 0 height
