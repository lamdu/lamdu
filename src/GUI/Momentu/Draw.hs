{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
-- | Draw on elements

module GUI.Momentu.Draw
    ( addInnerFrame, backgroundColor
    ) where

import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Animation (R)
import qualified GUI.Momentu.Animation as Anim
import           GUI.Momentu.Element
import qualified GUI.Momentu.View as View
import qualified Graphics.DrawingCombinators as Draw

import           Lamdu.Prelude

backgroundColor ::
    (MonadReader env m, HasAnimIdPrefix env, Element a) =>
    m (Draw.Color -> a -> a)
backgroundColor =
    subAnimId ["bg"] <&>
    \animId color -> setLayers %@~ \sz x ->
    x
    & View.layers %~ addBg (Anim.backgroundColor animId color sz)
    where
        addBg bg [] = [bg]
        addBg bg (x:xs) = x <> bg : xs

addInnerFrame ::
    (MonadReader env m, HasAnimIdPrefix env, Element a) =>
    m (Draw.Color -> Vector2 R -> a -> a)
addInnerFrame =
    subAnimId ["inner-frame"] <&>
    \animId color frameWidth -> bottomLayer %@~ \sz ->
    mappend
    ( Anim.emptyRectangle frameWidth sz animId
        & Anim.unitImages %~ Draw.tint color
    )
