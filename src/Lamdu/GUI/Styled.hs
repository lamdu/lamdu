-- | Styled widgets
-- Apply the Lamdu theme to various widgets and guis
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.Styled
    ( grammarLabel, grammarText
    , addValBG, addValBGWithColor
    , addValPadding, addValFrame
    , addDeletionDiagonal
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           GUI.Momentu.Align (WithTextPos(..))
import           GUI.Momentu.Animation (AnimId)
import qualified GUI.Momentu.Animation as Anim
import qualified GUI.Momentu.Draw as Draw
import           GUI.Momentu.Element (Element)
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Lamdu.Config.Theme (Theme, HasTheme(..))
import qualified Lamdu.Config.Theme as Theme

import           Lamdu.Prelude

grammarLabel ::
    ( MonadReader env m
    , HasTheme env
    , TextView.HasStyle env
    , Element.HasAnimIdPrefix env
    ) => Text -> m (WithTextPos View)
grammarLabel text =
    do
        th <- Lens.view theme <&> Theme.codeForegroundColors
        TextView.makeLabel text
            & Reader.local (TextView.color .~ Theme.grammarColor th)

grammarText ::
    ( MonadReader env m
    , HasTheme env
    , TextView.HasStyle env
    ) => m (Text -> AnimId -> WithTextPos View)
grammarText =
    do
        th <- Lens.view theme <&> Theme.codeForegroundColors
        TextView.make
            & Reader.local (TextView.color .~ Theme.grammarColor th)

addValBG ::
    ( MonadReader env m, Element a
    , Element.HasAnimIdPrefix env, HasTheme env
    ) => m (a -> a)
addValBG = addValBGWithColor Theme.valFrameBGColor

addValBGWithColor ::
    ( MonadReader env m, Element a
    , Element.HasAnimIdPrefix env, HasTheme env
    ) => (Theme -> Draw.Color) -> m (a -> a)
addValBGWithColor color =
    Draw.backgroundColor <*> (Lens.view Theme.theme <&> color)

addValPadding :: (MonadReader env m, Element a, HasTheme env) => m (a -> a)
addValPadding =
    Lens.view Theme.theme <&> Theme.valFramePadding <&> fmap realToFrac
    <&> Element.pad

addValFrame ::
    ( MonadReader env m, Element a, Element.HasAnimIdPrefix env, HasTheme env
    ) => m (a -> a)
addValFrame =
    (.)
    <$> addValBG
    <*> addValPadding
    & Reader.local (Element.animIdPrefix <>~ ["val"])

-- | Add a diagonal line (top-left to right-bottom). Useful as a
-- "deletion" GUI annotation
addDiagonal ::
    (MonadReader env m, Element.HasAnimIdPrefix env, Element a) =>
    m (Draw.Color -> Draw.R -> a -> a)
addDiagonal =
    Element.subAnimId ["diagonal"] <&>
    \animId color thickness -> Element.topLayer %@~
    \sz ->
    Draw.convexPoly
    [ (0, thickness)
    , (0, 0)
    , (thickness, 0)
    , (1, 1-thickness)
    , (1, 1)
    , (1-thickness, 1)
    ]
    & Draw.tint color
    & void
    & Anim.singletonFrame 1 (animId ++ ["diagonal"])
    & Anim.scale sz
    & flip mappend

addDeletionDiagonal ::
    (MonadReader env m, Element a, Element.HasAnimIdPrefix env, HasTheme env) =>
    m (Widget.R -> a -> a)
addDeletionDiagonal =
    addDiagonal <*> (Lens.view Theme.theme <&> Theme.typeIndicatorErrorColor)
