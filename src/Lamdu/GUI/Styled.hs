{-# LANGUAGE RankNTypes #-}
-- | Styled widgets
-- Apply the Lamdu theme to various widgets and guis
module Lamdu.GUI.Styled
    ( grammar, info
    , text
    , mkLabel, mkFocusableLabel, OneOfT(..)
    , label, focusableLabel
    , addValBG, addBgColor
    , addValPadding, addValFrame
    , deletedDef, deletedUse
    , actionable
    , withColor
    , nameAtBinder
    , unitSprite, sprite
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Extended (OneOf)
import qualified Control.Monad.Reader as Reader
import qualified GUI.Momentu as M
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Animation as Anim
import           GUI.Momentu.Animation.Id (ElemIds(..))
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Font as Font
import qualified GUI.Momentu.View as View
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Clickable as Clickable
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Graphics.DrawingCombinators.Extended ((%%))
import qualified Graphics.DrawingCombinators.Extended as GLDraw
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Config.Theme.Sprites (Sprites)
import           Lamdu.Config.Theme.TextColors (TextColors)
import qualified Lamdu.Config.Theme.TextColors as TextColors
import           Lamdu.Name (Name(..))
import qualified Lamdu.Name as Name
import qualified Lamdu.Style as Style

import           Lamdu.Prelude

info :: _ => m a -> m a
info = withColor TextColors.infoTextColor

grammar :: _ => m a -> m a
grammar = withColor TextColors.grammarColor

rawText :: _ => M.AnimId -> Text -> f (M.WithTextPos M.View)
rawText animIdSuffix txt =
    (TextView.make ?? txt) <*> (Element.subAnimId ?? animIdSuffix)

text :: _ => M.AnimId -> OneOf t -> f (M.WithTextPos M.View)
text animIdSuffix txtLens =
    Lens.view (has . Lens.cloneLens txtLens)
    >>= rawText animIdSuffix

-- work around lack of impredicative types
newtype OneOfT a = OneOf (OneOf a)

mkLabel :: _ => m (OneOfT t -> M.WithTextPos M.View)
mkLabel =
    (,,) <$> TextView.make <*> Element.subAnimId <*> Lens.view has
    <&> \(textView, subAnimId, texts) (OneOf lens) ->
    textView (texts ^# lens) (subAnimId (elemIds ^# lens))

mkFocusableLabel :: _ => m (OneOfT t -> M.TextWidget f)
mkFocusableLabel =
    (,,) <$> Widget.makeFocusableView <*> Lens.view Element.animIdPrefix <*> mkLabel
    <&> \(toFocusable, animIdPrefix, lbl) (OneOf lens) ->
        let widgetId = animIdPrefix <> elemIds ^# lens & Widget.Id
        in  lbl (OneOf lens) & Align.tValue %~ toFocusable widgetId

label :: _ => OneOf t -> m (M.WithTextPos M.View)
label lens = mkLabel ?? OneOf lens

focusableLabel :: _ => OneOf t -> m (M.TextWidget f)
focusableLabel lens = mkFocusableLabel ?? OneOf lens

addValBG :: _ => m (a -> a)
addValBG = addBgColor Theme.valFrameBGColor

addBgColor :: _ => Lens.ALens' Theme Draw.Color -> m (a -> a)
addBgColor getColor =
    Draw.backgroundColor <*> Lens.view (has . Lens.cloneLens getColor)

addValPadding :: _ => m (a -> a)
addValPadding =
    Lens.view (has . Theme.valFramePadding) <&> Element.padAround

addValFrame :: _ => m (a -> a)
addValFrame =
    (.)
    <$> addValBG
    <*> addValPadding
    & Reader.local (Element.animIdPrefix <>~ ["val"])

-- | Add a diagonal line (top-left to right-bottom).
addDiagonal :: _ => m (Draw.Color -> Draw.R -> a -> a)
addDiagonal =
    Element.subAnimId ?? ["diagonal"] <&>
    \animId color thickness ->
        let mkFrame sz =
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
        in  Element.setLayeredImage <. Element.layers %@~ snoc . mkFrame
    where
        snoc x xs = xs ++ [x]

deletedDiagonal :: _ => Lens.Getting Widget.R Theme.Deleted Widget.R -> m (a -> a)
deletedDiagonal widthLens =
    do
        width <- Lens.view (has . Theme.deleted . widthLens)
        addDiagonal <*> Lens.view (has . Theme.errorColor) ?? width

deletedUse :: _ => m (a -> a)
deletedUse = deletedDiagonal Theme.deletedUseDiagonalWidth

deletedDef :: _ => m (a -> a)
deletedDef =
    (.)
    <$> deletedDiagonal Theme.deletedDefDiagonalWidth
    <*> (Lens.view (has . Theme.deleted . Theme.deletedDefTint) <&> Element.tint)

withColor :: _ => Lens.ALens' TextColors Draw.Color -> m a -> m a
withColor textColor act =
    do
        color <- Lens.view (has . Theme.textColors . Lens.cloneLens textColor)
        Reader.local (TextView.color .~ color) act

actionable :: _ => Widget.Id -> OneOf t -> E.Doc -> f Widget.Id -> m (M.TextWidget f)
actionable myId txtLens doc action =
    do
        color <- Lens.view (has . Theme.textColors . TextColors.actionTextColor)
        underlineWidth <- Lens.view (has . Theme.narrowUnderlineWidth)
        let underline =
                Font.Underline
                { Font._underlineColor = color
                , Font._underlineWidth = underlineWidth
                }
        actionKeys <- Lens.view (has . Config.actionKeys)
        let eventMap = E.keysEventMapMovesCursor actionKeys doc action
        Lens.view has <&> (^. txtLens)
            >>= Clickable.makeText myId (Clickable.Config action doc actionKeys)
            & Reader.local (TextView.color .~ color)
            & Reader.local (TextView.underline ?~ underline)
            <&> Align.tValue %~ Widget.weakerEvents eventMap

nameAtBinder :: _ => Name -> m a -> m a
nameAtBinder name act =
    do
        textEditStyle <- Lens.view (has . style)
        act & Reader.local (has .~ textEditStyle)
    where
        style =
            case name of
            Name.NameTag t
                | not (t ^. Name.tnIsAutoGen) ->
                    Style.nameAtBinder
            _ -> Style.autoNameOrigin

-- Sprite the size of unit (1 pixel)
unitSprite :: _ => OneOf Sprites -> m M.View
unitSprite lens =
    Lens.view id
    <&> \env ->
    Draw.sprite (env ^. has . Lens.cloneLens lens)
    & void
    -- Y goes up by default in DrawingCombinators, switch that
    & (GLDraw.scaleV (M.Vector2 1 (-1)) %%)
    -- (-1..1) -> (0..2)
    & (GLDraw.translateV 1 %%)
    -- (0..2) -> (0..1)
    & (GLDraw.scaleV (M.Vector2 0.5 0.5) %%)
    & Anim.singletonFrame 1 (elemIds ^# lens)
    & View.make 1

sprite :: _ => OneOf Sprites -> m M.View
sprite lens =
    do
        height <- Lens.view has <&> TextView.lineHeight
        unitSprite lens <&> Element.scale (realToFrac height)
