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
    , sprite
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Extended (OneOf)
import qualified Control.Monad.Reader as Reader
import           GUI.Momentu.Align (WithTextPos(..), TextWidget)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Animation as Anim
import           GUI.Momentu.Animation.Id (AnimId, ElemIds(..))
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.Draw as Draw
import           GUI.Momentu.Element (Element)
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Font as Font
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.State as State
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.View as View
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Graphics.DrawingCombinators.Extended as GLDraw
import           Graphics.DrawingCombinators.Extended ((%%))
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Config.Theme.TextColors (TextColors)
import qualified Lamdu.Config.Theme.TextColors as TextColors
import           Lamdu.Name (Name(..))
import qualified Lamdu.Name as Name
import qualified Lamdu.Style as Style

import           Lamdu.Prelude

info :: (MonadReader env m, Has Theme env, Has TextView.Style env) => m a -> m a
info = withColor TextColors.infoTextColor

grammar :: (MonadReader env m, Has Theme env, Has TextView.Style env) => m a -> m a
grammar = withColor TextColors.grammarColor

rawText ::
    ( MonadReader env f, Has TextView.Style env, Element.HasAnimIdPrefix env
    , Has Dir.Layout env
    ) =>
    AnimId -> Text -> f (WithTextPos View)
rawText animIdSuffix txt =
    (TextView.make ?? txt) <*> (Element.subAnimId ?? animIdSuffix)

text ::
    ( MonadReader env f, Has TextView.Style env, Element.HasAnimIdPrefix env
    , Has (t Text) env, Has Dir.Layout env
    ) =>
    AnimId -> OneOf t -> f (WithTextPos View)
text animIdSuffix txtLens =
    Lens.view (has . Lens.cloneLens txtLens)
    >>= rawText animIdSuffix

-- work around lack of impredicative types
newtype OneOfT a = OneOf (OneOf a)

mkLabel ::
    ( MonadReader env m, Has TextView.Style env, Element.HasAnimIdPrefix env
    , Has (t Text) env, Has Dir.Layout env, ElemIds t
    ) =>
    m (OneOfT t -> WithTextPos View)
mkLabel =
    (,,) <$> TextView.make <*> Element.subAnimId <*> Lens.view has
    <&> \(textView, subAnimId, texts) (OneOf lens) ->
    textView (texts ^# lens) (subAnimId (elemIds ^# lens))

mkFocusableLabel ::
    ( MonadReader env m, Applicative f, State.HasCursor env
    , Has TextView.Style env, Element.HasAnimIdPrefix env
    , Has (t Text) env, ElemIds t, Has Dir.Layout env
    ) =>
    m (OneOfT t -> TextWidget f)
mkFocusableLabel =
    (,,) <$> Widget.makeFocusableView <*> Lens.view Element.animIdPrefix <*> mkLabel
    <&> \(toFocusable, animIdPrefix, lbl) (OneOf lens) ->
        let widgetId = animIdPrefix <> elemIds ^# lens & Widget.Id
        in  lbl (OneOf lens) & Align.tValue %~ toFocusable widgetId

label ::
    ( MonadReader env m, Has TextView.Style env, Element.HasAnimIdPrefix env
    , Has (t Text) env, ElemIds t, Has Dir.Layout env
    ) =>
    OneOf t -> m (WithTextPos View)
label lens = mkLabel ?? OneOf lens

focusableLabel ::
    ( MonadReader env m, Applicative f, State.HasCursor env
    , Has TextView.Style env, Element.HasAnimIdPrefix env
    , Has (t Text) env, ElemIds t, Has Dir.Layout env
    ) =>
    OneOf t -> m (TextWidget f)
focusableLabel lens = mkFocusableLabel ?? OneOf lens

addValBG ::
    ( MonadReader env m, Element a
    , Element.HasAnimIdPrefix env, Has Theme env
    ) => m (a -> a)
addValBG = addBgColor Theme.valFrameBGColor

addBgColor ::
    ( MonadReader env m, Element a
    , Element.HasAnimIdPrefix env, Has Theme env
    ) => Lens.ALens' Theme Draw.Color -> m (a -> a)
addBgColor getColor =
    Draw.backgroundColor <*> Lens.view (has . Lens.cloneLens getColor)

addValPadding :: (MonadReader env m, Element a, Has Theme env) => m (a -> a)
addValPadding =
    Lens.view (has . Theme.valFramePadding) <&> Element.padAround

addValFrame ::
    ( MonadReader env m, Element a, Element.HasAnimIdPrefix env, Has Theme env
    ) => m (a -> a)
addValFrame =
    (.)
    <$> addValBG
    <*> addValPadding
    & Reader.local (Element.animIdPrefix <>~ ["val"])

-- | Add a diagonal line (top-left to right-bottom).
addDiagonal ::
    (MonadReader env m, Element.HasAnimIdPrefix env, Element a) =>
    m (Draw.Color -> Draw.R -> a -> a)
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
        in  Element.setLayers <. Element.layers %@~ snoc . mkFrame
    where
        snoc x xs = xs ++ [x]

deletedDiagonal ::
    ( MonadReader env m, Has Theme env, Element a, Element.HasAnimIdPrefix env
    ) =>
    Lens.Getting Widget.R Theme.Deleted Widget.R -> m (a -> a)
deletedDiagonal widthLens =
    do
        width <- Lens.view (has . Theme.deleted . widthLens)
        addDiagonal <*> Lens.view (has . Theme.errorColor) ?? width

deletedUse ::
    (MonadReader env m, Element a, Element.HasAnimIdPrefix env, Has Theme env) =>
    m (a -> a)
deletedUse = deletedDiagonal Theme.deletedUseDiagonalWidth

deletedDef ::
    (MonadReader env m, Element a, Element.HasAnimIdPrefix env, Has Theme env) =>
    m (a -> a)
deletedDef =
    (.)
    <$> deletedDiagonal Theme.deletedDefDiagonalWidth
    <*> (Lens.view (has . Theme.deleted . Theme.deletedDefTint) <&> Element.tint)

withColor ::
    (MonadReader env m, Has Theme env, Has TextView.Style env) =>
    Lens.ALens' TextColors Draw.Color -> m a -> m a
withColor textColor act =
    do
        color <- Lens.view (has . Theme.textColors . Lens.cloneLens textColor)
        Reader.local (TextView.color .~ color) act

actionable ::
    ( Element.HasAnimIdPrefix env, Has TextView.Style env
    , GuiState.HasCursor env, Has Config env, Has Theme env
    , Applicative f, MonadReader env m, Has Dir.Layout env
    , Has (t Text) env, ElemIds t
    ) =>
    Widget.Id -> OneOf t -> E.Doc -> f Widget.Id -> m (TextWidget f)
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
        (Widget.makeFocusableView ?? myId <&> (Align.tValue %~))
            <*> label txtLens
            & Reader.local (TextView.color .~ color)
            & Reader.local (TextView.underline ?~ underline)
            <&> Align.tValue %~ Widget.weakerEvents eventMap

nameAtBinder ::
    (MonadReader env m, Style.HasStyle env) => Name n -> m b -> m b
nameAtBinder name act =
    do
        style <- Lens.view has
        let textEditStyle =
                style
                ^. case name of
                    Name.AutoGenerated {} -> Style.autoNameOrigin
                    Name.Unnamed {}       -> Style.autoNameOrigin
                    Name.Stored {}        -> Style.nameAtBinder
        act & Reader.local (has .~ textEditStyle)

sprite ::
    (MonadReader env m, Style.HasStyle env) =>
    AnimId -> Lens.ALens' Style.LoadedSprites Draw.Sprite -> m View
sprite animId lens =
    Lens.view (has . Style.sprites . Lens.cloneLens lens)
    <&> Draw.sprite
    <&> void
    -- (-1..1) -> (0..2)
    <&> (GLDraw.translateV 1 %%)
    -- (0..2) -> (0..1)
    <&> (GLDraw.scaleV 0.5 %%)
    <&> \img -> Anim.singletonFrame 1 animId img & View.make 1
