-- | Styled widgets
-- Apply the Lamdu theme to various widgets and guis
module Lamdu.GUI.Styled
    ( grammar, info
    , text
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
import           Control.Lens.Extended (AnItemLens)
import qualified GUI.Momentu as M
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Animation as Anim
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.Element.Id (ElemId(..), ElemIds(..))
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

rawText :: _ => M.ElemId -> Text -> f (M.WithTextPos M.View)
rawText elemIdSuffix txt = Element.subElemId elemIdSuffix >>= TextView.make txt

text :: _ => M.ElemId -> Lens.ALens' (t Text) Text -> f (M.WithTextPos M.View)
text elemIdSuffix txtLens =
    Lens.view (has . Lens.cloneLens txtLens)
    >>= rawText elemIdSuffix

label :: _ => AnItemLens t (Text, M.ElemId) -> m (M.WithTextPos M.View)
label lens =
    do
        texts <- Lens.view has
        let (txt, elemId) = ((,) <$> texts <*> elemIds) ^# lens
        Element.subElemId elemId >>= TextView.make txt

focusableLabel :: _ => AnItemLens t (Text, M.ElemId) -> m (M.TextWidget f)
focusableLabel lens =
    do
        elemIdPrefix <- Lens.view Element.elemIdPrefix
        let widgetId = elemIdPrefix <> (elemIds <&> (,) "") ^. Lens.cloneLens lens . Lens._2
        label lens >>= Align.tValue (Widget.makeFocusableView widgetId)

addValBG :: _ => a -> m a
addValBG = addBgColor Theme.valFrameBGColor

addBgColor :: _ => Lens.ALens' Theme Draw.Color -> a -> m a
addBgColor getColor x =
    Lens.view (has . Lens.cloneLens getColor) >>= (`Draw.backgroundColor` x)

addValPadding :: _ => a -> m a
addValPadding x =
    Lens.view (has . Theme.valFramePadding) <&> (`Element.padAround` x)

addValFrame :: _ => a -> m a
addValFrame x =
    addValPadding x >>= addValBG
    & local (Element.elemIdPrefix <>~ "val")

-- | Add a diagonal line (top-left to right-bottom).
addDiagonal :: _ => Draw.Color -> Draw.R -> a -> m a
addDiagonal color thickness w =
    Element.subElemId "diagonal" <&>
    \elemId ->
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
                & Anim.singletonFrame 1 (elemId <> "diagonal")
                & Anim.scale sz
        in  w & Element.setLayeredImage <. Element.layers %@~ snoc . mkFrame
    where
        snoc x xs = xs ++ [x]

deletedDiagonal :: _ => Lens.Getting Widget.R Theme.Deleted Widget.R -> a -> m a
deletedDiagonal widthLens w =
    do
        color <- Lens.view (has . Theme.errorColor)
        width <- Lens.view (has . Theme.deleted . widthLens)
        addDiagonal color width w

deletedUse :: _ => a -> m a
deletedUse = deletedDiagonal Theme.deletedUseDiagonalWidth

deletedDef :: _ => a -> m a
deletedDef w =
    Lens.view (has . Theme.deleted . Theme.deletedDefTint)
    >>= deletedDiagonal Theme.deletedDefDiagonalWidth . (`Element.tint` w)

withColor :: _ => Lens.ALens' TextColors Draw.Color -> m a -> m a
withColor textColor act =
    do
        color <- Lens.view (has . Theme.textColors . Lens.cloneLens textColor)
        local (TextView.color .~ color) act

actionable :: _ => ElemId -> Lens.ALens' (t Text) Text -> E.Doc -> f ElemId -> m (M.TextWidget f)
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
        Lens.view (has . Lens.cloneLens txtLens)
            >>= Clickable.makeText myId (Clickable.Config action doc actionKeys)
            & local (TextView.color .~ color)
            & local (TextView.underline ?~ underline)
            <&> Align.tValue %~ Widget.weakerEvents eventMap

nameAtBinder :: _ => Name -> m a -> m a
nameAtBinder name act =
    do
        textEditStyle <- Lens.view (has . style)
        act & local (has .~ textEditStyle)
    where
        style =
            case name of
            Name.NameTag t
                | not (t ^. Name.tnIsAutoGen) ->
                    Style.nameAtBinder
            _ -> Style.autoNameOrigin

-- Sprite the size of unit (1 pixel)
unitSprite :: _ => AnItemLens t (GLDraw.Sprite, M.ElemId) -> m M.View
unitSprite lens =
    Lens.view id <&>
    \env ->
    let (s, elemId) = ((,) <$> env ^. has <*> elemIds) ^# lens
    in
    Draw.sprite s
    & void
    -- Y goes up by default in DrawingCombinators, switch that
    & (GLDraw.scaleV (M.Vector2 1 (-1)) %%)
    -- (-1..1) -> (0..2)
    & (GLDraw.translateV 1 %%)
    -- (0..2) -> (0..1)
    & (GLDraw.scaleV (M.Vector2 0.5 0.5) %%)
    & Anim.singletonFrame 1 (env ^. Element.elemIdPrefix <> elemId)
    & View.make 1

sprite :: _ => AnItemLens t (GLDraw.Sprite, M.ElemId) -> m M.View
sprite lens =
    do
        height <- Lens.view has <&> TextView.lineHeight
        unitSprite lens <&> Element.scale (realToFrac height)
