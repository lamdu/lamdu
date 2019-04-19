-- | Styled widgets
-- Apply the Lamdu theme to various widgets and guis
module Lamdu.GUI.Styled
    ( grammar, info
    , text, label
    , addValBG, addBgColor
    , addValPadding, addValFrame
    , deletedDef, deletedUse
    , actionable
    , withColor
    , nameAtBinder
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           GUI.Momentu.Align (WithTextPos(..), TextWidget)
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.Animation (AnimId)
import qualified GUI.Momentu.Animation as Anim
import qualified GUI.Momentu.Draw as Draw
import           GUI.Momentu.Element (Element)
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Font as Font
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Label as Label
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (Theme, HasTheme(..))
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Config.Theme.TextColors (TextColors)
import qualified Lamdu.Config.Theme.TextColors as TextColors
import           Lamdu.I18N.Languages (texts)
import           Lamdu.I18N.Texts (Texts)
import           Lamdu.Name (Name(..))
import qualified Lamdu.Name as Name
import qualified Lamdu.Style as Style

import           Lamdu.Prelude

info :: (MonadReader env m, HasTheme env, TextView.HasStyle env) => m a -> m a
info = withColor TextColors.infoTextColor

grammar :: (MonadReader env m, HasTheme env, TextView.HasStyle env) => m a -> m a
grammar = withColor TextColors.grammarColor

rawText ::
    (MonadReader env f, TextView.HasStyle env, Element.HasAnimIdPrefix env) =>
    AnimId -> Text -> f (WithTextPos View)
rawText animIdSuffix txt =
    (TextView.make ?? txt) <*> Element.subAnimId animIdSuffix

text ::
    (MonadReader env f, TextView.HasStyle env, Element.HasAnimIdPrefix env) =>
    (f (WithTextPos View) -> view) -> AnimId -> Lens.ALens' Texts Text -> view
text style animIdSuffix txtLens = rawText animIdSuffix (texts ^# txtLens) & style

label ::
    (MonadReader env m, TextView.HasStyle env, Element.HasAnimIdPrefix env) =>
    Lens.ALens' Texts Text -> m (WithTextPos View)
label = Label.make . (texts ^#)

addValBG ::
    ( MonadReader env m, Element a
    , Element.HasAnimIdPrefix env, HasTheme env
    ) => m (a -> a)
addValBG = addBgColor Theme.valFrameBGColor

addBgColor ::
    ( MonadReader env m, Element a
    , Element.HasAnimIdPrefix env, HasTheme env
    ) => Lens.ALens' Theme Draw.Color -> m (a -> a)
addBgColor getColor =
    Draw.backgroundColor <*> Lens.view (Theme.theme . Lens.cloneLens getColor)

addValPadding :: (MonadReader env m, Element a, HasTheme env) => m (a -> a)
addValPadding =
    Lens.view (Theme.theme . Theme.valFramePadding) <&> Element.padAround

addValFrame ::
    ( MonadReader env m, Element a, Element.HasAnimIdPrefix env, HasTheme env
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
    Element.subAnimId ["diagonal"] <&>
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
    ( MonadReader env m, HasTheme env, Element a, Element.HasAnimIdPrefix env
    ) =>
    Lens.Getting Widget.R Theme.Deleted Widget.R -> m (a -> a)
deletedDiagonal widthLens =
    do
        width <- Lens.view (Theme.theme . Theme.deleted . widthLens)
        addDiagonal <*> Lens.view (Theme.theme . Theme.errorColor) ?? width

deletedUse ::
    (MonadReader env m, Element a, Element.HasAnimIdPrefix env, HasTheme env) =>
    m (a -> a)
deletedUse = deletedDiagonal Theme.deletedUseDiagonalWidth

deletedDef ::
    (MonadReader env m, Element a, Element.HasAnimIdPrefix env, HasTheme env) =>
    m (a -> a)
deletedDef =
    (.)
    <$> deletedDiagonal Theme.deletedDefDiagonalWidth
    <*> (Lens.view (Theme.theme . Theme.deleted . Theme.deletedDefTint) <&> Element.tint)

withColor ::
    (MonadReader env m, HasTheme env, TextView.HasStyle env) =>
    Lens.ALens' TextColors Draw.Color -> m a -> m a
withColor textColor act =
    do
        color <- Lens.view (Theme.theme . Theme.textColors . Lens.cloneLens textColor)
        Reader.local (TextView.color .~ color) act

actionable ::
    ( Element.HasAnimIdPrefix env, TextView.HasStyle env
    , GuiState.HasCursor env, Config.HasConfig env, HasTheme env, Applicative f
    , MonadReader env m
    ) =>
    Widget.Id -> Lens.ALens' Texts Text -> E.Doc -> f Widget.Id ->
    m (TextWidget f)
actionable myId txtLens doc action =
    do
        color <- Lens.view (Theme.theme . Theme.textColors . TextColors.actionTextColor)
        underlineWidth <- Lens.view (Theme.theme . Theme.narrowUnderlineWidth)
        let underline =
                Font.Underline
                { Font._underlineColor = color
                , Font._underlineWidth = underlineWidth
                }
        actionKeys <- Lens.view (Config.config . Config.actionKeys)
        let eventMap = E.keysEventMapMovesCursor actionKeys doc action
        (Widget.makeFocusableView ?? myId <&> (Align.tValue %~))
            <*> Label.make (texts ^# txtLens)
            & Reader.local (TextView.color .~ color)
            & Reader.local (TextView.underline ?~ underline)
            <&> Align.tValue %~ Widget.weakerEvents eventMap

nameAtBinder ::
    (MonadReader env m, Style.HasStyle env) => Name n -> m b -> m b
nameAtBinder name act =
    do
        style <- Lens.view Style.style
        let textEditStyle =
                style
                ^. case name of
                    Name.AutoGenerated {} -> Style.autoNameOrigin
                    Name.Unnamed {}       -> Style.autoNameOrigin
                    Name.Stored {}        -> Style.nameAtBinder
        act & Reader.local (TextEdit.style .~ textEditStyle)
