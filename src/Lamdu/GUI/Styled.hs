-- | Styled widgets
-- Apply the Lamdu theme to various widgets and guis
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.Styled
    ( grammarLabel, grammarText
    , addValBG, addBgColor
    , addValPadding, addValFrame
    , addDeletionDiagonal
    , actionable
    , withColor
    , nameAtBinder
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           GUI.Momentu.Align (WithTextPos(..))
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
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (Theme, HasTheme(..))
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Name (Name(..))
import qualified Lamdu.Name as Name
import qualified Lamdu.Style as Style

import           Lamdu.Prelude

grammarLabel ::
    ( MonadReader env m
    , HasTheme env
    , TextView.HasStyle env
    , Element.HasAnimIdPrefix env
    ) => Text -> m (WithTextPos View)
grammarLabel text =
    do
        th <- Lens.view theme <&> Theme.textColors
        TextView.makeLabel text
            & Reader.local (TextView.color .~ Theme.grammarColor th)

grammarText ::
    ( MonadReader env m
    , HasTheme env
    , TextView.HasStyle env
    ) => m (Text -> AnimId -> WithTextPos View)
grammarText =
    do
        th <- Lens.view theme <&> Theme.textColors
        TextView.make
            & Reader.local (TextView.color .~ Theme.grammarColor th)

addValBG ::
    ( MonadReader env m, Element a
    , Element.HasAnimIdPrefix env, HasTheme env
    ) => m (a -> a)
addValBG = addBgColor Theme.valFrameBGColor

addBgColor ::
    ( MonadReader env m, Element a
    , Element.HasAnimIdPrefix env, HasTheme env
    ) => (Theme -> Draw.Color) -> m (a -> a)
addBgColor getColor =
    Draw.backgroundColor <*> (Lens.view Theme.theme <&> getColor)

addValPadding :: (MonadReader env m, Element a, HasTheme env) => m (a -> a)
addValPadding = Lens.view Theme.theme <&> Theme.valFramePadding <&> Element.pad

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

withColor ::
    (MonadReader env m, HasTheme env, TextView.HasStyle env) =>
    (Theme -> Draw.Color) -> m a -> m a
withColor nameColor act =
    do
        color <- Lens.view Theme.theme <&> nameColor
        Reader.local (TextView.color .~ color) act

actionable ::
    ( Element.HasAnimIdPrefix env, TextView.HasStyle env
    , GuiState.HasCursor env, Config.HasConfig env, HasTheme env, Applicative f
    , MonadReader env m
    ) =>
    Widget.Id -> Text -> E.Doc -> f Widget.Id ->
    m (WithTextPos (Widget (f GuiState.Update)))
actionable myId text doc action =
    do
        color <- Lens.view Theme.theme <&> Theme.actionTextColor
        underlineWidth <- Lens.view Theme.theme <&> Theme.narrowUnderlineWidth
        let underline =
                Font.Underline
                { Font._underlineColor = color
                , Font._underlineWidth = underlineWidth
                }
        actionKeys <- Lens.view Config.config <&> Config.actionKeys
        let eventMap = E.keysEventMapMovesCursor actionKeys doc action
        (Widget.makeFocusableView ?? myId <&> (Align.tValue %~))
            <*> TextView.makeLabel text
            & Reader.local (TextView.color .~ color)
            & Reader.local (TextView.underline ?~ underline)
            <&> Align.tValue %~ Widget.weakerEvents eventMap

nameAtBinder ::
    (MonadReader env m, HasTheme env, Style.HasStyle env) =>
    (Theme.Name -> Draw.Color) -> Name n -> m b -> m b
nameAtBinder nameColor name act =
    do
        color <- Lens.view theme <&> Theme.name <&> nameColor
        style <- Lens.view Style.style
        let textEditStyle =
                style
                ^. case name of
                    Name.AutoGenerated {} -> Style.styleAutoNameOrigin
                    Name.Unnamed {}       -> Style.styleAutoNameOrigin
                    Name.Stored {}        -> Style.styleNameAtBinder
                & TextView.color .~ color
        act & Reader.local (TextEdit.style .~ textEditStyle)
