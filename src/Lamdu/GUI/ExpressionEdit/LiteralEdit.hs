{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.LiteralEdit
    ( make
    ) where

import           Control.Lens (LensLike')
import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import           Data.UUID.Types (UUID)
import           GUI.Momentu.Align (WithTextPos)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/|/))
import           GUI.Momentu.MetaKey (MetaKey(..), noMods)
import qualified GUI.Momentu.MetaKey as MetaKey
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.FocusDelegator as FocusDelegator
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextEdit.Property as TextEdits
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Config as Config
import           Lamdu.Config (HasConfig)
import           Lamdu.Formatting (Format(..))
import           Lamdu.GUI.ExpressionEdit.HoleEdit.State (setHoleStateAndJump)
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Style (Style, HasStyle)
import qualified Lamdu.Style as Style
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction.Transaction

mkEditEventMap ::
    Monad m =>
    Text -> T m (UUID, Sugar.EntityId) ->
    Widget.EventMap (T m GuiState.Update)
mkEditEventMap valText setToHole =
    Widget.keysEventMapMovesCursor [MetaKey noMods MetaKey.Key'Enter]
    (E.Doc ["Edit", "Value"]) $
    do
        (uuid, entityId) <- setToHole
        setHoleStateAndJump uuid valText entityId

genericEdit ::
    ( Monad m, Format a, MonadReader env f, HasStyle env, Widget.HasCursor env
    ) =>
    LensLike' (Lens.Const TextEdit.Style) Style TextEdit.Style ->
    Transaction.Property m a ->
    Sugar.Payload (T m) ExprGuiT.Payload -> f (ExpressionGui m)
genericEdit whichStyle prop pl =
    do
        style <- Lens.view Style.style <&> (^. whichStyle)
        TextView.makeFocusable ?? valText ?? myId
            & Reader.local (TextEdit.style .~ style)
            <&> Align.tValue %~ E.weakerEvents editEventMap
            <&> Responsive.fromWithTextPos
    where
        myId = WidgetIds.fromExprPayload pl
        editEventMap =
            case pl ^. Sugar.plActions . Sugar.delete of
            Sugar.SetToHole action -> mkEditEventMap valText action
            _ -> error "Cannot set literal to hole?!"
        valText = prop ^. Property.pVal & format

fdConfig :: Config.LiteralText -> FocusDelegator.Config
fdConfig conf = FocusDelegator.Config
    { FocusDelegator.focusChildKeys = Config.literalTextStartEditingKeys conf
    , FocusDelegator.focusChildDoc = E.Doc ["Edit", "Literal Text", "Start editing"]
    , FocusDelegator.focusParentKeys = Config.literalTextStopEditingKeys conf
    , FocusDelegator.focusParentDoc = E.Doc ["Edit", "Literal Text", "Stop editing"]
    }

textEdit ::
    ( Monad m, MonadReader env f, HasConfig env, HasStyle env
    , Element.HasAnimIdPrefix env, Widget.HasCursor env
    ) =>
    Transaction.Property m Text ->
    Sugar.Payload (T m) ExprGuiT.Payload ->
    f (WithTextPos (Widget (T m GuiState.Update)))
textEdit prop pl =
    do
        config <- Lens.view Config.config <&> Config.literalText
        style <- Lens.view (Style.style . Style.styleText)
        do
            left <- TextView.makeLabel "“"
            text <- TextEdits.make ?? empty ?? prop ?? innerId
            right <-
                TextView.makeLabel "„"
                <&> Element.padToSizeAlign (text ^. Element.size & _1 .~ 0) 1
            left /|/ text /|/ right & return
            & Reader.local (TextEdit.style .~ style)
            >>= Align.tValue %%~
                (FocusDelegator.make ?? fdConfig config
                ?? FocusDelegator.FocusEntryParent
                ?? myId ??)
    where
        empty = TextEdit.EmptyStrings "" ""
        innerId = WidgetIds.literalTextEditOf myId
        myId = WidgetIds.fromExprPayload pl

make ::
    Monad m =>
    Sugar.Literal (Transaction.Property m) -> Sugar.Payload (T m) ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
make lit pl =
    case lit of
    Sugar.LiteralNum x -> genericEdit Style.styleNum x pl
    Sugar.LiteralBytes x -> genericEdit Style.styleBytes x pl
    Sugar.LiteralText x -> textEdit x pl <&> Responsive.fromWithTextPos
    & ExpressionGui.stdWrap pl
