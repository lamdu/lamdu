{-# LANGUAGE RecordWildCards, NoImplicitPrelude, OverloadedStrings #-}
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
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.FocusDelegator as FocusDelegator
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextEdit.Property as TextEdits
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Config as Config
import           Lamdu.Formatting (Format(..))
import           Lamdu.GUI.ExpressionEdit.HoleEdit.State (HoleState(..), setHoleStateAndJump)
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Style (Style)
import qualified Lamdu.Style as Style
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction.Transaction

mkEditEventMap ::
    Monad m =>
    Text -> T m (UUID, Sugar.EntityId) ->
    Widget.EventMap (T m Widget.EventResult)
mkEditEventMap valText setToHole =
    Widget.keysEventMapMovesCursor [MetaKey noMods MetaKey.Key'Enter]
    (E.Doc ["Edit", "Value"]) $
    do
        (uuid, entityId) <- setToHole
        setHoleStateAndJump uuid (HoleState valText) entityId

genericEdit ::
    (Monad m, Format a) =>
    LensLike' (Lens.Const TextEdit.Style) Style TextEdit.Style ->
    Transaction.Property m a ->
    Sugar.Payload m ExprGuiT.Payload -> ExprGuiM m (ExpressionGui m)
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
            case pl ^. Sugar.plActions . Sugar.setToHole of
            Sugar.SetToHole action -> mkEditEventMap valText action
            Sugar.SetWrapperToHole action -> mkEditEventMap valText action
            Sugar.AlreadyAHole -> error "Literal val is a hole?!"
        valText = prop ^. Property.pVal & format

fdConfig :: Config.LiteralText -> FocusDelegator.Config
fdConfig Config.LiteralText{..} = FocusDelegator.Config
    { FocusDelegator.focusChildKeys = literalTextStartEditingKeys
    , FocusDelegator.focusChildDoc = E.Doc ["Edit", "Literal Text", "Start editing"]
    , FocusDelegator.focusParentKeys = literalTextStopEditingKeys
    , FocusDelegator.focusParentDoc = E.Doc ["Edit", "Literal Text", "Stop editing"]
    }

textEdit ::
    Monad m => Transaction.Property m Text ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (WithTextPos (Widget (T m Widget.EventResult)))
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
                ?? WidgetIds.notDelegatingId myId ??)
    & Widget.assignCursor myId (WidgetIds.notDelegatingId myId)
    where
        empty = TextEdit.EmptyStrings "" ""
        innerId = WidgetIds.delegatingId myId
        myId = WidgetIds.fromExprPayload pl

make ::
    Monad m =>
    Sugar.Literal (Transaction.Property m) -> Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
make lit pl =
    case lit of
    Sugar.LiteralNum x -> genericEdit Style.styleNum x pl
    Sugar.LiteralBytes x -> genericEdit Style.styleBytes x pl
    Sugar.LiteralText x -> textEdit x pl <&> Responsive.fromWithTextPos
    & ExpressionGui.stdWrap pl
