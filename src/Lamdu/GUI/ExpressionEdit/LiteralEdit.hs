{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.LiteralEdit
    ( make
    ) where

import           Control.Applicative (liftA2)
import           Control.Lens (LensLike')
import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Data.Text as Text
import           GUI.Momentu.Align (WithTextPos)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/|/))
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.ModKey (ModKey(..))
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.FocusDelegator as FocusDelegator
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextEdit.Property as TextEdits
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Lamdu.Config (HasConfig)
import qualified Lamdu.Config as Config
import           Lamdu.Formatting (Format(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds as HoleWidgetIds
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExprGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import           Lamdu.GUI.ExpressionGui.Wrap (stdWrap)
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Style (Style, HasStyle)
import qualified Lamdu.Style as Style
import qualified Lamdu.Sugar.NearestHoles as NearestHoles
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction.Transaction

mkEditEventMap ::
    Monad m =>
    Text -> T m Sugar.EntityId ->
    EventMap (T m GuiState.Update)
mkEditEventMap valText setToHole =
    setToHole <&> HoleWidgetIds.make <&> HoleWidgetIds.hidOpen
    <&> SearchMenu.enterWithSearchTerm valText
    & E.keyPresses [ModKey mempty MetaKey.Key'Enter] (E.Doc ["Edit", "Value"])

withStyle ::
    (MonadReader env m, HasStyle env) =>
    Lens.Getting TextEdit.Style Style TextEdit.Style -> m a -> m a
withStyle whichStyle act =
    do
        style <- Lens.view Style.style <&> (^. whichStyle)
        Reader.local (TextEdit.style .~ style) act

genericEdit ::
    ( Monad m, Format a, MonadReader env f, HasStyle env, GuiState.HasCursor env
    ) =>
    LensLike' (Lens.Const TextEdit.Style) Style TextEdit.Style ->
    Transaction.Property m a ->
    Sugar.Payload (T m) ExprGui.Payload -> f (ExpressionGui m)
genericEdit whichStyle prop pl =
    TextView.makeFocusable ?? valText ?? myId
    <&> Align.tValue %~ Widget.weakerEvents editEventMap
    <&> Responsive.fromWithTextPos
    & withStyle whichStyle
    where
        myId = WidgetIds.fromExprPayload pl
        editEventMap =
            case pl ^. Sugar.plActions . Sugar.delete of
            Sugar.SetToHole action -> mkEditEventMap valText action
            _ -> error "Cannot set literal to hole?!"
        valText = prop ^. Property.pVal & format

fdConfig :: (MonadReader env m, HasConfig env, Menu.HasConfig env) => m FocusDelegator.Config
fdConfig =
    (,)
    <$> (Lens.view Config.config <&> Config.literal)
    <*> (Lens.view Menu.config <&> Menu.configKeys)
    <&>
    \(litConf, menuKeys) ->
    FocusDelegator.Config
    { FocusDelegator.focusChildKeys = Config.literalStartEditingKeys litConf
    , FocusDelegator.focusChildDoc = E.Doc ["Edit", "Literal", "Start editing"]
    , FocusDelegator.focusParentKeys =
        Config.literalStopEditingKeys litConf
        -- The literal edit should behave like holes, in that the "pick option"
        -- key goes to the resulting expr.
        <> Menu.keysPickOption menuKeys
    , FocusDelegator.focusParentDoc = E.Doc ["Edit", "Literal", "Stop editing"]
    }

withFd ::
    ( MonadReader env m, HasConfig env, GuiState.HasCursor env, Menu.HasConfig env
    , Applicative f
    ) =>
    m (Widget.Id -> WithTextPos (Widget (f GuiState.Update)) -> WithTextPos (Widget (f GuiState.Update)))
withFd =
    (FocusDelegator.make <*> fdConfig ?? FocusDelegator.FocusEntryParent)
    <&> Lens.mapped %~ (Align.tValue %~)

textEdit ::
    ( MonadReader env m, HasConfig env, HasStyle env, Menu.HasConfig env
    , Element.HasAnimIdPrefix env, GuiState.HasCursor env, Monad f
    ) =>
    Transaction.Property f Text ->
    Sugar.Payload (T f) ExprGui.Payload ->
    m (WithTextPos (Widget (T f GuiState.Update)))
textEdit prop pl =
    do
        left <- TextView.makeLabel "“"
        text <- TextEdits.make ?? empty ?? prop ?? WidgetIds.literalEditOf myId
        right <-
            TextView.makeLabel "„"
            <&> Element.padToSizeAlign (text ^. Element.size & _1 .~ 0) 1
        withFd ?? myId ?? left /|/ text /|/ right
    & withStyle Style.styleText
    where
        empty = TextEdit.EmptyStrings "" ""
        myId = WidgetIds.fromExprPayload pl

parseNum :: Text -> Maybe Double
parseNum newText
    | Text.null newText = Just 0
    | otherwise = tryParse newText

expandedNumText :: Double -> Text
expandedNumText val
    | Text.any (== '.') baseText = baseText
    | otherwise = baseText <> "."
    where
        baseText = format val

numEdit ::
    ( MonadReader env m, HasConfig env, HasStyle env, Menu.HasConfig env
    , Element.HasAnimIdPrefix env, GuiState.HasCursor env, Monad f
    ) =>
    Transaction.Property f Double ->
    Sugar.Payload (T f) ExprGui.Payload ->
    m (WithTextPos (Widget (T f GuiState.Update)))
numEdit prop pl =
    (withFd ?? myId) <*>
    do
        (pos, text, remainderText) <-
            do
                cursor <- Lens.view GuiState.cursor
                if cursor == innerId
                    then
                        let r = format curVal
                        in pure (Text.length r, r, "")
                    else
                        TextEdit.getCursor ?? expandedText ?? innerId
                        <&> fromMaybe 0
                        <&> numState
        let preEvent =
                Widget.PreEvent
                { Widget._pDesc = ""
                , Widget._pAction = pure mempty
                , Widget._pTextRemainder = remainderText
                }
        let negateEvent
                -- '-' at last position should apply operator rather than negate
                | pos /= Text.length text =
                    GuiState.updateCursor (TextEdit.encodeCursor innerId (pos + round (signum curVal))) <$
                    (prop ^. Property.pSet) (negate curVal)
                    & const
                    & E.charGroup Nothing (E.Doc ["Edit", "Literal", "Negate"]) "-"
                | otherwise = mempty
        nextEntryEvent <-
            case pl ^. Sugar.plData . ExprGui.plNearestHoles . NearestHoles.next of
            Nothing -> pure mempty
            Just nextEntry ->
                Lens.view Menu.config <&> Menu.configKeys <&> Menu.keysPickOptionAndGotoNext
                <&>
                \keys ->
                WidgetIds.fromEntityId nextEntry & pure
                & E.keysEventMapMovesCursor keys (E.Doc ["Navigation", "Next entry"])
        ( (TextEdit.make ?? empty ?? text ?? innerId)
                <&> Align.tValue . Widget.eventMapMaker . Lens.mapped %~
                    -- Avoid taking keys that don't belong to us,
                    -- so weakerEvents with them will work.
                    E.filter (Lens.has Lens._Just . parseNum . fst)
                <&> Align.tValue . Lens.mapped %~ event
                <&> Align.tValue %~ Widget.strongerEvents (negateEvent <> nextEntryEvent)
                <&> Align.tValue %~ Widget.addPreEventWith (liftA2 mappend) preEvent
            )
    & withStyle Style.styleNum
    where
        expandedText = expandedNumText curVal
        innerId = WidgetIds.literalEditOf myId
        curVal = prop ^. Property.pVal
        event (newText, update)
            | Text.null newText =
                pl ^? Sugar.plActions . Sugar.delete . Lens.failing Sugar._SetToHole Sugar._Delete
                <&> fmap WidgetIds.fromEntityId <&> fmap GuiState.updateCursor
                & fromMaybe mempty
            | otherwise =
                case parseNum newText of
                Nothing -> pure mempty
                Just val -> update <$ (prop ^. Property.pSet) val
        empty = TextEdit.EmptyStrings "0" "0"
        myId = WidgetIds.fromExprPayload pl
        numState pos
            | "." `Text.isSuffixOf` expandedText =
                if pos == Text.length expandedText
                then (pos, expandedText, ".")
                else (pos, Text.init expandedText, "")
            | otherwise =
                (pos, expandedText, "")

make ::
    Monad m =>
    Sugar.Literal (Transaction.Property m) -> Sugar.Payload (T m) ExprGui.Payload ->
    ExprGuiM m (ExpressionGui m)
make lit pl =
    stdWrap pl
    <*>
    case lit of
    Sugar.LiteralNum x -> numEdit x pl <&> Responsive.fromWithTextPos
    Sugar.LiteralBytes x -> genericEdit Style.styleBytes x pl
    Sugar.LiteralText x -> textEdit x pl <&> Responsive.fromWithTextPos
