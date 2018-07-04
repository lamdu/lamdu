module Lamdu.GUI.ExpressionEdit.LiteralEdit
    ( make
    ) where

import           Control.Applicative (liftA2)
import           Control.Lens (LensLike')
import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import qualified Data.Char as Char
import           Data.Property (Property)
import qualified Data.Property as Property
import qualified Data.Text as Text
import           GUI.Momentu.Align (TextWidget)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/|/))
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.ModKey (ModKey(..))
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import           GUI.Momentu.State (Gui)
import qualified GUI.Momentu.State as GuiState
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
import qualified Lamdu.GUI.ExpressionGui.Payload as ExprGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import           Lamdu.GUI.ExpressionGui.Wrap (stdWrap)
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name)
import           Lamdu.Style (Style, HasStyle)
import qualified Lamdu.Style as Style
import qualified Lamdu.Sugar.NearestHoles as NearestHoles
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

mkEditEventMap ::
    Monad o =>
    Text -> o Sugar.EntityId ->
    Gui EventMap o
mkEditEventMap valText setToHole =
    setToHole <&> HoleWidgetIds.make <&> HoleWidgetIds.hidOpen
    <&> SearchMenu.enterWithSearchTerm valText
    & E.keyPresses [ModKey mempty MetaKey.Key'Enter] (E.Doc ["Edit", "Value"])

withStyle ::
    (MonadReader env m, HasStyle env) =>
    Lens.Getting TextEdit.Style Style TextEdit.Style -> m a -> m a
withStyle whichStyle =
    Reader.local (\x -> x & TextEdit.style .~ x ^. Style.style . whichStyle)

genericEdit ::
    ( Monad o, Format a, MonadReader env f, HasStyle env, GuiState.HasCursor env
    ) =>
    LensLike' (Lens.Const TextEdit.Style) Style TextEdit.Style ->
    Property o a ->
    Sugar.Payload name i o ExprGui.Payload -> f (Gui Responsive o)
genericEdit whichStyle prop pl =
    TextView.makeFocusable ?? valText ?? myId
    <&> Align.tValue %~ Widget.weakerEvents editEventMap
    <&> Responsive.fromWithTextPos
    & withStyle whichStyle
    where
        myId = WidgetIds.fromExprPayload pl
        editEventMap =
            case pl ^. Sugar.plActions . Sugar.mSetToHole of
            Just action -> mkEditEventMap valText action
            _ -> error "Cannot set literal to hole?!"
        valText = prop ^. Property.pVal & format

fdConfig :: (MonadReader env m, HasConfig env, Menu.HasConfig env) => m FocusDelegator.Config
fdConfig =
    (,)
    <$> Lens.view (Config.config . Config.literal)
    <*> Lens.view (Menu.config . Menu.configKeys)
    <&>
    \(litConf, menuKeys) ->
    FocusDelegator.Config
    { FocusDelegator.focusChildKeys = litConf ^. Config.literalStartEditingKeys
    , FocusDelegator.focusChildDoc = E.Doc ["Edit", "Literal", "Start editing"]
    , FocusDelegator.focusParentKeys =
        litConf ^. Config.literalStopEditingKeys
        -- The literal edit should behave like holes, in that the "pick option"
        -- key goes to the resulting expr.
        <> menuKeys ^. Menu.keysPickOption
        -- Only taken when the literal edit doesn't handle it by
        -- jumping to next entry:
        <> menuKeys ^. Menu.keysPickOptionAndGotoNext
    , FocusDelegator.focusParentDoc = E.Doc ["Edit", "Literal", "Stop editing"]
    }

withFd ::
    ( MonadReader env m, HasConfig env, GuiState.HasCursor env, Menu.HasConfig env
    , Applicative f
    ) =>
    m (Widget.Id -> TextWidget f -> TextWidget f)
withFd =
    (FocusDelegator.make <*> fdConfig ?? FocusDelegator.FocusEntryParent)
    <&> Lens.mapped %~ (Align.tValue %~)

textEdit ::
    ( MonadReader env m, HasConfig env, HasStyle env, Menu.HasConfig env
    , Element.HasAnimIdPrefix env, GuiState.HasCursor env, Monad o
    ) =>
    Property o Text ->
    Sugar.Payload name i o ExprGui.Payload ->
    m (TextWidget o)
textEdit prop pl =
    do
        left <- TextView.makeLabel "“"
        text <- TextEdits.make ?? empty ?? prop ?? WidgetIds.literalEditOf myId
        right <-
            TextView.makeLabel "„"
            <&> Element.padToSizeAlign (text ^. Element.size & _1 .~ 0) 1
        withFd ?? myId ?? left /|/ text /|/ right
    & withStyle Style.text
    where
        empty = TextEdit.Modes "" ""
        myId = WidgetIds.fromExprPayload pl

parseNum :: Text -> Maybe Double
parseNum newText
    | newText /= Text.strip newText = Nothing
    | newText `elem` ["", "-", ".", "-."] = Just 0
    | otherwise = tryParse newText

numEdit ::
    ( MonadReader env m, HasConfig env, HasStyle env, Menu.HasConfig env
    , GuiState.HasState env, Monad o
    ) =>
    Property o Double ->
    Sugar.Payload name i o ExprGui.Payload ->
    m (TextWidget o)
numEdit prop pl =
    (withFd ?? myId) <*>
    do
        text <- GuiState.readWidgetState myId <&> fromMaybe (format (prop ^. Property.pVal))
        let preEvent =
                Widget.PreEvent
                { Widget._pDesc = ""
                , Widget._pAction = pure mempty
                , Widget._pTextRemainder = if "." `Text.isSuffixOf` text then "." else ""
                }
        pos <- TextEdit.getCursor ?? text ?? innerId <&> fromMaybe (Text.length text)
        let negateText
                | "-" `Text.isPrefixOf` text = Text.tail text
                | otherwise = "-" <> text
        let negateEvent
                -- '-' at last position should apply operator rather than negate
                | pos /= Text.length text =
                    setPos (pos + Text.length negateText - Text.length text)
                    <> GuiState.updateWidgetState myId negateText
                    <$
                    (prop ^. Property.pSet) (negate curVal)
                    & const
                    & E.charGroup Nothing (E.Doc ["Edit", "Literal", "Negate"]) "-"
                | otherwise = mempty
        nextEntryEvent <-
            case pl ^. Sugar.plData . ExprGui.plNearestHoles . NearestHoles.next of
            Nothing -> pure mempty
            Just nextEntry ->
                Lens.view (Menu.config . Menu.configKeys . Menu.keysPickOptionAndGotoNext)
                <&>
                \keys ->
                WidgetIds.fromEntityId nextEntry & pure
                & E.keysEventMapMovesCursor keys (E.Doc ["Navigation", "Next entry"])
        let delEvent =
                case pl ^. Sugar.plActions . Sugar.mSetToHole of
                -- Allow to delete when text is empty
                Just action | Text.null text ->
                    E.keyPresses [ModKey mempty MetaKey.Key'Backspace] (E.Doc ["Edit", "Delete"])
                    (action <&> WidgetIds.fromEntityId <&> GuiState.updateCursor)
                    <>
                    E.charEventMap "Character" (E.Doc ["Edit", "Replace"]) holeWithChar
                    where
                        holeWithChar c =
                            (action <&> HoleWidgetIds.make <&> HoleWidgetIds.hidOpen
                                <&> SearchMenu.enterWithSearchTerm (Text.singleton c))
                            <$ guard (Char.isAlpha c)
                _ -> mempty
        TextEdit.make ?? empty ?? text ?? innerId
            <&> Align.tValue . Widget.eventMapMaker . Lens.mapped %~
                -- Avoid taking keys that don't belong to us,
                -- so weakerEvents with them will work.
                E.filter (Lens.has Lens._Just . parseNum . fst)
            <&> Align.tValue . Lens.mapped %~ event
            <&> Align.tValue %~ Widget.strongerEvents (negateEvent <> delEvent <> nextEntryEvent)
            <&> Align.tValue %~ Widget.addPreEventWith (liftA2 mappend) preEvent
    & withStyle Style.num
    where
        setPos newPos = TextEdit.encodeCursor innerId newPos & GuiState.updateCursor
        innerId = WidgetIds.literalEditOf myId
        curVal = prop ^. Property.pVal
        event (newText, update) =
            GuiState.updateWidgetState myId newText <> update <$
            maybe (pure ()) (prop ^. Property.pSet) (parseNum newText)
        empty =
            TextEdit.Modes
            { TextEdit._unfocused = "0"
            , TextEdit._focused = ""
            }
        myId = WidgetIds.fromExprPayload pl

make ::
    (Monad i, Monad o) =>
    Sugar.Literal (Property o) ->
    Sugar.Payload (Name o) i o ExprGui.Payload ->
    ExprGuiM i o (Gui Responsive o)
make lit pl =
    stdWrap pl
    <*>
    case lit of
    Sugar.LiteralNum x -> numEdit x pl <&> Responsive.fromWithTextPos
    Sugar.LiteralBytes x -> genericEdit Style.bytes x pl
    Sugar.LiteralText x -> textEdit x pl <&> Responsive.fromWithTextPos
