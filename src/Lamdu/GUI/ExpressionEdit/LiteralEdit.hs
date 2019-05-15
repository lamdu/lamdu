module Lamdu.GUI.ExpressionEdit.LiteralEdit
    ( make, makeLiteralEventMap
    ) where

import           Control.Applicative (liftA2)
import           Control.Lens (LensLike')
import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import qualified Data.Char as Char
import           Data.Functor.Identity (Identity(..))
import           Data.Property (Property)
import qualified Data.Property as Property
import qualified Data.Text as Text
import           GUI.Momentu.Align (TextWidget)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Direction as Dir
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
import qualified Lamdu.CharClassification as Chars
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.Formatting (Format(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds as HoleWidgetIds
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Payload as ExprGui
import           Lamdu.GUI.ExpressionGui.Wrap (stdWrap)
import           Lamdu.GUI.Styled (label)
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.Language as Language
import qualified Lamdu.I18N.Texts as Texts
import           Lamdu.Name (Name)
import           Lamdu.Style (Style, HasStyle)
import qualified Lamdu.Style as Style
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

mkEditEventMap ::
    (MonadReader env m, Monad o, Language.HasLanguage env) =>
    m (Text -> o Sugar.EntityId -> Gui EventMap o)
mkEditEventMap =
    Lens.view id
    <&> \env valText setToHole ->
    setToHole <&> HoleWidgetIds.make <&> HoleWidgetIds.hidOpen
    <&> SearchMenu.enterWithSearchTerm valText
    & E.keyPresses [ModKey mempty MetaKey.Key'Enter]
    (E.toDoc env [has . Texts.edit, has . Texts.value])

withStyle ::
    (MonadReader env m, HasStyle env) =>
    Lens.Getting TextEdit.Style Style TextEdit.Style -> m a -> m a
withStyle whichStyle =
    Reader.local (\x -> x & has .~ x ^. has . whichStyle)

genericEdit ::
    ( Monad o, Format a, MonadReader env f, HasStyle env, GuiState.HasCursor env
    , Language.HasLanguage env
    ) =>
    LensLike' (Lens.Const TextEdit.Style) Style TextEdit.Style ->
    Property o a ->
    Sugar.Payload name i o ExprGui.Payload -> f (Gui Responsive o)
genericEdit whichStyle prop pl =
    do
        editEventMap <-
            case pl ^. Sugar.plActions . Sugar.mSetToHole of
            Just action -> mkEditEventMap ?? valText ?? action
            _ -> error "Cannot set literal to hole?!"
        TextView.makeFocusable ?? valText ?? myId
            <&> Align.tValue %~ Widget.weakerEvents editEventMap
            <&> Responsive.fromWithTextPos
            & withStyle whichStyle
    where
        myId = WidgetIds.fromExprPayload pl
        valText = prop ^. Property.pVal & format

fdConfig ::
    ( MonadReader env m, Has Config env, Has Menu.Config env
    , Language.HasLanguage env
    ) =>
    m FocusDelegator.Config
fdConfig =
    Lens.view id
    <&> \env ->
    let litConf = env ^. has . Config.literal
        menuKeys = env ^. has . Menu.configKeys
    in
    FocusDelegator.Config
    { FocusDelegator.focusChildKeys = litConf ^. Config.literalStartEditingKeys
    , FocusDelegator.focusChildDoc =
        E.toDoc env
        [ has . Texts.edit
        , has . Texts.literal
        , has . Texts.startEditing
        ]
    , FocusDelegator.focusParentKeys =
        litConf ^. Config.literalStopEditingKeys
        -- The literal edit should behave like holes, in that the "pick option"
        -- key goes to the resulting expr.
        <> menuKeys ^. Menu.keysPickOption
        -- Only taken when the literal edit doesn't handle it by
        -- jumping to next entry:
        <> menuKeys ^. Menu.keysPickOptionAndGotoNext
    , FocusDelegator.focusParentDoc =
        E.toDoc env
        [ has . Texts.edit
        , has . Texts.literal
        , has . Texts.stopEditing
        ]
    }

withFd ::
    ( MonadReader env m, Has Config env, GuiState.HasCursor env
    , Has Menu.Config env, Language.HasLanguage env, Applicative f
    ) =>
    m (Widget.Id -> TextWidget f -> TextWidget f)
withFd =
    (FocusDelegator.make <*> fdConfig ?? FocusDelegator.FocusEntryParent)
    <&> Lens.mapped %~ (Align.tValue %~)

textEdit ::
    ( MonadReader env m, Has Config env, HasStyle env, Has Menu.Config env
    , Element.HasAnimIdPrefix env, GuiState.HasCursor env
    , Language.HasLanguage env, Monad o
    ) =>
    Property o Text ->
    Sugar.Payload name i o ExprGui.Payload ->
    m (TextWidget o)
textEdit prop pl =
    do
        text <- TextEdits.make ?? empty ?? prop ?? WidgetIds.literalEditOf myId
        (withFd ?? myId) <*>
            label (Texts.code . Texts.textOpener)
            /|/ pure text
            /|/ ((Align.tValue %~)
                    <$> (Element.padToSize ?? (text ^. Element.size & _1 .~ 0) ?? 1)
                    <*> label (Texts.code . Texts.textCloser)
                )
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
    ( MonadReader env m, Monad o
    , Has Config env, HasStyle env, Has Menu.Config env
    , GuiState.HasState env, Language.HasLanguage env
    ) =>
    Property o Double ->
    Sugar.Payload name i o ExprGui.Payload ->
    m (TextWidget o)
numEdit prop pl =
    (withFd ?? myId) <*>
    do
        text <-
            GuiState.readWidgetState myId
            <&> (^? Lens._Just . Lens.filtered ((== Just prevVal) . parseNum))
            <&> fromMaybe (format prevVal)
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
        toDoc <- Lens.view id <&> E.toDoc
        let negateEvent
                -- '-' at last position should apply operator rather than negate
                | pos /= Text.length text =
                    setPos (pos + Text.length negateText - Text.length text)
                    <> GuiState.updateWidgetState myId negateText
                    <$
                    (prop ^. Property.pSet) (negate curVal)
                    & const
                    & E.charGroup Nothing
                      (toDoc
                          [ has . Texts.edit
                          , has . Texts.literal
                          , has . Texts.negate
                          ]) "-"
                | otherwise = mempty
        strollEvent <-
            Lens.view (has . Menu.configKeys . Menu.keysPickOptionAndGotoNext)
            <&>
            \keys ->
            E.keysEventMap keys
            (toDoc
                [ has . Dir.navigation
                , has . Texts.nextEntry
                ])
            (pure ())
            <&> Lens.mapped . GuiState.uPreferStroll .~ (True ^. Lens._Unwrapped)
        let delEvent =
                case pl ^. Sugar.plActions . Sugar.mSetToHole of
                -- Allow to delete when text is empty
                Just action | Text.null text ->
                    E.keyPresses [ModKey mempty MetaKey.Key'Backspace]
                    (toDoc [has . Texts.edit, has . Texts.delete])
                    (action <&> WidgetIds.fromEntityId <&> GuiState.updateCursor)
                    <>
                    E.charEventMap "Letter"
                    (toDoc [has . Texts.edit, has . Texts.replace])
                    holeWithChar
                    where
                        holeWithChar c =
                            (action <&> HoleWidgetIds.make <&> HoleWidgetIds.hidOpen
                                <&> SearchMenu.enterWithSearchTerm (Text.singleton c))
                            <$ guard (Char.isAlpha c)
                _ -> mempty
        newLiteralEvent <-
            if Text.null text
            then makeLiteralEventMap ?? pl ^. Sugar.plActions . Sugar.setToLiteral
            else pure mempty
        TextEdit.make ?? empty ?? text ?? innerId
            <&> Align.tValue . Widget.eventMapMaker . Lens.mapped %~
                -- Avoid taking keys that don't belong to us,
                -- so weakerEvents with them will work.
                E.filter (Lens.has Lens._Just . parseNum . fst)
            <&> Align.tValue . Lens.mapped %~ event
            <&> Align.tValue %~ Widget.strongerEvents (negateEvent <> delEvent <> newLiteralEvent <> strollEvent)
            <&> Align.tValue %~ Widget.addPreEventWith (liftA2 mappend) preEvent
        & withStyle Style.num
    where
        prevVal = prop ^. Property.pVal
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

makeLiteralEventMap ::
    (MonadReader env m, Monad o, Language.HasLanguage env) =>
    m ((Sugar.Literal Identity -> o Sugar.EntityId) -> Gui EventMap o)
makeLiteralEventMap =
    Lens.view id <&> E.toDoc
    <&> \toDoc makeLiteral ->
    E.charGroup Nothing
    (toDoc [has . Texts.edit, has . Texts.literalText]) "'\""
    (const (makeLiteral (Sugar.LiteralText (Identity "")) <&> r))
    <>
    E.charGroup (Just "Digit")
    (toDoc [has . Texts.edit, has . Texts.literalNumber])
    Chars.digit
    (fmap r . makeLiteral . Sugar.LiteralNum . Identity . read . (: []))
    where
        r = GuiState.updateCursor . WidgetIds.literalEditOf . WidgetIds.fromEntityId
