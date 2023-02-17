module Lamdu.GUI.Expr.LiteralEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified Data.ByteString.Base16 as Hex
import           Data.ByteString.Lens (chars)
import qualified Data.Char as Char
import           Data.Property (Property(..))
import qualified Data.Property as Property
import qualified Data.Text as Text
import           Data.Text.Encoding (encodeUtf8)
import           GUI.Momentu (Responsive, noMods)
import qualified GUI.Momentu as M
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.I18N as MomentuTexts
import qualified GUI.Momentu.ModKey as ModKey
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.FocusDelegator as FocusDelegator
import qualified GUI.Momentu.Widgets.Label as Label
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextEdit.Property as TextEdits
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Config as Config
import           Lamdu.Formatting (Format(..))
import qualified Lamdu.GUI.Expr.EventMap as ExprEventMap
import           Lamdu.GUI.Monad (GuiM)
import           Lamdu.GUI.Styled (label)
import qualified Lamdu.GUI.Types as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.GUI.Wrap (stdWrap)
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.Navigation as Texts
import           Lamdu.Style (Style)
import qualified Lamdu.Style as Style
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

withStyle :: _ => Lens.Getting TextEdit.Style Style TextEdit.Style -> m a -> m a
withStyle whichStyle =
    local (\x -> x & has .~ x ^. has . whichStyle)

groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf c l =
    x : groupsOf c xs
    where
        (x, xs) = splitAt c l

bytesEdit ::
    _ =>
    Property o ByteString ->
    Sugar.Payload v o -> m (M.TextWidget o)
bytesEdit prop pl =
    Label.make "#" M./|/
    ( TextEdits.makeLineEdit ?? emptyTexts ??
        Property.pureCompose enc dec prop ??
        WidgetIds.literalEditOf (WidgetIds.fromExprPayload pl)
    )
    where
        enc s = Hex.encode s ^.. chars & groupsOf 2 & unwords & Text.pack
        dec s =
            Hex.decode (encodeUtf8 (mconcat
                (Text.splitOn " " s <&> Text.take 2 . (<> "00") . Text.filter Char.isHexDigit)))
            ^?! Lens._Right

charEdit :: _ => Property o Char -> Sugar.Payload v o -> m (M.TextWidget o)
charEdit (Property char setChar) pl =
    do
        env <- Lens.view id
        let doc =
                E.toDoc env
                [ has . MomentuTexts.edit
                , has . Texts.literal
                , has . Texts.setLiteralChar
                ]
        let setCharEventMap = E.allChars (env ^. has . TextEdit.textCharacter) doc ((mempty <$) . setChar)
        edit <-
            TextView.makeFocusable ?? Text.singleton char ?? innerId
            <&> M.tValue %~ Widget.weakerEvents setCharEventMap
        let quote elemId = (TextView.make ?? "'") <*> (Element.subElemId ?? elemId)
        quote "opener"
            M./|/ pure edit
            M./|/ quote "closer"
        & withStyle Style.char
    where
        myId = WidgetIds.fromExprPayload pl
        innerId = WidgetIds.literalEditOf myId

fdConfig :: _ => m FocusDelegator.Config
fdConfig =
    Lens.view id
    <&> \env ->
    let litConf = env ^. has . Config.literal
        menuConfig = env ^. has
    in
    FocusDelegator.Config
    { FocusDelegator.focusChildKeys = litConf ^. Config.literalStartEditingKeys
    , FocusDelegator.focusChildDoc =
        E.toDoc env
        [ has . MomentuTexts.edit
        , has . Texts.literal
        , has . Texts.startEditing
        ]
    , FocusDelegator.focusParentKeys =
        litConf ^. Config.literalStopEditingKeys
        -- The literal edit should behave like holes, in that the "pick option"
        -- key goes to the resulting expr.
        <> menuConfig ^. Menu.configKeysPickOption
        -- Only taken when the literal edit doesn't handle it by
        -- jumping to next entry:
        <> menuConfig ^. Menu.configKeysPickOptionAndGotoNext
    , FocusDelegator.focusParentDoc =
        E.toDoc env
        [ has . MomentuTexts.edit
        , has . Texts.literal
        , has . Texts.stopEditing
        ]
    }

textEdit :: _ => Property o Text -> Sugar.Payload v o -> m (M.TextWidget o)
textEdit prop pl =
    do
        text <- TextEdits.make ?? emptyTexts ?? prop ?? WidgetIds.literalEditOf (WidgetIds.fromExprPayload pl)
        label Texts.textOpener
            M./|/ pure text
            M./|/ ((M.tValue %~)
                    <$> (Element.padToSize ?? (text ^. Element.size & _1 .~ 0) ?? 1)
                    <*> label Texts.textCloser
                )
    & withStyle Style.text

emptyTexts :: TextEdit.Modes Text
emptyTexts = TextEdit.Modes "" ""

parseNum :: Text -> Maybe Double
parseNum newText
    | newText /= Text.strip newText = Nothing
    | newText `elem` ["", "-", ".", "-."] = Just 0
    | otherwise = tryParse newText

numEdit :: _ => Property o Double -> Sugar.Payload v o -> m (M.TextWidget o)
numEdit prop pl =
    do
        text <-
            M.readWidgetState myId
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
                          [ has . MomentuTexts.edit
                          , has . Texts.literal
                          , has . Texts.negate
                          ]) "-"
                | otherwise = mempty
        strollEvent <-
            Lens.view (has . Menu.configKeysPickOptionAndGotoNext)
            <&>
            \keys ->
            E.keysEventMap keys
            (toDoc
                [ has . MomentuTexts.navigation
                , has . Texts.nextEntry
                ])
            (pure ())
            <&> Lens.mapped . GuiState.uPreferStroll .~ True ^. Lens._Unwrapped
        let delEvent =
                case pl ^? Sugar.plActions . Sugar.delete . (Sugar._SetToHole <> Sugar._Delete) of
                -- Allow to delete when text is empty
                Just action | Text.null text ->
                    E.keyPresses [noMods ModKey.Key'Backspace]
                    (toDoc [has . MomentuTexts.edit, has . MomentuTexts.delete])
                    (action <&> WidgetIds.fromEntityId <&> GuiState.updateCursor)
                    <>
                    E.charEventMap "Letter"
                    (toDoc [has . MomentuTexts.edit, has . Texts.replace])
                    holeWithChar
                    where
                        holeWithChar c =
                            (action <&> WidgetIds.fromEntityId
                                <&> SearchMenu.enterWithSearchTerm (Text.singleton c))
                            <$ guard (Char.isAlpha c)
                _ -> mempty
        newLiteralEvent <-
            if Text.null text
            then ExprEventMap.makeLiteralEventMap ?? pl ^. Sugar.plActions . Sugar.setToLiteral
            else pure mempty
        TextEdit.make ?? empty ?? text ?? innerId
            <&> M.tValue . Widget.eventMapMaker . Lens.mapped %~
                -- Avoid taking keys that don't belong to us,
                -- so weakerEvents with them will work.
                E.filter (Lens.has Lens._Just . parseNum . fst)
            <&> M.tValue . Widget.updates %~ event
            <&> M.tValue %~ Widget.strongerEvents (negateEvent <> delEvent <> newLiteralEvent <> strollEvent)
            <&> M.tValue %~ Widget.addPreEvent preEvent
        & withStyle Style.num
    where
        prevVal = prop ^. Property.pVal
        setPos newPos = TextEdit.encodeCursor innerId newPos & GuiState.updateCursor
        innerId = WidgetIds.literalEditOf myId
        curVal = prop ^. Property.pVal
        event (newText, update) =
            GuiState.updateWidgetState myId newText <> update <$
            traverse_ (prop ^. Property.pSet) (parseNum newText)
        empty =
            TextEdit.Modes
            { TextEdit._unfocused = "0"
            , TextEdit._focused = ""
            }
        myId = WidgetIds.fromExprPayload pl

make ::
    _ =>
    Annotated (ExprGui.Payload i o) # Const (Sugar.Literal (Property o)) ->
    GuiM env i o (Responsive o)
make (Ann (Const p) (Const lit)) =
    (FocusDelegator.make <*> fdConfig ?? FocusDelegator.FocusEntryParent ?? WidgetIds.fromExprPayload p
        <&> (M.tValue %~)) <*>
    case lit of
    Sugar.LiteralNum x -> numEdit x p
    Sugar.LiteralBytes x -> bytesEdit x p
    Sugar.LiteralChar x -> charEdit x p
    Sugar.LiteralText x -> textEdit x p
    <&> Responsive.fromWithTextPos
    & stdWrap p
