{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE TemplateHaskell, ViewPatterns, NamedFieldPuns, RankNTypes #-}
{-# LANGUAGE DerivingVia, MultiParamTypeClasses, ConstraintKinds #-}
module GUI.Momentu.Widgets.TextEdit
    ( Style(..)
        , sCursorColor, sCursorWidth, sEmptyStringsColors, sTextViewStyle
    , Modes(..), focused, unfocused
    , EmptyStrings
    , make, makeWithAnimId
    , defaultStyle
    , getCursor, encodeCursor
    , Texts(..)
    , Deps, HasTexts
    ) where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH
import qualified Data.Binary.Extended as Binary
import           Data.Char (isSpace)
import           Data.List.Extended (genericLength, minimumOn)
import qualified Data.Text as Text
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Align (TextWidget)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Animation as Anim
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.FocusDirection (FocusDirection(..))
import qualified GUI.Momentu.Font as Font
import qualified GUI.Momentu.I18N as MomentuTexts
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.ModKey (ModKey(..))
import qualified GUI.Momentu.ModKey as ModKey
import           GUI.Momentu.Rect (Rect(..))
import qualified GUI.Momentu.Rect as Rect
import qualified GUI.Momentu.State as State
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Graphics.DrawingCombinators as Draw

import           GUI.Momentu.Prelude

data Texts a = Texts
    { _textWord :: a
    , _textTill :: a
    , _textNewline :: a
    , _textSpace :: a
    , _textBeginningOfLine :: a
    , _textEndOfLine :: a
    , _textBeginningOfText :: a
    , _textEndOfText :: a
    , _textSwapLetters :: a
    , _textCharacter :: a
    , _textClipboard :: a
    , _textPaste :: a
    , _textCopy :: a
    } deriving Eq

JsonTH.derivePrefixed "_text" ''Texts

type HasTexts env =
    ( Has (Dir.Texts Text) env, Has Dir.Layout env
    , Has (MomentuTexts.Texts Text) env
    , Has (Texts Text) env
    )

Lens.makeLenses ''Texts

type Cursor = Int

data Modes a = Modes
    { _unfocused :: a
    , _focused :: a
    } deriving stock (Generic, Generic1, Eq, Ord, Show, Functor, Foldable, Traversable)
    deriving Applicative via Generically1 Modes
Lens.makeLenses ''Modes
JsonTH.derivePrefixed "_" ''Modes

type EmptyStrings = Modes Text

data Style = Style
    { _sCursorColor :: Draw.Color
    , _sCursorWidth :: Widget.R
    , _sEmptyStringsColors :: Modes Draw.Color
    , _sTextViewStyle :: TextView.Style
    }
Lens.makeLenses ''Style

type HasStyle env = (Has Style env, Has TextView.Style env)

type Deps env = (HasStyle env, HasTexts env)

instance Has TextView.Style Style where has = sTextViewStyle

defaultStyle :: TextView.Style -> Style
defaultStyle tvStyle =
    Style
    { _sCursorColor = Draw.Color 0 1 0 1
    , _sEmptyStringsColors = pure (Draw.Color r g b a)
    , _sCursorWidth = 4
    , _sTextViewStyle = tvStyle
    }
    where
        Draw.Color r g b a = tvStyle ^. TextView.color

tillEndOfWord :: Text -> Text
tillEndOfWord xs = spaces <> nonSpaces
    where
        spaces = Text.takeWhile isSpace xs
        nonSpaces = Text.dropWhile isSpace xs & Text.takeWhile (not . isSpace)

encodeCursor :: Widget.Id -> Cursor -> Widget.Id
encodeCursor myId = Widget.joinId myId . (:[]) . Binary.encodeS

rightSideOfRect :: Rect -> Rect
rightSideOfRect rect =
    rect
    & Rect.left .~ rect ^. Rect.right
    & Rect.width .~ 0

cursorRects :: TextView.Style -> Text -> [Rect]
cursorRects s str =
    TextView.letterRects s str
    <&> Lens.mapped %~ rightSideOfRect
    & zipWith addFirstCursor (iterate (+lineHeight) 0)
    -- A bit ugly: letterRects returns rects for all but newlines, and
    -- returns a list of lines. Then addFirstCursor adds the left-most
    -- cursor of each line, thereby the number of rects becomes the
    -- original number of letters which can be used to match the
    -- original string index-wise.
    & concat
    where
        addFirstCursor y = (Rect (Vector2 0 y) (Vector2 0 lineHeight) :)
        lineHeight = TextView.lineHeight s

makeInternal ::
    (Has Dir.Layout env, HasStyle env) =>
    env -> (forall a. Lens.Getting a (Modes a) a) ->
    Text -> EmptyStrings -> Anim.AnimId -> Widget.Id -> TextWidget ((,) Text)
makeInternal env mode str emptyStrings animId myId =
    v
    & Align.tValue %~ Widget.fromView
    & Align.tValue . Widget.wState . Widget._StateUnfocused . Widget.uMEnter ?~
        Widget.enterFuncAddVirtualCursor (Rect 0 (v ^. Element.size))
        (enterFromDirection env (v ^. Element.size) str myId)
    where
        emptyColor = env ^. has . sEmptyStringsColors . mode
        emptyView = mkView (emptyStrings ^. mode) (TextView.color .~ emptyColor)
        nonEmptyView =
            mkView (Text.take 5000 str) id
            & Align.tValue %~ Element.padToSize env (emptyView ^. Element.size) 0
        v = if Text.null str then emptyView else nonEmptyView
        mkView displayStr setColor =
            TextView.make (setColor env) displayStr animId
            & Element.padAround (Vector2 (env ^. has . sCursorWidth / 2) 0)

minimumIndex :: Ord a => [a] -> Int
minimumIndex xs =
    xs ^@.. Lens.traversed & minimumOn snd & fst

cursorNearRect :: TextView.Style -> Text -> Rect -> Cursor
cursorNearRect s str fromRect =
    cursorRects s str <&> Rect.sqrDistance fromRect
    & minimumIndex -- cursorRects(TextView.letterRects) should never return an empty list

enterFromDirection ::
    (Has Dir.Layout env, HasStyle env) =>
    env -> Widget.Size -> Text -> Widget.Id ->
    FocusDirection -> Widget.EnterResult (Text, State.Update)
enterFromDirection env sz str myId dir =
    encodeCursor myId cursor
    & State.updateCursor
    & (,) str
    & Widget.EnterResult cursorRect 0
    where
        cursor =
            case dir of
            Point x -> Rect x 0 & fromRect
            FromOutside -> Text.length str
            FromLeft  r -> Rect 0 0    & Rect.verticalRange   .~ r & fromRect
            FromRight r -> edgeRect _1 & Rect.verticalRange   .~ r & fromRect
            FromAbove r -> Rect 0 0    & Rect.horizontalRange .~ r & fromRect
            FromBelow r -> edgeRect _2 & Rect.horizontalRange .~ r & fromRect
        edgeRect l = Rect (0 & Lens.cloneLens l .~ sz ^. Lens.cloneLens l) 0
        cursorRect = mkCursorRect env cursor str
        fromRect = cursorNearRect (env ^. has . sTextViewStyle) str

eventResult :: Widget.Id -> Text -> Cursor -> (Text, State.Update)
eventResult myId newText newCursor =
    ( newText
    , encodeCursor myId newCursor & State.updateCursor
    )

-- | Note: maxLines prevents the *user* from exceeding it, not the
-- | given text...
makeFocused ::
    (HasTexts env, HasStyle env) =>
    env -> Text -> EmptyStrings -> Cursor -> Anim.AnimId -> Widget.Id ->
    TextWidget ((,) Text)
makeFocused env str empty cursor animId myId =
    makeInternal env focused str empty animId myId
    & Element.bottomLayer <>~ cursorFrame
    & Align.tValue %~
        Widget.setFocusedWith cursorRect
        (eventMap env cursor str myId)
    where
        actualStr = if Text.null str then empty ^. focused else str
        cursorRect@(Rect origin size) = mkCursorRect env cursor actualStr
        cursorFrame =
            Anim.unitSquare ["text-cursor"]
            & Anim.unitImages %~ Draw.tint (env ^. has . sCursorColor)
            & unitIntoCursorRect
        unitIntoCursorRect img =
            img
            & Anim.scale size
            & Anim.translate origin

mkCursorRect :: (HasStyle env, Has Dir.Layout env) => env -> Cursor -> Text -> Rect
mkCursorRect env cursor str =
    Rect cursorPos cursorSize
    where
        beforeCursorLines = Text.splitOn "\n" $ Text.take cursor str
        lineHeight = TextView.lineHeight (env ^. has . sTextViewStyle)
        maybeMirror =
            case env ^. has of
            Dir.LeftToRight -> id
            Dir.RightToLeft -> (totalWidth -)
        cursorPos = Vector2 (maybeMirror cursorPosX) cursorPosY
        cursorSize = Vector2 cursorWidth lineHeight
        cursorWidth = env ^. has . sCursorWidth
        totalWidth = draw str ^. TextView.renderedTextSize . Font.bounding . _1
        draw = TextView.drawText env
        cursorPosX =
            draw (last beforeCursorLines) ^.
            TextView.renderedTextSize . Font.advance . _1
        cursorPosY = lineHeight * (genericLength beforeCursorLines - 1)

-- TODO: Implement intra-TextEdit virtual cursor
eventMap ::
    HasTexts env =>
    env -> Cursor -> Text -> Widget.Id -> Widget.EventContext ->
    EventMap (Text, State.Update)
eventMap env cursor str myId _eventContext =
    mconcat $ concat [
        [ moveRelative (-1) & logicalRetreat | cursor > 0 ],
        [ moveRelative   1  & logicalAdvance | cursor < textLength ],
        [ logicalRetreatWord backMoveWord | cursor > 0 ],
        [ logicalAdvanceWord moveWord | cursor < textLength ],

        [ moveRelative (- cursorX - 1 - Text.length (Text.drop cursorX prevLine))
            & E.keyPressOrRepeat (noMods MetaKey.Key'Up)
            (moveDoc [has . Dir.up])
        | cursorY > 0 ],

        [ moveRelative
            (Text.length curLineAfter + 1 + min cursorX (Text.length nextLine))
            & E.keyPressOrRepeat (noMods MetaKey.Key'Down)
            (moveDoc [has . Dir.down])
        | cursorY < lineCount - 1 ],

        [ moveRelative (-cursorX)
            & keys (moveDoc [has . textBeginningOfLine]) homeKeys
        | cursorX > 0 ],

        [ moveRelative (Text.length curLineAfter)
            & keys (moveDoc [has . textEndOfLine]) endKeys
        | not . Text.null $ curLineAfter ],

        [ moveAbsolute 0 & keys (moveDoc [has . textBeginningOfText]) homeKeys
        | cursorX == 0 && cursor > 0 ],

        [ moveAbsolute textLength
            & keys (moveDoc [has . textEndOfText]) endKeys
        | Text.null curLineAfter && cursor < textLength ],

        [ backDelete 1
            & keys (deleteDoc [has . MomentuTexts.backward]) [noMods MetaKey.Key'Backspace]
        | cursor > 0 ],

        [ keys (deleteDoc [has . textWord, has . MomentuTexts.backward]) [ctrl MetaKey.Key'W]
            backDeleteWord
        | cursor > 0 ],

        let swapPoint = min (textLength - 2) (cursor - 1)
            (beforeSwap, Text.unpack -> x:y:afterSwap) = Text.splitAt swapPoint str
            swapLetters =
                min textLength (cursor + 1)
                & eventRes (beforeSwap <> Text.pack (y:x:afterSwap))

        in

        [ keys (editDoc [has . textSwapLetters]) [ctrl MetaKey.Key'T]
            swapLetters
        | cursor > 0 && textLength >= 2 ],

        [ delete 1
            & keys (deleteDoc [has . MomentuTexts.forward]) [noMods MetaKey.Key'Delete]
        | cursor < textLength ],

        [ keys (deleteDoc [has . textWord, has . MomentuTexts.forward]) [alt MetaKey.Key'D]
            deleteWord
        | cursor < textLength ],

        [ delete (Text.length curLineAfter)
            & keys (deleteDoc [has . textTill, has . textEndOfLine])
            [ctrl MetaKey.Key'K]
        | not . Text.null $ curLineAfter ],

        [ delete 1 & keys (deleteDoc [has . textNewline]) [ctrl MetaKey.Key'K]
        | Text.null curLineAfter && cursor < textLength ],

        [ backDelete (Text.length curLineBefore)
            & keys (deleteDoc [has . textTill, has . textBeginningOfLine])
            [ctrl MetaKey.Key'U]
        | not . Text.null $ curLineBefore ],

        [ E.filterChars (`notElem` (" \n" :: String)) .
            E.allChars "Character" (insertDoc [has . textCharacter]) $
            insert . Text.singleton
        ],

        [ keys (insertDoc [has . textNewline])
            [noMods MetaKey.Key'Enter, ModKey.shift MetaKey.Key'Enter] (insert "\n") ],

        [ keys (insertDoc [has . textSpace])
            [noMods MetaKey.Key'Space, ModKey.shift MetaKey.Key'Space] (insert " ") ],

        [ E.pasteOnKey (cmd MetaKey.Key'V)
            (toDoc [has . textClipboard, has . textPaste]) insert ],

        [ keys (toDoc [has . textClipboard, has . textCopy])
            [cmd MetaKey.Key'C] (str, mempty & State.uSetSystemClipboard ?~ str ) ]

        ]
    where
        leftWord = keys (moveWordDoc [has . Dir.left]) [ctrl MetaKey.Key'Left]
        rightWord = keys (moveWordDoc [has . Dir.right]) [ctrl MetaKey.Key'Right]
        left =
            E.keyPressOrRepeat (noMods MetaKey.Key'Left)
            (moveDoc [has . Dir.left])
        right =
            E.keyPressOrRepeat (noMods MetaKey.Key'Right)
            (moveDoc [has . Dir.right])
        ( logicalRetreatWord, logicalAdvanceWord
            , logicalRetreat, logicalAdvance) =
            case env ^. has of
            Dir.LeftToRight -> (leftWord , rightWord, left , right)
            Dir.RightToLeft -> (rightWord, leftWord , right, left )
        editDoc = toDoc . (has . MomentuTexts.edit :)
        deleteDoc = editDoc . (has . MomentuTexts.delete :)
        insertDoc = editDoc . (has . MomentuTexts.insert :)
        moveWordDoc = moveDoc . (has . textWord :)
        toDoc = E.toDoc env
        moveDoc =
            toDoc
            . (has . MomentuTexts.navigation :)
            . (has . MomentuTexts.move :)
        splitLines = Text.splitOn "\n"
        linesBefore = reverse (splitLines before)
        linesAfter = splitLines after
        prevLine = linesBefore !! 1
        nextLine = linesAfter !! 1
        curLineBefore = head linesBefore
        curLineAfter = head linesAfter
        cursorX = Text.length curLineBefore
        cursorY = length linesBefore - 1

        eventRes = eventResult myId
        moveAbsolute a = eventRes str . max 0 $ min (Text.length str) a
        moveRelative d = moveAbsolute (cursor + d)
        backDelete n = eventRes (Text.take (cursor-n) str <> Text.drop cursor str) (cursor-n)
        delete n = eventRes (before <> Text.drop n after) cursor
        insert l =
            eventRes str' cursor'
            where
                cursor' = cursor + Text.length l
                str' = mconcat [before, l, after]

        backDeleteWord =
            backDelete . Text.length . tillEndOfWord $ Text.reverse before
        deleteWord = delete . Text.length $ tillEndOfWord after

        backMoveWord =
            moveRelative . negate . Text.length . tillEndOfWord $
            Text.reverse before
        moveWord = moveRelative . Text.length $ tillEndOfWord after

        keys = flip E.keyPresses

        noMods = ModKey mempty
        cmd = MetaKey.toModKey . MetaKey.cmd
        ctrl = ModKey.ctrl
        alt = ModKey.alt
        homeKeys = [noMods MetaKey.Key'Home, ctrl MetaKey.Key'A]
        endKeys = [noMods MetaKey.Key'End, ctrl MetaKey.Key'E]
        textLength = Text.length str
        lineCount = length $ Text.splitOn "\n" str
        (before, after) = Text.splitAt cursor str

getCursor ::
    (MonadReader env m, State.HasCursor env) =>
    m (Text -> Widget.Id -> Maybe Int)
getCursor =
    State.subId <&> f
    where
        f sub str myId =
            sub myId <&> decodeCursor
            where
                decodeCursor [x] = min (Text.length str) $ Binary.decodeS x
                decodeCursor _ = Text.length str

make ::
    (MonadReader env m, State.HasCursor env, HasStyle env, HasTexts env) =>
    m ( EmptyStrings -> Text -> Widget.Id ->
        TextWidget ((,) Text)
      )
make = makeWithAnimId <&> Lens.mapped . Lens.mapped %~ \f w -> f (Widget.toAnimId w) w

makeWithAnimId ::
    (MonadReader env m, State.HasCursor env, HasStyle env, HasTexts env) =>
    m ( EmptyStrings -> Text -> Anim.AnimId -> Widget.Id ->
        TextWidget ((,) Text)
      )
makeWithAnimId =
    do
        get <- getCursor
        env <- Lens.view id
        pure $ \empty str animId myId ->
            case get str myId of
            Nothing -> makeInternal env unfocused str empty animId myId
            Just pos -> makeFocused env str empty pos animId  myId
