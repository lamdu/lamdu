{-# LANGUAGE TemplateHaskell, ViewPatterns, NamedFieldPuns #-}
module GUI.Momentu.Widgets.TextEdit
    ( Style(..), sCursorColor, sCursorWidth, sEmptyStringsColor, sTextViewStyle
    , HasStyle(..)
    , EmptyStrings(..), emptyFocusedString, emptyUnfocusedString
    , make
    , defaultStyle
    , getCursor, encodeCursor
    ) where

import qualified Control.Lens as Lens
import           Data.Aeson.TH (deriveJSON)
import qualified Data.Aeson.Types as Aeson
import qualified Data.Binary.Extended as Binary
import           Data.Char (isSpace, toLower)
import           Data.List.Extended (genericLength, minimumOn)
import           Data.List.Lens (prefixed)
import qualified Data.Text as Text
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Align (WithTextPos(..))
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Animation as Anim
import qualified GUI.Momentu.Direction as Direction
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Font as Font
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.ModKey (ModKey(..))
import qualified GUI.Momentu.ModKey as ModKey
import           GUI.Momentu.Rect (Rect(..))
import qualified GUI.Momentu.Rect as Rect
import qualified GUI.Momentu.State as State
import           GUI.Momentu.Widget (Widget(..))
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Graphics.DrawingCombinators as Draw

import           Lamdu.Prelude

type Cursor = Int

data Style = Style
    { _sCursorColor :: Draw.Color
    , _sCursorWidth :: Widget.R
    , _sEmptyStringsColor :: Draw.Color
    , _sTextViewStyle :: TextView.Style
    }
Lens.makeLenses ''Style

class TextView.HasStyle env => HasStyle env where style :: Lens' env Style
instance HasStyle Style where style = id

data EmptyStrings = EmptyStrings
    { _emptyUnfocusedString :: Text
    , _emptyFocusedString :: Text
    } deriving (Eq, Show)
Lens.makeLenses ''EmptyStrings

deriveJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier = (Lens.ix 0 %~ toLower) . (^?! prefixed "_empty")
    } ''EmptyStrings

instance TextView.HasStyle Style where style = sTextViewStyle

defaultStyle :: TextView.Style -> Style
defaultStyle tvStyle =
    Style
    { _sCursorColor = Draw.Color 0 1 0 1
    , _sEmptyStringsColor = Draw.Color r g b a
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
    Style -> Text -> Text -> Widget.Id ->
    WithTextPos (Widget (Text, State.Update))
makeInternal s str emptyStr myId =
    v
    & Align.tValue %~ Widget.fromView
    & Align.tValue . Widget.wState . Widget._StateUnfocused . Widget.uMEnter
        ?~ enterFromDirection (v ^. Element.size) s str myId
    where
        (displayStr, setColor)
            | Text.null str = (emptyStr, TextView.color .~ s ^. sEmptyStringsColor)
            | otherwise = (Text.take 5000 str, id)
        v = TextView.make (setColor (s ^. sTextViewStyle)) displayStr animId
            & Element.pad (Vector2 (s ^. sCursorWidth / 2) 0)
        animId = Widget.toAnimId myId

makeUnfocused :: EmptyStrings -> Style -> Text -> Widget.Id -> WithTextPos (Widget (Text, State.Update))
makeUnfocused empty s str =
    makeInternal s str (empty ^. emptyUnfocusedString)

minimumIndex :: Ord a => [a] -> Int
minimumIndex xs =
    xs ^@.. Lens.traversed & minimumOn snd & fst

cursorNearRect :: TextView.Style -> Text -> Rect -> Cursor
cursorNearRect s str fromRect =
    cursorRects s str <&> Rect.sqrDistance fromRect
    & minimumIndex -- cursorRects(TextView.letterRects) should never return an empty list

enterFromDirection ::
    Widget.Size -> Style -> Text -> Widget.Id ->
    Direction.Direction -> Widget.EnterResult (Text, State.Update)
enterFromDirection sz sty str myId dir =
    encodeCursor myId cursor
    & State.updateCursor
    & (,) str
    & Widget.EnterResult cursorRect 0
    where
        cursor =
            case dir of
            Direction.Outside -> Text.length str
            Direction.Point x -> Rect x 0 & fromRect
            Direction.FromLeft  r -> Rect 0 0    & Rect.verticalRange   .~ r & fromRect
            Direction.FromRight r -> edgeRect _1 & Rect.verticalRange   .~ r & fromRect
            Direction.FromAbove r -> Rect 0 0    & Rect.horizontalRange .~ r & fromRect
            Direction.FromBelow r -> edgeRect _2 & Rect.horizontalRange .~ r & fromRect
        edgeRect l = Rect (0 & Lens.cloneLens l .~ sz ^. Lens.cloneLens l) 0
        cursorRect = mkCursorRect sty cursor str
        fromRect = cursorNearRect (sty ^. sTextViewStyle) str

eventResult :: Widget.Id -> Text -> Cursor -> (Text, State.Update)
eventResult myId newText newCursor =
    ( newText
    , encodeCursor myId newCursor & State.updateCursor
    )

-- | Note: maxLines prevents the *user* from exceeding it, not the
-- | given text...
makeFocused ::
    Cursor -> EmptyStrings -> Style -> Text -> Widget.Id ->
    WithTextPos (Widget (Text, State.Update))
makeFocused cursor empty s str myId =
    makeInternal s str (empty ^. emptyFocusedString) myId
    & Element.bottomLayer <>~ cursorFrame
    & Align.tValue %~ Widget.setFocusedWith cursorRect (eventMap cursor str myId)
    where
        cursorRect@(Rect origin size) = mkCursorRect s cursor str
        cursorFrame =
            Anim.unitSquare ["text-cursor"]
            & Anim.unitImages %~ Draw.tint (s ^. sCursorColor)
            & unitIntoCursorRect
        unitIntoCursorRect img =
            img
            & Anim.scale size
            & Anim.translate origin

mkCursorRect :: Style -> Cursor -> Text -> Rect
mkCursorRect s cursor str =
    Rect cursorPos cursorSize
    where
        beforeCursorLines = Text.splitOn "\n" $ Text.take cursor str
        lineHeight = TextView.lineHeight (s ^. sTextViewStyle)
        cursorPos = Vector2 cursorPosX cursorPosY
        cursorSize = Vector2 (s ^. sCursorWidth) lineHeight
        cursorPosX =
            TextView.drawText (s ^. sTextViewStyle) (last beforeCursorLines) ^.
            TextView.renderedTextSize . Font.advance . _1
        cursorPosY = lineHeight * (genericLength beforeCursorLines - 1)

-- TODO: Implement intra-TextEdit virtual cursor
eventMap ::
    Cursor -> Text -> Widget.Id -> Widget.EventContext ->
    EventMap (Text, State.Update)
eventMap cursor str myId _eventContext =
    mconcat . concat $ [
        [ E.keyPressOrRepeat (noMods MetaKey.Key'Left) (moveDoc ["left"]) $
            moveRelative (-1)
        | cursor > 0 ],

        [ E.keyPressOrRepeat (noMods MetaKey.Key'Right) (moveDoc ["right"]) $
            moveRelative 1
        | cursor < textLength ],

        [ keys (moveDoc ["word", "left"]) [ctrl MetaKey.Key'Left]
            backMoveWord
        | cursor > 0 ],

        [ keys (moveDoc ["word", "right"]) [ctrl MetaKey.Key'Right] moveWord
        | cursor < textLength ],

        [ E.keyPressOrRepeat (noMods MetaKey.Key'Up) (moveDoc ["up"]) $
            moveRelative (- cursorX - 1 - Text.length (Text.drop cursorX prevLine))
        | cursorY > 0 ],

        [ E.keyPressOrRepeat (noMods MetaKey.Key'Down) (moveDoc ["down"]) $
            moveRelative
            (Text.length curLineAfter + 1 +
             min cursorX (Text.length nextLine))
        | cursorY < lineCount - 1 ],

        [ keys (moveDoc ["beginning of line"]) homeKeys $
            moveRelative (-cursorX)
        | cursorX > 0 ],

        [ keys (moveDoc ["end of line"]) endKeys $
          moveRelative (Text.length curLineAfter)
        | not . Text.null $ curLineAfter ],

        [ keys (moveDoc ["beginning of text"]) homeKeys $
            moveAbsolute 0
        | cursorX == 0 && cursor > 0 ],

        [ keys (moveDoc ["end of text"]) endKeys $
            moveAbsolute textLength
        | Text.null curLineAfter && cursor < textLength ],

        [ keys (deleteDoc ["backwards"]) [noMods MetaKey.Key'Backspace] $
            backDelete 1
        | cursor > 0 ],

        [ keys (deleteDoc ["word", "backwards"]) [ctrl MetaKey.Key'W]
            backDeleteWord
        | cursor > 0 ],

        let swapPoint = min (textLength - 2) (cursor - 1)
            (beforeSwap, Text.unpack -> x:y:afterSwap) = Text.splitAt swapPoint str
            swapLetters =
                min textLength (cursor + 1)
                & eventRes (beforeSwap <> Text.pack (y:x:afterSwap))

        in

        [ keys (editDoc ["Swap letters"]) [ctrl MetaKey.Key'T]
            swapLetters
        | cursor > 0 && textLength >= 2 ],

        [ keys (deleteDoc ["forward"]) [noMods MetaKey.Key'Delete] $
            delete 1
        | cursor < textLength ],

        [ keys (deleteDoc ["word", "forward"]) [alt MetaKey.Key'D]
            deleteWord
        | cursor < textLength ],

        [ keys (deleteDoc ["till", "end of line"]) [ctrl MetaKey.Key'K] $
            delete (Text.length curLineAfter)
        | not . Text.null $ curLineAfter ],

        [ keys (deleteDoc ["newline"]) [ctrl MetaKey.Key'K] $
            delete 1
        | Text.null curLineAfter && cursor < textLength ],

        [ keys (deleteDoc ["till", "beginning of line"]) [ctrl MetaKey.Key'U] $
            backDelete (Text.length curLineBefore)
        | not . Text.null $ curLineBefore ],

        [ E.filterChars (`notElem` (" \n" :: String)) .
            E.allChars "Character" (insertDoc ["character"]) $
            insert . Text.singleton
        ],

        [ keys (insertDoc ["Newline"])
            [noMods MetaKey.Key'Enter, ModKey.shift MetaKey.Key'Enter] (insert "\n") ],

        [ keys (insertDoc ["Space"])
            [noMods MetaKey.Key'Space, ModKey.shift MetaKey.Key'Space] (insert " ") ],

        [ E.pasteOnKey (cmd MetaKey.Key'V) (E.Doc ["Clipboard", "Paste"]) insert ]

        ]
    where
        editDoc = E.Doc . ("Edit" :)
        deleteDoc = editDoc . ("Delete" :)
        insertDoc = editDoc . ("Insert" :)
        moveDoc = E.Doc . ("Navigation" :) . ("Move" :)
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
    (MonadReader env m, State.HasCursor env, HasStyle env) =>
    m ( EmptyStrings -> Text -> Widget.Id ->
        WithTextPos (Widget (Text, State.Update))
      )
make =
    do
        get <- getCursor
        s <- Lens.view style
        pure $ \empty str myId ->
            case get str myId of
            Nothing -> makeUnfocused empty s str myId
            Just pos -> makeFocused pos empty s str myId
