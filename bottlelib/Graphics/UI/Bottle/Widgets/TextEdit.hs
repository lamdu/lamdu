{-# LANGUAGE NoImplicitPrelude, RecordWildCards, OverloadedStrings, TemplateHaskell, ViewPatterns #-}
module Graphics.UI.Bottle.Widgets.TextEdit
    ( Cursor
    , Style(..)
        , sCursorColor, sCursorWidth, sEmptyUnfocusedString
        , sEmptyFocusedString, sTextViewStyle
    , make
    , defaultCursorColor
    , defaultCursorWidth
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import qualified Data.Binary.Utils as BinUtils
import           Data.Char (isSpace)
import           Data.List (genericLength)
import           Data.List.Utils (minimumOn)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.DrawingCombinators.Utils (TextSize(..))
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.Direction as Direction
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.ModKey (ModKey(..))
import qualified Graphics.UI.Bottle.ModKey as ModKey
import           Graphics.UI.Bottle.Rect (Rect(..))
import qualified Graphics.UI.Bottle.Rect as Rect
import           Graphics.UI.Bottle.Widget (Widget(..))
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import qualified Graphics.UI.GLFW as GLFW

import           Prelude.Compat

type Cursor = Int

data Style = Style
    { _sCursorColor :: Draw.Color
    , _sCursorWidth :: Widget.R
    , _sEmptyUnfocusedString :: Text
    , _sEmptyFocusedString :: Text
    , _sTextViewStyle :: TextView.Style
    }
Lens.makeLenses ''Style

-- TODO: Replace with a defaultStyle :: TextViewStyle -> .. -> Style
defaultCursorColor :: Draw.Color
defaultCursorColor = Draw.Color 0 1 0 1

defaultCursorWidth :: Widget.R
defaultCursorWidth = 4

tillEndOfWord :: Text -> Text
tillEndOfWord xs = spaces <> nonSpaces
    where
        spaces = Text.takeWhile isSpace xs
        nonSpaces = Text.dropWhile isSpace xs & Text.takeWhile (not . isSpace)

makeDisplayStr :: Text -> Text -> Text
makeDisplayStr empty ""  = empty
makeDisplayStr _     str = str

encodeCursor :: Widget.Id -> Int -> Widget.Id
encodeCursor myId = Widget.joinId myId . (:[]) . BinUtils.encodeS

rightSideOfRect :: Rect -> Rect
rightSideOfRect rect =
    rect
    & Rect.left .~ rect ^. Rect.right
    & Rect.width .~ 0

cursorRects :: TextView.Style -> Text -> [Rect]
cursorRects style str =
    concat .
    -- A bit ugly: letterRects returns rects for all but newlines, and
    -- returns a list of lines. Then addFirstCursor adds the left-most
    -- cursor of each line, thereby the number of rects becomes the
    -- original number of letters which can be used to match the
    -- original string index-wise.
    zipWith addFirstCursor (iterate (+lineHeight) 0) .
    (map . map) rightSideOfRect $
    TextView.letterRects style str
    where
        addFirstCursor y = (Rect (Vector2 0 y) (Vector2 0 lineHeight) :)
        lineHeight = TextView.lineHeight style

makeUnfocused :: Style -> Text -> Widget.Id -> Widget (Text, Widget.EventResult)
makeUnfocused Style{..} str myId =
    TextView.makeWidget _sTextViewStyle displayStr animId
    & Widget.pad (Vector2 (_sCursorWidth / 2) 0)
    & Widget.mEnter .~ Just (enterFromDirection Style{..} str myId)
    where
        animId = Widget.toAnimId myId
        displayStr = makeDisplayStr _sEmptyUnfocusedString str

minimumIndex :: Ord a => [a] -> Int
minimumIndex xs =
    xs ^@.. Lens.traversed & minimumOn snd & fst

cursorNearRect :: TextView.Style -> Text -> Rect -> Int
cursorNearRect style str fromRect =
    cursorRects style str <&> Rect.distance fromRect
    & minimumIndex -- cursorRects(TextView.letterRects) should never return an empty list

enterFromDirection ::
    Style -> Text -> Widget.Id ->
    Direction.Direction -> Widget.EnterResult (Text, Widget.EventResult)
enterFromDirection Style{..} str myId dir =
    Widget.EnterResult cursorRect .
    (,) str . Widget.eventResultFromCursor $
    encodeCursor myId cursor
    where
        cursor =
            case dir of
            Direction.Outside -> Text.length str
            Direction.PrevFocalArea rect -> cursorNearRect _sTextViewStyle str rect
            Direction.Point x -> cursorNearRect _sTextViewStyle str $ Rect x 0
        cursorRect = mkCursorRect Style{..} cursor str

eventResult :: Widget.Id -> Text -> Int -> (Text, Widget.EventResult)
eventResult myId newText newCursor =
    ( newText
    , encodeCursor myId newCursor & Widget.eventResultFromCursor
    )

-- | Note: maxLines prevents the *user* from exceeding it, not the
-- | given text...
makeFocused ::
    Cursor -> Style -> Text -> Widget.Id ->
    Widget (Text, Widget.EventResult)
makeFocused cursor Style{..} str myId =
    makeUnfocused Style{..} str myId
    & Widget.bottomFrame <>~ cursorFrame
    & Widget.mFocus .~
        Just Widget.Focus
        { _focalArea = cursorRect
        , _fEventMap = eventMap cursor str displayStr myId
        }
    where
        displayStr = makeDisplayStr _sEmptyFocusedString str
        cursorRect = mkCursorRect Style{..} cursor str
        cursorFrame =
            Anim.unitSquare (Widget.cursorAnimId ++ ["text"])
            & Anim.unitImages %~ Draw.tint _sCursorColor
            & Anim.unitIntoRect cursorRect

mkCursorRect :: Style -> Int -> Text -> Rect
mkCursorRect Style{..} cursor str = Rect cursorPos cursorSize
    where
        beforeCursorLines = Text.splitOn "\n" $ Text.take cursor str
        lineHeight = TextView.lineHeight _sTextViewStyle
        cursorPos = Vector2 cursorPosX cursorPosY
        cursorSize = Vector2 _sCursorWidth lineHeight
        cursorPosX =
            TextView.drawText _sTextViewStyle (last beforeCursorLines) ^.
            TextView.renderedTextSize . Lens.to advance . _1
        cursorPosY = lineHeight * (genericLength beforeCursorLines - 1)

eventMap ::
    Int -> Text -> Text -> Widget.Id ->
    Widget.EventMap (Text, Widget.EventResult)
eventMap cursor str displayStr myId =
    mconcat . concat $ [
        [ keys (moveDoc ["left"]) [noMods GLFW.Key'Left] $
            moveRelative (-1)
        | cursor > 0 ],

        [ keys (moveDoc ["right"]) [noMods GLFW.Key'Right] $
            moveRelative 1
        | cursor < textLength ],

        [ keys (moveDoc ["word", "left"]) [ctrl GLFW.Key'Left]
            backMoveWord
        | cursor > 0 ],

        [ keys (moveDoc ["word", "right"]) [ctrl GLFW.Key'Right] moveWord
        | cursor < textLength ],

        [ keys (moveDoc ["up"]) [noMods GLFW.Key'Up] $
            moveRelative (- cursorX - 1 - Text.length (Text.drop cursorX prevLine))
        | cursorY > 0 ],

        [ keys (moveDoc ["down"]) [noMods GLFW.Key'Down] $
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

        [ keys (deleteDoc ["backwards"]) [noMods GLFW.Key'Backspace] $
            backDelete 1
        | cursor > 0 ],

        [ keys (deleteDoc ["word", "backwards"]) [ctrl GLFW.Key'W]
            backDeleteWord
        | cursor > 0 ],

        let swapPoint = min (textLength - 2) (cursor - 1)
            (beforeSwap, Text.unpack -> x:y:afterSwap) = Text.splitAt swapPoint str
            swapLetters =
                min textLength (cursor + 1)
                & eventRes (beforeSwap <> Text.pack (y:x:afterSwap))

        in

        [ keys (editDoc ["Swap letters"]) [ctrl GLFW.Key'T]
            swapLetters
        | cursor > 0 && textLength >= 2 ],

        [ keys (deleteDoc ["forward"]) [noMods GLFW.Key'Delete] $
            delete 1
        | cursor < textLength ],

        [ keys (deleteDoc ["word", "forward"]) [alt GLFW.Key'D]
            deleteWord
        | cursor < textLength ],

        [ keys (deleteDoc ["till", "end of line"]) [ctrl GLFW.Key'K] $
            delete (Text.length curLineAfter)
        | not . Text.null $ curLineAfter ],

        [ keys (deleteDoc ["newline"]) [ctrl GLFW.Key'K] $
            delete 1
        | Text.null curLineAfter && cursor < textLength ],

        [ keys (deleteDoc ["till", "beginning of line"]) [ctrl GLFW.Key'U] $
            backDelete (Text.length curLineBefore)
        | not . Text.null $ curLineBefore ],

        [ E.filterChars (`notElem` (" \n" :: String)) .
            E.allChars "Character" (insertDoc ["character"]) $
            insert . Text.singleton
        ],

        [ keys (insertDoc ["Newline"])
            [noMods GLFW.Key'Enter, ModKey.shift GLFW.Key'Enter] (insert "\n") ],

        [ keys (insertDoc ["Space"])
            [noMods GLFW.Key'Space, ModKey.shift GLFW.Key'Space] (insert " ") ],

        [ E.pasteOnKey (ctrl GLFW.Key'V) (E.Doc ["Clipboard", "Paste"]) insert ]

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
        ctrl = ModKey.ctrl
        alt = ModKey.alt
        homeKeys = [noMods GLFW.Key'Home, ctrl GLFW.Key'A]
        endKeys = [noMods GLFW.Key'End, ctrl GLFW.Key'E]
        textLength = Text.length str
        lineCount = length $ Text.splitOn "\n" displayStr
        (before, after) = Text.splitAt cursor str

make ::
    Style -> Text -> Widget.Id -> Widget.Env -> Widget (Text, Widget.EventResult)
make style str myId env =
    makeFunc style str myId
    where
        makeFunc =
            case Widget.subId myId (env ^. Widget.envCursor) of
            Nothing -> makeUnfocused
            Just suffix -> makeFocused (decodeCursor suffix)
        decodeCursor [x] = min (Text.length str) $ BinUtils.decodeS x
        decodeCursor _ = Text.length str
