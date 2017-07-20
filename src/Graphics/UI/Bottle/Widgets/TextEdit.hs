{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TemplateHaskell, ViewPatterns, LambdaCase, NamedFieldPuns #-}
module Graphics.UI.Bottle.Widgets.TextEdit
    ( Style(..), sCursorColor, sCursorWidth, sTextViewStyle
    , HasStyle(..)
    , EmptyStrings(..), emptyFocusedString, emptyUnfocusedString
    , make
    , defaultStyle
    , getCursor
    ) where

import qualified Control.Lens as Lens
import qualified Data.Binary.Utils as BinUtils
import           Data.Char (isSpace)
import           Data.List (genericLength)
import           Data.List.Utils (minimumOn)
import qualified Data.Text as Text
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.DrawingCombinators.Utils (TextSize(..))
import           Graphics.UI.Bottle.Align (WithTextPos(..))
import qualified Graphics.UI.Bottle.Align as Align
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.Direction as Direction
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.MetaKey as MetaKey
import           Graphics.UI.Bottle.ModKey (ModKey(..))
import qualified Graphics.UI.Bottle.ModKey as ModKey
import           Graphics.UI.Bottle.Rect (Rect(..))
import qualified Graphics.UI.Bottle.Rect as Rect
import qualified Graphics.UI.Bottle.View as View
import           Graphics.UI.Bottle.Widget (Widget(..))
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import qualified Graphics.UI.GLFW as GLFW

import           Lamdu.Prelude

type Cursor = Int

data Style = Style
    { _sCursorColor :: Draw.Color
    , _sCursorWidth :: Widget.R
    , _sTextViewStyle :: TextView.Style
    }
Lens.makeLenses ''Style

class HasStyle env where style :: Lens' env Style
instance HasStyle Style where style = id

data EmptyStrings = EmptyStrings
    { _emptyUnfocusedString :: Text
    , _emptyFocusedString :: Text
    }
Lens.makeLenses ''EmptyStrings

instance TextView.HasStyle Style where style = sTextViewStyle

defaultStyle :: TextView.Style -> Style
defaultStyle tvStyle =
    Style
    { _sCursorColor = Draw.Color 0 1 0 1
    , _sCursorWidth = 4
    , _sTextViewStyle = tvStyle
    }

tillEndOfWord :: Text -> Text
tillEndOfWord xs = spaces <> nonSpaces
    where
        spaces = Text.takeWhile isSpace xs
        nonSpaces = Text.dropWhile isSpace xs & Text.takeWhile (not . isSpace)

makeDisplayStr :: Text -> Text -> Text
makeDisplayStr empty ""  = empty
makeDisplayStr _     str = Text.take 5000 str

encodeCursor :: Widget.Id -> Cursor -> Widget.Id
encodeCursor myId = Widget.joinId myId . (:[]) . BinUtils.encodeS

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
    WithTextPos (Widget (Text, Widget.EventResult))
makeInternal s str displayStr myId =
    TextView.make (s ^. sTextViewStyle) displayStr animId
    & View.pad (Vector2 (s ^. sCursorWidth / 2) 0)
    & Align.tValue %~ Widget.fromView
    & Align.tValue %~ Widget.mEnter ?~ enterFromDirection s str myId
    where
        animId = Widget.toAnimId myId

makeUnfocused :: EmptyStrings -> Style -> Text -> Widget.Id -> WithTextPos (Widget (Text, Widget.EventResult))
makeUnfocused empty s str =
    makeInternal s str displayStr
    where
        displayStr = makeDisplayStr (empty ^. emptyUnfocusedString) str

minimumIndex :: Ord a => [a] -> Int
minimumIndex xs =
    xs ^@.. Lens.traversed & minimumOn snd & fst

cursorNearRect :: TextView.Style -> Text -> Rect -> Cursor
cursorNearRect s str fromRect =
    cursorRects s str <&> Rect.distance fromRect
    & minimumIndex -- cursorRects(TextView.letterRects) should never return an empty list

enterFromDirection ::
    Style -> Text -> Widget.Id ->
    Direction.Direction -> Widget.EnterResult (Text, Widget.EventResult)
enterFromDirection s str myId dir =
    Widget.EnterResult cursorRect 0 .
    (,) str . Widget.eventResultFromCursor $
    encodeCursor myId cursor
    where
        cursor =
            case dir of
            Direction.Outside -> Text.length str
            Direction.PrevFocalArea rect -> cursorNearRect (s ^. sTextViewStyle) str rect
            Direction.Point x -> cursorNearRect (s ^. sTextViewStyle) str $ Rect x 0
        cursorRect = mkCursorRect s cursor str

eventResult :: Widget.Id -> Text -> Cursor -> (Text, Widget.EventResult)
eventResult myId newText newCursor =
    ( newText
    , encodeCursor myId newCursor & Widget.eventResultFromCursor
    )

-- | Note: maxLines prevents the *user* from exceeding it, not the
-- | given text...
makeFocused ::
    Cursor -> EmptyStrings -> Style -> Text -> Widget.Id ->
    WithTextPos (Widget (Text, Widget.EventResult))
makeFocused cursor empty s str myId =
    makeInternal s str displayStr myId
    & View.bottomFrame <>~ cursorFrame
    & Align.tValue %~ Widget.setFocusedWith cursorRect (eventMap cursor str myId)
    where
        displayStr = makeDisplayStr (empty ^. emptyFocusedString) str
        cursorRect = mkCursorRect s cursor str
        cursorFrame =
            Anim.unitSquare (Widget.cursorAnimId ++ ["text"])
            & Anim.unitImages %~ Draw.tint (s ^. sCursorColor)
            & Anim.unitIntoRect cursorRect

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
            TextView.renderedTextSize . Lens.to advance . _1
        cursorPosY = lineHeight * (genericLength beforeCursorLines - 1)

-- TODO: Implement intra-TextEdit virtual cursor
eventMap ::
    Cursor -> Text -> Widget.Id -> Widget.VirtualCursor ->
    Widget.EventMap (Text, Widget.EventResult)
eventMap cursor str myId _virtualCursor =
    mconcat . concat $ [
        [ E.keyPressOrRepeat (noMods GLFW.Key'Left) (moveDoc ["left"]) $
            moveRelative (-1)
        | cursor > 0 ],

        [ E.keyPressOrRepeat (noMods GLFW.Key'Right) (moveDoc ["right"]) $
            moveRelative 1
        | cursor < textLength ],

        [ keys (moveDoc ["word", "left"]) [ctrl GLFW.Key'Left]
            backMoveWord
        | cursor > 0 ],

        [ keys (moveDoc ["word", "right"]) [ctrl GLFW.Key'Right] moveWord
        | cursor < textLength ],

        [ E.keyPressOrRepeat (noMods GLFW.Key'Up) (moveDoc ["up"]) $
            moveRelative (- cursorX - 1 - Text.length (Text.drop cursorX prevLine))
        | cursorY > 0 ],

        [ E.keyPressOrRepeat (noMods GLFW.Key'Down) (moveDoc ["down"]) $
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

        [ E.pasteOnKey (cmd GLFW.Key'V) (E.Doc ["Clipboard", "Paste"]) insert ]

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
        homeKeys = [noMods GLFW.Key'Home, ctrl GLFW.Key'A]
        endKeys = [noMods GLFW.Key'End, ctrl GLFW.Key'E]
        textLength = Text.length str
        lineCount = length $ Text.splitOn "\n" str
        (before, after) = Text.splitAt cursor str

getCursor ::
    (MonadReader env m, Widget.HasCursor env) =>
    m (Text -> Widget.Id -> Maybe Int)
getCursor =
    Widget.subId <&> f
    where
        f sub str myId =
            sub myId <&> decodeCursor
            where
                decodeCursor [x] = min (Text.length str) $ BinUtils.decodeS x
                decodeCursor _ = Text.length str

make ::
    (MonadReader env m, Widget.HasCursor env, HasStyle env) =>
    m ( EmptyStrings -> Text -> Widget.Id ->
        WithTextPos (Widget (Text, Widget.EventResult))
      )
make =
    do
        get <- getCursor
        s <- Lens.view style
        pure $ \empty str myId ->
            case get str myId of
            Nothing -> makeUnfocused empty s str myId
            Just pos -> makeFocused pos empty s str myId
