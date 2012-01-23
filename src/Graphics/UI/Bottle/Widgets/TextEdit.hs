{-# OPTIONS -Wall #-}
{-# LANGUAGE TypeOperators, OverloadedStrings #-}
module Graphics.UI.Bottle.Widgets.TextEdit(Cursor, Style(..), make, defaultCursorColor, defaultCursorWidth) where

import Data.Char (isSpace)
import Data.List (genericLength)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Monoid (Monoid(..))
import Data.Vector.Vector2 (Vector2(..))
import Graphics.DrawingCombinators.Utils (square, textHeight)
import Graphics.UI.Bottle.SizeRange (fixedSize)
import Graphics.UI.Bottle.Sized (Sized(..))
import Graphics.UI.Bottle.Widget (Widget(..))
import Graphics.UI.GLFW (Key(KeyBackspace, KeyDel, KeyDown, KeyEnd, KeyEnter, KeyHome, KeyLeft, KeyRight, KeyUp))
import qualified Data.Binary.Utils as BinUtils
import qualified Data.Vector.Vector2 as Vector2
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView

type Cursor = Int

data Style = Style {
  sCursorColor :: Draw.Color,
  sCursorWidth :: Draw.R,
  sTextCursorId :: Anim.AnimId,
  sBackgroundCursorId :: Anim.AnimId,
  sTextViewStyle :: TextView.Style
  }

defaultCursorColor :: Draw.Color
defaultCursorColor = Draw.Color 0 1 0 1

defaultCursorWidth :: Draw.R
defaultCursorWidth = 8

splitLines :: String -> [String]
splitLines = splitOn "\n"

tillEndOfWord :: String -> String
tillEndOfWord xs = spaces ++ nonSpaces
  where
    spaces = takeWhile isSpace xs
    nonSpaces = takeWhile (not . isSpace) . dropWhile isSpace $ xs

makeDisplayStr :: String -> String -> String
makeDisplayStr emptyStr ""  = emptyStr
makeDisplayStr _        str = str

cursorTranslate :: Style -> Anim.Frame -> Anim.Frame
cursorTranslate style = Anim.translate (Vector2 (sCursorWidth style / 2) 0)

makeTextEditCursor :: Anim.AnimId -> Int -> Anim.AnimId
makeTextEditCursor myId = Anim.joinId myId . (:[]) . BinUtils.encodeS

makeUnfocused :: Style -> String -> Anim.AnimId -> Widget ((,) String)
makeUnfocused style str myId =
  Widget.takesFocus enter .
  Widget.atImage
    (cursorTranslate style) $
  TextView.makeWidget (sTextViewStyle style) (lines str) myId
  where
    enter dir = (str, makeTextEditCursor myId (enterPos dir))
    enterPos (Just (Vector2 x _))
      | x < 0 = 0
      | otherwise = length str
    enterPos Nothing = length str

-- TODO: Instead of font + ptSize, let's pass a text-drawer (that's
-- what "Font" should be)
-- | Note: maxLines prevents the *user* from exceeding it, not the
-- | given text...
makeFocused :: Style -> String -> Cursor -> String -> Anim.AnimId -> Widget ((,) String)
makeFocused style emptyStr cursor str myId =
  Widget.atImageWithSize
    (Anim.backgroundColor (sBackgroundCursorId style) 10 blue) .
  Widget.atImage (`mappend` cursorFrame) .
  Widget.strongerKeys eventMap $
  widget
  where
    widget = Widget {
      isFocused = True,
      content =
        Sized reqSize . const $
        Widget.UserIO {
          Widget.uioFrame = img,
          Widget.uioEventMap = mempty,
          Widget.uioMaybeEnter = Nothing
          }
      }
    reqSize = fixedSize $ Vector2 (sCursorWidth style + tlWidth) tlHeight
    img = cursorTranslate style $ frameGen myId
    (frameGen, Vector2 tlWidth tlHeight) = TextView.drawText True (sTextViewStyle style) textLines

    blue = Draw.Color 0 0 0.8 0.8

    textLinesWidth = Vector2.fst . snd . TextView.drawText True (sTextViewStyle style)
    sz = fromIntegral . TextView.styleFontSize $ sTextViewStyle style
    lineHeight = sz * textHeight
    beforeCursor = take cursor str
    cursorPosX = textLinesWidth . (: []) . last . splitLines $ beforeCursor
    cursorPosY = (lineHeight *) . subtract 1 . genericLength . splitLines $ beforeCursor
    cursorFrame =
      Anim.onDepth (+2) .
      Anim.translate (Vector2 cursorPosX cursorPosY) .
      Anim.scale (Vector2 (sCursorWidth style) lineHeight) .
      Anim.simpleFrame (sTextCursorId style) $
      Draw.tint (sCursorColor style) square

    (before, after) = splitAt cursor str
    textLength = length str
    lineCount = length textLines
    textLines = splitLines displayStr
    displayStr = makeDisplayStr emptyStr str

    linesBefore = reverse (splitLines before)
    linesAfter = splitLines after
    prevLine = linesBefore !! 1
    nextLine = linesAfter !! 1
    curLineBefore = head linesBefore
    curLineAfter = head linesAfter
    cursorX = length curLineBefore
    cursorY = length linesBefore - 1

    eventResult newStr newCursor = (newStr, makeTextEditCursor myId newCursor)
    moveAbsolute a = eventResult str . max 0 $ min (length str) a
    moveRelative d = moveAbsolute (cursor + d)
    backDelete n = eventResult (take (cursor-n) str ++ drop cursor str) (cursor-n)
    delete n = eventResult (before ++ drop n after) cursor
    insert l = eventResult str' cursor'
      where
        cursor' = cursor + length l
        str' = concat [before, l, after]

    backDeleteWord = backDelete . length . tillEndOfWord . reverse $ before
    deleteWord = delete . length . tillEndOfWord $ after

    backMoveWord = moveRelative . negate . length . tillEndOfWord . reverse $ before
    moveWord = moveRelative . length . tillEndOfWord $ after

    singleton doc eventType mkModel =
      const (E.singleton eventType mkModel) (doc :: String)

    keys doc = const (mconcat . map E.fromEventType) (doc :: String)

    specialKey = E.KeyEventType E.noMods
    ctrlSpecialKey = E.KeyEventType E.ctrl
    ctrlCharKey = E.KeyEventType E.ctrl . E.charKey
    altCharKey = E.KeyEventType E.alt . E.charKey
    homeKeys = [specialKey KeyHome, ctrlCharKey 'A']
    endKeys = [specialKey KeyEnd, ctrlCharKey 'E']

    eventMap =
      mconcat . concat $ [
        [ keys "Move left" [specialKey KeyLeft] $
          moveRelative (-1)
        | cursor > 0 ],

        [ keys "Move right" [specialKey KeyRight] $
          moveRelative 1
        | cursor < textLength ],

        [ keys "Move word left" [ctrlSpecialKey KeyLeft]
          backMoveWord
        | cursor > 0 ],

        [ keys "Move word right" [ctrlSpecialKey KeyRight] moveWord
        | cursor < textLength ],

        [ keys "Move up" [specialKey KeyUp] $
          moveRelative (- cursorX - 1 - length (drop cursorX prevLine))
        | cursorY > 0 ],

        [ keys "Move down" [specialKey KeyDown] $
          moveRelative (length curLineAfter + 1 + min cursorX (length nextLine))
        | cursorY < lineCount - 1 ],

        [ keys "Move to beginning of line" homeKeys $
          moveRelative (-cursorX)
        | cursorX > 0 ],

        [ keys "Move to end of line" endKeys $
          moveRelative (length curLineAfter)
        | not . null $ curLineAfter ],

        [ keys "Move to beginning of text" homeKeys $
          moveAbsolute 0
        | cursorX == 0 && cursor > 0 ],

        [ keys "Move to end of text" endKeys $
          moveAbsolute textLength
        | null curLineAfter && cursor < textLength ],

        [ keys "Delete backwards" [specialKey KeyBackspace] $
          backDelete 1
        | cursor > 0 ],

        [ keys "Delete word backwards" [ctrlCharKey 'w']
          backDeleteWord
        | cursor > 0 ],

        let swapPoint = min (textLength - 2) (cursor - 1)
            (beforeSwap, x:y:afterSwap) = splitAt swapPoint str
            swapLetters = eventResult (beforeSwap ++ y:x:afterSwap) $ min textLength (cursor + 1)

        in

        [ keys "Swap letters" [ctrlCharKey 't']
          swapLetters
        | cursor > 0 && textLength >= 2 ],

        [ keys "Delete forward" [specialKey KeyDel] $
          delete 1
        | cursor < textLength ],

        [ keys "Delete word forward" [altCharKey 'd']
          deleteWord
        | cursor < textLength ],

        [ keys "Delete rest of line" [ctrlCharKey 'k'] $
          delete (length curLineAfter)
        | not . null $ curLineAfter ],

        [ keys "Delete newline" [ctrlCharKey 'k'] $
          delete 1
        | null curLineAfter && cursor < textLength ],

        [ keys "Delete till beginning of line" [ctrlCharKey 'u'] $
          backDelete (length curLineBefore)
        | not . null $ curLineBefore ],

        [ singleton "Insert character" E.CharEventType (insert . (: []) . fromJust . E.keyEventChar) ],

        [ keys "Insert Newline" [specialKey KeyEnter] (insert "\n") ]

        ]

make :: Style -> String -> Anim.AnimId -> String -> Anim.AnimId -> Widget ((,) String)
make style emptyStr cursor str myId =
  maybe (makeUnfocused style) (makeFocused style emptyStr) mCursor str myId
  where
    mCursor = fmap extractTextEditCursor $ Anim.subId myId cursor
    extractTextEditCursor [x] = BinUtils.decodeS x
    extractTextEditCursor _ = length str
