{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Graphics.UI.Bottle.Widgets.TextEdit(
  Cursor, Style(..), make, defaultCursorColor, defaultCursorWidth,
  makeTextEditCursor,
  atSCursorColor,
  atSCursorWidth,
  atSTextCursorId,
  atSBackgroundCursorId,
  atSEmptyString,
  atSTextViewStyle) where

import Control.Arrow (first)
import Data.Char (isSpace)
import Data.List (genericLength, minimumBy)
import Data.List.Split (splitWhen)
import Data.List.Utils (enumerate)
import Data.Maybe (fromJust, mapMaybe)
import Data.Monoid (Monoid(..))
import Data.Ord (comparing)
import Data.Vector.Vector2 (Vector2(..))
import Graphics.DrawingCombinators.Utils (square, textHeight)
import Graphics.UI.Bottle.Rect (Rect(..))
import Graphics.UI.Bottle.Sized (Sized(..))
import Graphics.UI.Bottle.Widget (Widget(..))
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.Binary.Utils as BinUtils
import qualified Data.ByteString.Char8 as SBS8
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector.Vector2 as Vector2
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.Direction as Direction
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Rect as Rect
import qualified Graphics.UI.Bottle.SizeRange as SizeRange
import qualified Graphics.UI.Bottle.Sized as Sized
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import qualified Safe

type Cursor = Int

data Style = Style {
  sCursorColor :: Draw.Color,
  sCursorWidth :: Widget.R,
  sTextCursorId :: Widget.Id,
  sBackgroundCursorId :: Widget.Id,
  sEmptyString :: String,
  sTextViewStyle :: TextView.Style
  }
AtFieldTH.make ''Style

defaultCursorColor :: Draw.Color
defaultCursorColor = Draw.Color 0 1 0 1

defaultCursorWidth :: Widget.R
defaultCursorWidth = 8

tillEndOfWord :: String -> String
tillEndOfWord xs = spaces ++ nonSpaces
  where
    spaces = takeWhile isSpace xs
    nonSpaces = takeWhile (not . isSpace) . dropWhile isSpace $ xs

makeDisplayStr :: Style -> String -> String
makeDisplayStr style ""  = sEmptyString style
makeDisplayStr _     str = str

cursorTranslate :: Style -> Anim.Frame -> Anim.Frame
cursorTranslate style = Anim.translate (Vector2 (sCursorWidth style / 2) 0)

makeTextEditCursor :: Widget.Id -> Int -> Widget.Id
makeTextEditCursor myId = Widget.joinId myId . (:[]) . BinUtils.encodeS

zeroWideRectsBetween :: [Rect] -> [Rect]
zeroWideRectsBetween allRects@(rect:_) = zeroWideRectsBetween_
  (Rect.top rect) (Rect.height rect) (Rect.left rect) allRects
  where
    zeroWideRectsBetween_ y height startX [] = [Rect (Vector2 startX y)
                                                     (Vector2 0 height)]
    zeroWideRectsBetween_ y height startX
      (Rect _ (Vector2 width _): rects) =
        (Rect (Vector2 startX y) (Vector2 0 height):
          zeroWideRectsBetween_ y height (startX + width) rects)

makeUnfocused :: Style -> String -> Widget.Id -> Widget ((,) String)
makeUnfocused style str myId =
  Widget.takesFocus enter .
  (Widget.atContent . Sized.atRequestedSize)
   ((SizeRange.atSrMinSize . Vector2.first) (+ sCursorWidth style) .
    (SizeRange.atSrMaxSize . Vector2.first . fmap) (+ sCursorWidth style)) .
  Widget.atImage (cursorTranslate style) .
  TextView.makeWidget (sTextViewStyle style) str $
  Widget.toAnimId myId
  where
    enter dir =
      (,) str . makeTextEditCursor myId $
      Direction.fold (length str) rectToCursor dir
      where
        minimumOn = minimumBy . comparing
        rectToCursor fromRect =
          fst . minimumOn snd . enumerate . map (Rect.distance fromRect) .
          concat .
          (map zeroWideRectsBetween) $ TextView.letterRects (sTextViewStyle style) str

-- TODO: Instead of font + ptSize, let's pass a text-drawer (that's
-- what "Font" should be)
-- | Note: maxLines prevents the *user* from exceeding it, not the
-- | given text...
makeFocused :: Cursor -> Style -> String -> Widget.Id -> Widget ((,) String)
makeFocused cursor style str myId =
  Widget.backgroundColor (sBackgroundCursorId style) blue .
  Widget.atImage (`mappend` cursorFrame) .
  Widget.strongerEvents eventMap $
  widget
  where
    widget = Widget {
      isFocused = True,
      content =
        Sized reqSize . const $
        Widget.SizeDependentWidgetData {
          Widget.sdwdFrame = img,
          Widget.sdwdEventMap = mempty,
          Widget.sdwdMaybeEnter = Nothing,
          Widget.sdwdFocalArea = cursorRect
          }
      }
    reqSize = SizeRange.fixedSize $ Vector2 (sCursorWidth style + tlWidth) tlHeight
    myAnimId = Widget.toAnimId myId
    img = cursorTranslate style $ frameGen myAnimId
    drawText = TextView.drawTextAsSingleLetters (sTextViewStyle style)
    (frameGen, Vector2 tlWidth tlHeight) = drawText displayStr

    blue = Draw.Color 0 0 0.8 0.8

    textLinesWidth = Vector2.fst . snd . drawText
    sz = fromIntegral . TextView.styleFontSize $ sTextViewStyle style
    lineHeight = sz * textHeight
    strWithIds = map (first Just) $ enumerate str
    beforeCursor = take cursor strWithIds
    cursorRect = Rect cursorPos cursorSize
    cursorPos = Vector2 cursorPosX cursorPosY
    cursorSize = Vector2 (sCursorWidth style) lineHeight
    cursorPosX = textLinesWidth . map snd . last $ splitLines beforeCursor
    cursorPosY = (lineHeight *) . subtract 1 . genericLength . splitLines $ beforeCursor
    cursorFrame =
      Anim.onDepth (+2) .
      Anim.translate cursorPos .
      Anim.scale cursorSize .
      (Anim.simpleFrame . Widget.toAnimId . sTextCursorId) style $
      Draw.tint (sCursorColor style) square

    (before, after) = splitAt cursor strWithIds
    textLength = length str
    lineCount = length $ splitWhen (== '\n') displayStr
    displayStr = makeDisplayStr style str

    splitLines = splitWhen ((== '\n') . snd)
    linesBefore = reverse (splitLines before)
    linesAfter = splitLines after
    prevLine = linesBefore !! 1
    nextLine = linesAfter !! 1
    curLineBefore = head linesBefore
    curLineAfter = head linesAfter
    cursorX = length curLineBefore
    cursorY = length linesBefore - 1

    eventResult newText newCursor =
      (map snd newText,
        Widget.EventResult {
          Widget.eCursor = Just $ makeTextEditCursor myId newCursor,
          Widget.eAnimIdMapping = mapping
        })
      where
        mapping animId = maybe animId (Anim.joinId myAnimId . translateId) $ Anim.subId myAnimId animId
        translateId [subId] = (:[]) . maybe subId (SBS8.pack . show) $ (`Map.lookup` dict) =<< Safe.readMay (SBS8.unpack subId)
        translateId x = x
        dict = mappend movedDict deletedDict
        movedDict = Map.fromList . mapMaybe posMapping . enumerate $ map fst newText
        deletedDict = Map.fromList . map (flip (,) (-1)) $ Set.toList deletedKeys
        posMapping (_, Nothing) = Nothing
        posMapping (newPos, Just oldPos) = Just (oldPos, newPos)
        deletedKeys =
          Set.fromList (mapMaybe fst strWithIds) `Set.difference`
          Set.fromList (mapMaybe fst newText)

    moveAbsolute a = eventResult strWithIds . max 0 $ min (length str) a
    moveRelative d = moveAbsolute (cursor + d)
    backDelete n = eventResult (take (cursor-n) strWithIds ++ drop cursor strWithIds) (cursor-n)
    delete n = eventResult (before ++ drop n after) cursor
    insert l = eventResult str' cursor'
      where
        cursor' = cursor + length l
        str' = concat [before, map ((,) Nothing) l, after]

    backDeleteWord = backDelete . length . tillEndOfWord . reverse $ map snd before
    deleteWord = delete . length . tillEndOfWord $ map snd after

    backMoveWord = moveRelative . negate . length . tillEndOfWord . reverse $ map snd before
    moveWord = moveRelative . length . tillEndOfWord $ map snd after

    keys doc = mconcat . map (`E.fromEventType` doc)

    specialKey = E.KeyEventType E.noMods
    ctrlSpecialKey = E.KeyEventType E.ctrl
    ctrlCharKey = E.KeyEventType E.ctrl . E.charKey
    altCharKey = E.KeyEventType E.alt . E.charKey
    homeKeys = [specialKey E.KeyHome, ctrlCharKey 'A']
    endKeys = [specialKey E.KeyEnd, ctrlCharKey 'E']

    eventMap =
      mconcat . concat $ [
        [ keys "Move left" [specialKey E.KeyLeft] $
          moveRelative (-1)
        | cursor > 0 ],

        [ keys "Move right" [specialKey E.KeyRight] $
          moveRelative 1
        | cursor < textLength ],

        [ keys "Move word left" [ctrlSpecialKey E.KeyLeft]
          backMoveWord
        | cursor > 0 ],

        [ keys "Move word right" [ctrlSpecialKey E.KeyRight] moveWord
        | cursor < textLength ],

        [ keys "Move up" [specialKey E.KeyUp] $
          moveRelative (- cursorX - 1 - length (drop cursorX prevLine))
        | cursorY > 0 ],

        [ keys "Move down" [specialKey E.KeyDown] $
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

        [ keys "Delete backwards" [specialKey E.KeyBackspace] $
          backDelete 1
        | cursor > 0 ],

        [ keys "Delete word backwards" [ctrlCharKey 'w']
          backDeleteWord
        | cursor > 0 ],

        let swapPoint = min (textLength - 2) (cursor - 1)
            (beforeSwap, x:y:afterSwap) = splitAt swapPoint strWithIds
            swapLetters = eventResult (beforeSwap ++ y:x:afterSwap) $ min textLength (cursor + 1)

        in

        [ keys "Swap letters" [ctrlCharKey 't']
          swapLetters
        | cursor > 0 && textLength >= 2 ],

        [ keys "Delete forward" [specialKey E.KeyDel] $
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

        [ E.singleton E.CharEventType "Insert character" (insert . (: []) . fromJust . E.keyEventChar) ],

        [ keys "Insert Newline" [specialKey E.KeyEnter] (insert "\n") ],

        [ keys "Insert Space" [E.KeyEventType E.noMods E.KeySpace] (insert " ") ]

        ]

make :: Style -> Widget.Id -> String -> Widget.Id -> Widget ((,) String)
make style cursor str myId =
  maybe makeUnfocused makeFocused mCursor style str myId
  where
    mCursor = fmap extractTextEditCursor $ Widget.subId myId cursor
    extractTextEditCursor [x] = min (length str) $ BinUtils.decodeS x
    extractTextEditCursor _ = length str
