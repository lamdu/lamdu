{-# OPTIONS -Wall #-}
module TextEdit(Cursor, Model(..), make) where

import Data.Char
import Data.List.Split(splitOn)
import Graphics.DrawingCombinators((%%))
import qualified EventMap
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.DrawingCombinators.Affine as Affine
import qualified Codec.Binary.UTF8.String as UTF8
import Control.Monad
import Data.Monoid
import Graphics.UI.GLFW
import Data.Vector.Vector2 (Vector2(..))
import SizeRange (fixedSize)
import Sized (Sized(..))
import Widget (Widget(..))

type Cursor = Int

data Model = Model {
  textEditCursor :: Cursor,
  textEditText :: String
  }
  deriving (Show, Read, Eq, Ord)

splitLines :: String -> [String]
splitLines = splitOn "\n"

tillEndOfWord :: String -> String
tillEndOfWord xs = spaces ++ nonSpaces
  where
    spaces = takeWhile isSpace xs
    nonSpaces = takeWhile (not . isSpace) . dropWhile isSpace $ xs

square :: Draw.Image Any
square = Draw.convexPoly [ (-1, -1), (1, -1), (1, 1), (-1, 1) ]

-- | Note: maxLines prevents the *user* from exceeding it, not the
-- | given text...
make :: Draw.Font -> String -> Int -> Model -> Widget Model
make font emptyString maxLines (Model cursor str) =
  Widget . Sized reqSize $ const (img, Just keymap)
  where
    t = finalText str
    finalText "" = emptyString
    finalText x  = x

    reqSize = fixedSize $ Vector2 (textWidth t) 2
    img =
      void $
        mconcat [
          Draw.translate (0, 1) %% Draw.scale 1 (-1) %%
          Draw.text font (UTF8.encodeString t),
          cursorImage
        ]

    textWidth = Draw.textWidth font . UTF8.encodeString

    cursorPos = textWidth $ take cursor t
    cursorImage =
      Draw.tint (Draw.Color 0 1 0 1) $
      Affine.translate (cursorPos, 0.5) %%
      Draw.scale 0.1 1 %%
      square

    (before, after) = splitAt cursor str
    textLength = length str
    textLines = splitLines str
    --width = maximum . map length $ textLines
    height = length textLines

    linesBefore = reverse (splitLines before)
    linesAfter = splitLines after
    prevLine = linesBefore !! 1
    nextLine = linesAfter !! 1
    curLineBefore = head linesBefore
    curLineAfter = head linesAfter
    cursorX = length curLineBefore
    cursorY = length linesBefore - 1

    moveAbsolute a = (max 0 . min (length str) $ a, str)
    moveRelative d = moveAbsolute (cursor + d)
    backDelete n = (cursor-n, take (cursor-n) str ++ drop cursor str)
    delete n = (cursor, before ++ drop n after)

    backDeleteWord = backDelete . length . tillEndOfWord . reverse $ before
    deleteWord = delete . length . tillEndOfWord $ after

    backMoveWord = moveRelative . negate . length . tillEndOfWord . reverse $ before
    moveWord = moveRelative . length . tillEndOfWord $ after

    singleton _doc eventType makeModel =
      EventMap.singleton eventType $
      uncurry Model . makeModel

    keys doc = mconcat . map (\event -> singleton doc event . const)

    specialKey = EventMap.KeyEventType EventMap.noMods
    ctrlSpecialKey = EventMap.KeyEventType EventMap.ctrl
    ctrlCharKey = EventMap.KeyEventType EventMap.ctrl . CharKey . toUpper
    altCharKey = EventMap.KeyEventType EventMap.alt . CharKey . toUpper
    homeKeys = [specialKey KeyHome, ctrlCharKey 'A']
    endKeys = [specialKey KeyEnd, ctrlCharKey 'E']

    keymap =
      mconcat . concat $ [
        [ keys "Move left" [specialKey KeyLeft] $
          moveRelative (-1)
        | cursor > 0 ],

        [ keys "Move right" [specialKey KeyRight] $
          moveRelative 1
        | cursor < textLength ],

        [ keys "Move word left" [ctrlSpecialKey KeyLeft] $
          backMoveWord
        | cursor > 0 ],

        [ keys "Move word right" [ctrlSpecialKey KeyRight] moveWord
        | cursor < textLength ],

        [ keys "Move up" [specialKey KeyUp] $
          moveRelative (- cursorX - 1 - length (drop cursorX prevLine))
        | cursorY > 0 ],

        [ keys "Move down" [specialKey KeyDown] $
          moveRelative (length curLineAfter + 1 + min cursorX (length nextLine))
        | cursorY < height-1 ],

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
            swapLetters = (min textLength (cursor + 1),
                           beforeSwap ++ y:x:afterSwap)
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

        [ singleton "Insert character" EventMap.CharEventType (insert . return . EventMap.fromCharEvent) ],

        [ keys "Insert Newline" [specialKey KeyEnter] (insert "\n") ]

        ]

    insert :: String -> (Cursor, String)
    insert l = if (length . splitLines) str' <= max height maxLines
               then (cursor', str')
               else (cursor, str)
      where
        cursor' = cursor + length l
        str' = concat [before, l, after]
