{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Graphics.UI.Bottle.Widgets.TextEdit(
  Cursor, Style(..), make, defaultCursorColor, defaultCursorWidth,
  makeTextEditCursor,
  sCursorColor,
  sCursorWidth,
  sTextCursorId,
  sBackgroundCursorId,
  sEmptyUnfocusedString,
  sEmptyFocusedString,
  sTextViewStyle
  ) where

import           Control.Applicative ((<$>))
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import qualified Data.Binary.Utils as BinUtils
import qualified Data.ByteString.Char8 as SBS8
import           Data.Char (isSpace)
import           Data.List (genericLength, minimumBy)
import           Data.List.Split (splitWhen)
import           Data.List.Utils (enumerate)
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe)
import           Data.Monoid (Monoid(..))
import qualified Data.Monoid as Monoid
import           Data.Ord (comparing)
import qualified Data.Set as Set
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.DrawingCombinators.Utils (square, textHeight)
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
import qualified Safe

type Cursor = Int

data Style = Style
  { _sCursorColor :: Draw.Color
  , _sCursorWidth :: Widget.R
  , _sTextCursorId :: Anim.AnimId
  , _sBackgroundCursorId :: Anim.AnimId
  , _sBGColor :: Draw.Color
  , _sEmptyUnfocusedString :: String
  , _sEmptyFocusedString :: String
  , _sTextViewStyle :: TextView.Style
  }
Lens.makeLenses ''Style

defaultCursorColor :: Draw.Color
defaultCursorColor = Draw.Color 0 1 0 1

defaultCursorWidth :: Widget.R
defaultCursorWidth = 4

tillEndOfWord :: String -> String
tillEndOfWord xs = spaces ++ nonSpaces
  where
    spaces = takeWhile isSpace xs
    nonSpaces = takeWhile (not . isSpace) . dropWhile isSpace $ xs

makeDisplayStr :: String -> String -> String
makeDisplayStr empty ""  = empty
makeDisplayStr _     str = str

cursorTranslate :: Style -> Anim.Frame -> Anim.Frame
cursorTranslate style = Anim.translate $ Vector2 (style ^. sCursorWidth / 2) 0

makeTextEditCursor :: Widget.Id -> Int -> Widget.Id
makeTextEditCursor myId = Widget.joinId myId . (:[]) . BinUtils.encodeS

rightSideOfRect :: Rect -> Rect
rightSideOfRect rect =
  rect
  & Rect.left .~ rect ^. Rect.right
  & Rect.width .~ 0

cursorRects :: Style -> String -> [Rect]
cursorRects style str =
  concat .
  -- A bit ugly: letterRects returns rects for all but newlines, and
  -- returns a list of lines. Then addFirstCursor adds the left-most
  -- cursor of each line, thereby the number of rects becomes the
  -- original number of letters which can be used to match the
  -- original string index-wise.
  zipWith addFirstCursor (iterate (+lineHeight) 0) .
  (map . map) rightSideOfRect $
  TextView.letterRects (style ^. sTextViewStyle) str
  where
    addFirstCursor y = (Rect (Vector2 0 y) (Vector2 0 lineHeight) :)
    lineHeight = lineHeightOfStyle style

makeUnfocused :: Style -> String -> Widget.Id -> Widget ((,) String)
makeUnfocused style str myId =
  makeFocusable style str myId .
  (Widget.wSize . Lens._1 +~ cursorWidth) .
  (Widget.wFrame %~ cursorTranslate style) .
  TextView.makeWidget (style ^. sTextViewStyle) displayStr $
  Widget.toAnimId myId
  where
    cursorWidth = style ^. sCursorWidth
    displayStr = makeDisplayStr (style ^. sEmptyUnfocusedString) str

makeFocusable ::
  Style -> String -> Widget.Id ->
  Widget ((,) String) -> Widget ((,) String)
makeFocusable style str myId =
  Widget.wMaybeEnter .~ Just mEnter
  where
    minimumOn = minimumBy . comparing
    rectToCursor fromRect =
      fst . minimumOn snd . enumerate . map (Rect.distance fromRect) $
      cursorRects style str
    mEnter dir =
      Widget.EnterResult cursorRect .
      (,) str . Widget.eventResultFromCursor $
      makeTextEditCursor myId cursor
      where
        cursor =
          case dir of
          Direction.Outside -> length str
          Direction.PrevFocalArea rect -> rectToCursor rect
          Direction.Point x -> rectToCursor $ Rect x 0
        cursorRect = mkCursorRect style cursor str

lineHeightOfStyle :: Style -> Widget.R
lineHeightOfStyle style = sz * textHeight
  where
    sz = fromIntegral $ style ^. sTextViewStyle . TextView.styleFontSize

eventResult ::
  Widget.Id -> [(Maybe Int, Char)] -> [(Maybe Int, Char)] ->
  Int -> (String, Widget.EventResult)
eventResult myId strWithIds newText newCursor =
  (map snd newText,
    Widget.EventResult {
      Widget._eCursor = Monoid.Last . Just $ makeTextEditCursor myId newCursor,
      Widget._eAnimIdMapping = Monoid.Endo mapping
    })
  where
    myAnimId = Widget.toAnimId myId
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

-- TODO: Instead of font + ptSize, let's pass a text-drawer (that's
-- what "Font" should be)
-- | Note: maxLines prevents the *user* from exceeding it, not the
-- | given text...
makeFocused :: Cursor -> Style -> String -> Widget.Id -> Widget ((,) String)
makeFocused cursor style str myId =
  makeFocusable style str myId .
  Widget.backgroundColor 10 (style ^. sBackgroundCursorId) (style ^. sBGColor) $
  widget
  where
    widget = Widget
      { _wIsFocused = True
      , _wSize = reqSize
      , _wFrame = img `mappend` cursorFrame
      , _wEventMap = eventMap cursor str displayStr myId
      , _wMaybeEnter = Nothing
      , _wFocalArea = cursorRect
      }
    reqSize = Vector2 (style ^. sCursorWidth + tlWidth) tlHeight
    myAnimId = Widget.toAnimId myId
    img = cursorTranslate style $ frameGen myAnimId
    displayStr = makeDisplayStr (style ^. sEmptyFocusedString) str
    (frameGen, Vector2 tlWidth tlHeight) = textViewDraw style displayStr

    cursorRect = mkCursorRect style cursor str
    cursorFrame =
      Anim.onDepth (+2) .
      Anim.unitIntoRect cursorRect .
      Anim.simpleFrame (style ^. sTextCursorId) $
      Draw.tint (style ^. sCursorColor) square

textViewDraw ::
  Style -> String -> (Anim.AnimId -> Anim.Frame, Widget.Size)
textViewDraw = TextView.drawTextAsSingleLetters . (^. sTextViewStyle)

mkCursorRect :: Style -> Int -> String -> Rect
mkCursorRect style cursor str = Rect cursorPos cursorSize
  where
    beforeCursorLines = splitWhen (== '\n') $ take cursor str
    lineHeight = lineHeightOfStyle style
    cursorPos = Vector2 cursorPosX cursorPosY
    cursorSize = Vector2 (style ^. sCursorWidth) lineHeight
    cursorPosX =
      textViewDraw style (last beforeCursorLines) ^.
      Lens._2 . Lens._1
    cursorPosY = lineHeight * (genericLength beforeCursorLines - 1)

eventMap ::
  Int -> String -> String -> Widget.Id ->
  Widget.EventHandlers ((,) String)
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
      moveRelative (- cursorX - 1 - length (drop cursorX prevLine))
    | cursorY > 0 ],

    [ keys (moveDoc ["down"]) [noMods GLFW.Key'Down] $
      moveRelative (length curLineAfter + 1 + min cursorX (length nextLine))
    | cursorY < lineCount - 1 ],

    [ keys (moveDoc ["beginning of line"]) homeKeys $
      moveRelative (-cursorX)
    | cursorX > 0 ],

    [ keys (moveDoc ["end of line"]) endKeys $
      moveRelative (length curLineAfter)
    | not . null $ curLineAfter ],

    [ keys (moveDoc ["beginning of text"]) homeKeys $
      moveAbsolute 0
    | cursorX == 0 && cursor > 0 ],

    [ keys (moveDoc ["end of text"]) endKeys $
      moveAbsolute textLength
    | null curLineAfter && cursor < textLength ],

    [ keys (deleteDoc ["backwards"]) [noMods GLFW.Key'Backspace] $
      backDelete 1
    | cursor > 0 ],

    [ keys (deleteDoc ["word", "backwards"]) [ctrl GLFW.Key'W]
      backDeleteWord
    | cursor > 0 ],

    let swapPoint = min (textLength - 2) (cursor - 1)
        (beforeSwap, x:y:afterSwap) = splitAt swapPoint strWithIds
        swapLetters = eventRes (beforeSwap ++ y:x:afterSwap) $ min textLength (cursor + 1)

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
      delete (length curLineAfter)
    | not . null $ curLineAfter ],

    [ keys (deleteDoc ["newline"]) [ctrl GLFW.Key'K] $
      delete 1
    | null curLineAfter && cursor < textLength ],

    [ keys (deleteDoc ["till", "beginning of line"]) [ctrl GLFW.Key'U] $
      backDelete (length curLineBefore)
    | not . null $ curLineBefore ],

    [ E.filterChars (`notElem` " \n") .
      E.allChars "Character" (insertDoc ["character"]) $
      insert . (: [])
    ],

    [ keys (insertDoc ["Newline"]) [noMods GLFW.Key'Enter] (insert "\n") ],

    [ keys (insertDoc ["Space"]) [ModKey mempty GLFW.Key'Space] (insert " ") ]

    ]
  where
    editDoc = E.Doc . ("Edit" :)
    deleteDoc = editDoc . ("Delete" :)
    insertDoc = editDoc . ("Insert" :)
    moveDoc = E.Doc . ("Navigation" :) . ("Move" :)
    splitLines = splitWhen ((== '\n') . snd)
    linesBefore = reverse (splitLines before)
    linesAfter = splitLines after
    prevLine = linesBefore !! 1
    nextLine = linesAfter !! 1
    curLineBefore = head linesBefore
    curLineAfter = head linesAfter
    cursorX = length curLineBefore
    cursorY = length linesBefore - 1

    eventRes = eventResult myId strWithIds
    moveAbsolute a = eventRes strWithIds . max 0 $ min (length str) a
    moveRelative d = moveAbsolute (cursor + d)
    backDelete n = eventRes (take (cursor-n) strWithIds ++ drop cursor strWithIds) (cursor-n)
    delete n = eventRes (before ++ drop n after) cursor
    insert l = eventRes str' cursor'
      where
        cursor' = cursor + length l
        str' = concat [before, map ((,) Nothing) l, after]

    backDeleteWord = backDelete . length . tillEndOfWord . reverse $ map snd before
    deleteWord = delete . length . tillEndOfWord $ map snd after

    backMoveWord = moveRelative . negate . length . tillEndOfWord . reverse $ map snd before
    moveWord = moveRelative . length . tillEndOfWord $ map snd after

    keys = flip E.keyPresses

    noMods = ModKey mempty
    ctrl = ModKey.ctrl
    alt = ModKey.alt
    homeKeys = [noMods GLFW.Key'Home, ctrl GLFW.Key'A]
    endKeys = [noMods GLFW.Key'End, ctrl GLFW.Key'E]
    textLength = length str
    lineCount = length $ splitWhen (== '\n') displayStr
    strWithIds = Lens.mapped . _1 %~ Just $ enumerate str
    (before, after) = splitAt cursor strWithIds

make :: Style -> Widget.Id -> String -> Widget.Id -> Widget ((,) String)
make style cursor str myId =
  maybe makeUnfocused makeFocused mCursor style str myId
  where
    mCursor = extractTextEditCursor <$> Widget.subId myId cursor
    extractTextEditCursor [x] = min (length str) $ BinUtils.decodeS x
    extractTextEditCursor _ = length str
