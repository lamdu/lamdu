{-# LANGUAGE RecordWildCards, OverloadedStrings, TemplateHaskell #-}
module Graphics.UI.Bottle.Widgets.TextEdit
    ( Cursor
    , Style(..)
        , sCursorColor, sCursorWidth, sTextCursorId, sBGColor, sEmptyUnfocusedString
        , sEmptyFocusedString, sTextViewStyle
    , make
    , defaultCursorColor
    , defaultCursorWidth
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import qualified Data.Binary.Utils as BinUtils
import qualified Data.ByteString.Char8 as SBS8
import           Data.Char (isSpace)
import           Data.List (genericLength)
import           Data.List.Split (splitWhen)
import           Data.List.Utils (minimumOn)
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe)
import           Data.Monoid (Monoid(..), (<>))
import qualified Data.Monoid as Monoid
import qualified Data.Set as Set
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.Direction as Direction
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.ModKey (ModKey(..))
import qualified Graphics.UI.Bottle.ModKey as ModKey
import           Graphics.UI.Bottle.Rect (Rect(..))
import qualified Graphics.UI.Bottle.Rect as Rect
import           Graphics.UI.Bottle.View (View(..))
import           Graphics.UI.Bottle.Widget (Widget(..))
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import qualified Graphics.UI.GLFW as GLFW

type Cursor = Int

data Style = Style
    { _sCursorColor :: Draw.Color
    , _sCursorWidth :: Widget.R
    , _sTextCursorId :: Anim.AnimId
    , _sBGColor :: Draw.Color
    , _sEmptyUnfocusedString :: String
    , _sEmptyFocusedString :: String
    , _sTextViewStyle :: TextView.Style
    }
Lens.makeLenses ''Style

-- TODO: Replace with a defaultStyle :: TextViewStyle -> .. -> Style
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
cursorTranslate Style{..} = Anim.translate $ Vector2 (_sCursorWidth / 2) 0

encodeCursor :: Widget.Id -> Int -> Widget.Id
encodeCursor myId = Widget.joinId myId . (:[]) . BinUtils.encodeS

rightSideOfRect :: Rect -> Rect
rightSideOfRect rect =
    rect
    & Rect.left .~ rect ^. Rect.right
    & Rect.width .~ 0

cursorRects :: TextView.Style -> String -> [Rect]
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

makeUnfocused :: Style -> String -> Widget.Id -> Widget ((,) String)
makeUnfocused Style{..} str myId =
    TextView.makeWidget _sTextViewStyle displayStr animId
    & Widget.animFrame %~ cursorTranslate Style{..}
    & Widget.width +~ cursorWidth
    & makeFocusable Style{..} str myId
    where
        animId = Widget.toAnimId myId
        cursorWidth = _sCursorWidth
        displayStr = makeDisplayStr _sEmptyUnfocusedString str

minimumIndex :: Ord a => [a] -> Int
minimumIndex xs =
    xs ^@.. Lens.traversed & minimumOn snd & fst

cursorNearRect :: TextView.Style -> String -> Rect -> Int
cursorNearRect style str fromRect =
    cursorRects style str <&> Rect.distance fromRect
    & minimumIndex -- cursorRects(TextView.letterRects) should never return an empty list

enterFromDirection ::
    Style -> String -> Widget.Id ->
    Direction.Direction -> Widget.EnterResult ((,) String)
enterFromDirection Style{..} str myId dir =
    Widget.EnterResult cursorRect .
    (,) str . Widget.eventResultFromCursor $
    encodeCursor myId cursor
    where
        cursor =
            case dir of
            Direction.Outside -> length str
            Direction.PrevFocalArea rect -> cursorNearRect _sTextViewStyle str rect
            Direction.Point x -> cursorNearRect _sTextViewStyle str $ Rect x 0
        cursorRect = mkCursorRect Style{..} cursor str

makeFocusable ::
    Style -> String -> Widget.Id ->
    Widget ((,) String) -> Widget ((,) String)
makeFocusable style str myId =
    Widget.mEnter .~ Just (enterFromDirection style str myId)

readM :: Read a => String -> Maybe a
readM str =
    case reads str of
    [(x, "")] -> Just x
    _ -> Nothing

eventResult ::
    Widget.Id -> [(Maybe Int, Char)] -> [(Maybe Int, Char)] ->
    Int -> (String, Widget.EventResult)
eventResult myId strWithIds newText newCursor =
    (map snd newText,
        Widget.EventResult {
            Widget._eCursor = Monoid.Last . Just $ encodeCursor myId newCursor,
            Widget._eAnimIdMapping = Monoid.Endo mapping
        })
    where
        myAnimId = Widget.toAnimId myId
        mapping animId = maybe animId (Anim.joinId myAnimId . translateId) $ Anim.subId myAnimId animId
        translateId [subId] = (:[]) . maybe subId (SBS8.pack . show) $ (`Map.lookup` dict) =<< readM (SBS8.unpack subId)
        translateId x = x
        dict = mappend movedDict deletedDict
        movedDict =
            map fst newText ^@.. Lens.traversed
            <&> posMapping
            & (^.. Lens.traversed . Lens._Just)
            & Map.fromList
        deletedDict = Map.fromList . map (flip (,) (-1)) $ Set.toList deletedKeys
        posMapping (_, Nothing) = Nothing
        posMapping (newPos, Just oldPos) = Just (oldPos, newPos)
        deletedKeys =
            Set.fromList (mapMaybe fst strWithIds) `Set.difference`
            Set.fromList (mapMaybe fst newText)

-- | Note: maxLines prevents the *user* from exceeding it, not the
-- | given text...
makeFocused :: AnimId -> Cursor -> Style -> String -> Widget.Id -> Widget ((,) String)
makeFocused cursorBGAnimId cursor Style{..} str myId =
    widget
    & Widget.backgroundColor 10 cursorBGAnimId _sBGColor
    & makeFocusable Style{..} str myId
    where
        widget = Widget
            { _isFocused = True
            , _view = View reqSize $ img <> cursorFrame
            , _eventMap = eventMap cursor str displayStr myId
            , _mEnter = Nothing
            , _focalArea = cursorRect
            }
        reqSize = Vector2 (_sCursorWidth + tlWidth) tlHeight
        myAnimId = Widget.toAnimId myId
        img = cursorTranslate Style{..} $ frameGen myAnimId
        displayStr = makeDisplayStr _sEmptyFocusedString str
        (frameGen, Vector2 tlWidth tlHeight) =
            TextView.drawTextAsSingleLetters _sTextViewStyle displayStr

        cursorRect = mkCursorRect Style{..} cursor str
        cursorFrame =
            Anim.unitSquare _sTextCursorId
            & Anim.unitImages %~ Draw.tint _sCursorColor
            & Anim.unitIntoRect cursorRect
            & Anim.layers +~ 2 -- TODO: 2?!

mkCursorRect :: Style -> Int -> String -> Rect
mkCursorRect Style{..} cursor str = Rect cursorPos cursorSize
    where
        beforeCursorLines = splitWhen (== '\n') $ take cursor str
        lineHeight = TextView.lineHeight _sTextViewStyle
        cursorPos = Vector2 cursorPosX cursorPosY
        cursorSize = Vector2 _sCursorWidth lineHeight
        cursorPosX =
            TextView.drawTextAsSingleLetters _sTextViewStyle (last beforeCursorLines) ^.
            _2 . _1
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
        strWithIds = str ^@.. Lens.traversed <&> _1 %~ Just
        (before, after) = splitAt cursor strWithIds

make :: Style -> String -> Widget.Id -> Widget.Env -> Widget ((,) String)
make style str myId env =
    makeFunc style str myId
    where
        makeFunc =
            case Widget.subId myId (env ^. Widget.envCursor) of
            Nothing -> makeUnfocused
            Just suffix ->
                makeFocused (env ^. Widget.envCursorAnimId) (decodeCursor suffix)
        decodeCursor [x] = min (length str) $ BinUtils.decodeS x
        decodeCursor _ = length str
