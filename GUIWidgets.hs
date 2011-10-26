-- {-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
import Prelude hiding (lookup)

import qualified GLFWWrap

import qualified Codec.Binary.UTF8.String as UTF8
import Control.Concurrent (forkIO, threadDelay)
import Control.Newtype
import Control.Monad
import Control.Newtype.TH
import Data.Char
import Data.IORef
import Data.List.Split(splitOn)
import qualified Data.Map as Map
import Data.Map(Map)
import Data.Maybe
import Data.Monoid
import qualified Data.Set as Set
import Data.Set(Set)
import Data.Time.Clock
import Graphics.DrawingCombinators((%%))
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.DrawingCombinators.Affine as Affine
import Graphics.UI.GLFW
import qualified System.Info

defaultFont :: String -> FilePath
defaultFont "darwin" = "/Library/Fonts/Arial.ttf"
defaultFont _ = "/usr/share/fonts/truetype/freefont/FreeSerifBold.ttf"

data ModState = ModState {
  modCtrl :: Bool,
  modMeta :: Bool,
  modAlt :: Bool,
  modShift :: Bool
  }
  deriving (Show, Eq, Ord)

noMods = ModState False False False False
shift = noMods { modShift = True }
ctrl = noMods { modCtrl = True }
alt = noMods { modAlt = True }

instance Ord Key where
    compare a b = compare (show a) (show b)

-- TODO: Modifiers
data EventType = CharEventType | KeyEventType ModState Key
  deriving (Show, Eq, Ord)
data Event = CharEvent { fromCharEvent :: Char }
           | KeyEvent ModState Key
  deriving (Show, Eq, Ord)

modStateFromKeySet :: Set Key -> ModState
modStateFromKeySet keySet =
  ModState {
    modCtrl = isPressed [KeyLeftCtrl, KeyRightCtrl],
    modMeta = False, -- TODO: GLFW doesn't support meta/winkey?
    modAlt = isPressed [KeyLeftAlt, KeyRightAlt],
    modShift = isPressed [KeyLeftShift, KeyRightShift]
    }
  where
    isPressed = any (`Set.member` keySet)

eventTypeOf :: Event -> EventType
eventTypeOf (CharEvent _) = CharEventType
eventTypeOf (KeyEvent ms k) = KeyEventType ms k

newtype EventMap a = EventMap (Map EventType (Event -> a))
  deriving (Monoid)
$(mkNewTypes [''EventMap])

lookup :: Event -> EventMap a -> Maybe a
lookup event = fmap ($ event) .
               Map.lookup (eventTypeOf event) .
               unpack

type Cursor = Int

data Model = Model {
  textEditCursor :: Cursor,
  textEditText :: String
  }
  deriving (Show, Read, Eq, Ord)

splitLines :: String -> [String]
splitLines = splitOn "\n"

square = Draw.convexPoly [ (-1, -1), (1, -1), (1, 1), (-1, 1) ]

tillEndOfWord :: String -> String
tillEndOfWord xs = spaces ++ nonSpaces
  where
    spaces = takeWhile isSpace xs
    nonSpaces = takeWhile (not . isSpace) . dropWhile isSpace $ xs

-- | Note: maxLines prevents the *user* from exceeding it, not the
-- | given text...
make :: Draw.Font -> String -> Int -> Model -> (Draw.Image (), EventMap Model)
make font emptyString maxLines (Model cursor str) = (void image, keymap)
  where
    t = finalText str
    finalText "" = emptyString
    finalText t  = t

    image =
      mconcat [
        Draw.text font $ UTF8.encodeString t,
        cursorImage
      ]

    cursorPos = Draw.textWidth font . UTF8.encodeString $ take cursor t
    cursorImage = Draw.tint (Draw.Color 0 1 0 1) $
                  Affine.translate (cursorPos, 0.5) %%
                  Draw.scale 0.1 1 %%
                  square

    (before, after) = splitAt cursor str
    textLength = length str
    textLines = splitLines str
    width = maximum . map length $ textLines
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
        pack . Map.singleton eventType $
        uncurry Model . makeModel

    keys doc = mconcat . map (\event -> singleton doc event . const)

    specialKey = KeyEventType noMods
    ctrlSpecialKey = KeyEventType ctrl
    ctrlCharKey = KeyEventType ctrl . CharKey . toUpper
    altCharKey = KeyEventType alt . CharKey . toUpper
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

        [ singleton "Insert character" CharEventType (insert . return . fromCharEvent) ],

        [ keys "Insert Newline" [specialKey KeyEnter] (insert "\n") ]

        ]

    insert :: String -> (Cursor, String)
    insert l = if (length . splitLines) str' <= max height maxLines
               then (cursor', str')
               else (cursor, str)
      where
        cursor' = cursor + length l
        str' = concat [before, l, after]

data TypematicState = NoKey | TypematicRepeat { tsKey :: Key, tsStartTime :: UTCTime, tsCount :: Int }

assert :: Monad m => String -> Bool -> m ()
assert msg p = unless p (fail msg)

typematicKeyHandlerWrap :: (Int -> NominalDiffTime) -> (Key -> Bool -> IO ()) -> IO (Key -> Bool -> IO ())
typematicKeyHandlerWrap timeFunc wrappedHandler = do
  stateVar <- newIORef NoKey
  _ <- forkIO . forever $ do
    state <- readIORef stateVar
    sleepTime <- case state of
      TypematicRepeat key startTime count -> do
        now <- getCurrentTime
        let timeDiff = diffUTCTime now startTime
        if timeDiff >= timeFunc count
          then do
            wrappedHandler key True
            writeIORef stateVar . TypematicRepeat key startTime $ count + 1
            return $ timeFunc (count + 1) - timeDiff
          else
            return $ timeFunc count - timeDiff
      _ -> return $ timeFunc 0
    threadDelay . round $ 1000000 * sleepTime
  let
    handler key True = do
      now <- getCurrentTime
      writeIORef stateVar $ TypematicRepeat key now 0
      wrappedHandler key True
    handler key False = do
      writeIORef stateVar NoKey
      wrappedHandler key False
  return handler

modifiersEventHandlerWrap :: (Event -> IO ()) -> IO (GLFWWrap.GLFWEvent -> IO ())
modifiersEventHandlerWrap wrappedHandler = do
  keySetVar <- newIORef Set.empty
  let
    handler (GLFWWrap.KeyEvent key True) = do
      modifyIORef keySetVar (Set.insert key)
      keySet <- readIORef keySetVar
      wrappedHandler $ KeyEvent (modStateFromKeySet keySet) key
    handler (GLFWWrap.KeyEvent key False) =
      modifyIORef keySetVar (Set.delete key)
    handler (GLFWWrap.CharEvent char True) = do
      keySet <- readIORef keySetVar
      when (modStateFromKeySet keySet `elem` [noMods, shift]) . wrappedHandler $ CharEvent char
    handler _ = return ()
  return handler

main = GLFWWrap.withGLFW $ do
  font <- Draw.openFont (defaultFont System.Info.os)
  openWindow defaultDisplayOptions >>= assert "Open window failed"

  modelVar <- newIORef (Model 4 "Text")

  modifiersHandler <- modifiersEventHandlerWrap (modifyIORef modelVar . updateModel font)

  let
    keyHandler key isPress = modifiersHandler $ GLFWWrap.KeyEvent key isPress
    typematicTime x = 0.5 + fromIntegral x * 0.05

  typematicKeyHandler <- typematicKeyHandlerWrap typematicTime keyHandler

  let
    handleEvent (GLFWWrap.KeyEvent key isPress) = typematicKeyHandler key isPress
    handleEvent GLFWWrap.WindowClose = error "Quit"
    handleEvent x = modifiersHandler x

  GLFWWrap.eventLoop $ \events -> do
    mapM_ handleEvent events
    Draw.clearRender . (Draw.scale (20/800) (20/600) %%) . fst . widget font =<< readIORef modelVar

widget :: Draw.Font -> Model -> (Draw.Image (), EventMap Model)
widget font = make font "<empty>" 2

updateModel font event model =
    fromMaybe model .
    lookup event .
    snd $
    widget font model
