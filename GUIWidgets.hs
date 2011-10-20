-- {-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
import Prelude hiding (lookup)
import Graphics.Rendering.OpenGL hiding (scale, Color)
import Graphics.UI.GLFW
import Control.Arrow
import Control.Newtype
import Control.Exception(bracket_, Exception, throwIO)
import Control.Monad(forever, unless, void)
import Control.Concurrent
import Control.Newtype.TH
import Control.Applicative
import Graphics.DrawingCombinators((%%))
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.DrawingCombinators.Affine as Affine
import Data.Typeable
import Data.List.Split(splitOn)
import Data.StateVar
import Data.Monoid
import Data.IORef
import Data.Char
import Data.Maybe
import qualified Data.Map as Map
import Data.Map(Map)

assert msg p = unless p (fail msg)

defaultFont :: FilePath
defaultFont = "/usr/share/fonts/truetype/freefont/FreeSerifBold.ttf"

withGLFW = bracket_ (initialize >>= assert "initialize failed") terminate

-- TODO: Modifiers
data EventType = CharEventType | KeyEventType Key
  deriving (Show, Eq, Ord)
data Event = CharEvent { fromCharEvent :: Char }
           | KeyEvent  { fromKeyEvent  :: Key }

eventTypeOf :: Event -> EventType
eventTypeOf (CharEvent _) = CharEventType
eventTypeOf (KeyEvent k) = KeyEventType k

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
        Draw.text font t,
        cursorImage
      ]

    cursorPos = Draw.textWidth font (take cursor t)
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

    singleton _doc eventType makeModel =
        pack . Map.singleton eventType $
        uncurry Model . makeModel
    specialton doc key = singleton doc (KeyEventType (SpecialKey key)) . const

    keymap =
      mconcat . concat $ [
        [ specialton "Move left" LEFT $
          moveRelative (-1)
        | cursor > 0 ],

        [ specialton "Move right" RIGHT $
          moveRelative 1
        | cursor < textLength ],

        [ specialton "Move up" UP $
          moveRelative (- cursorX - 1 - length (drop cursorX prevLine))
        | cursorY > 0 ],

        [ specialton "Move down" DOWN $
          moveRelative (length curLineAfter + 1 + min cursorX (length nextLine))
        | cursorY < height-1 ],

        -- [ homeKeymap "Move to beginning of line" $
        --   moveRelative (-cursorX)
        -- | cursorX > 0 ],

        -- [ endKeymap "Move to end of line" $
        --   moveRelative (length curLineAfter)
        -- | not . null $ curLineAfter ],

        -- [ homeKeymap "Move to beginning of text" $
        --   moveAbsolute 0
        -- | cursorX == 0 && cursor > 0 ],

        -- [ endKeymap "Move to end of text" $
        --   moveAbsolute textLength
        -- | null curLineAfter && cursor < textLength ],

        [ specialton "Delete backwards" BACKSPACE $
          backDelete 1
        | cursor > 0 ],

        -- [ specialton "Delete word backwards" (ctrlCharK 'w')
        --   backDeleteWord
        -- | cursor > 0 ],

        -- let swapPoint = min (textLength - 2) (cursor - 1)
        --     (beforeSwap, x:y:afterSwap) = splitAt swapPoint str
        --     swapLetters = (min textLength (cursor + 1),
        --                    beforeSwap ++ y:x:afterSwap)
        -- in

        -- [ specialton "Swap letters" (ctrlCharK 't')
        --   swapLetters
        -- | cursor > 0 && textLength >= 2 ],

        [ specialton "Delete forward" DEL $
          delete 1
        | cursor < textLength ],

        -- [ specialton "Delete word forward" (altCharK 'd')
        --   deleteWord
        -- | cursor < textLength ],

        -- [ specialton "Delete rest of line" (ctrlCharK 'k') $
        --   delete (length curLineAfter)
        -- | not . null $ curLineAfter ],

        -- [ specialton "Delete newline" (ctrlCharK 'k') $
        --   delete 1
        -- | null curLineAfter && cursor < textLength ],

        -- [ specialton "Delete till beginning of line" (ctrlCharK 'u') $
        --   backDelete (length curLineBefore)
        -- | not . null $ curLineBefore ],

        [ singleton "Insert character" CharEventType (insert . return . fromCharEvent) ],

        [ specialton "Insert Newline" ENTER (insert "\n") ]

        ]

    insert :: String -> (Cursor, String)
    insert l = if (length . splitLines) str' <= max height maxLines
               then (cursor', str')
               else (cursor, str)
      where
        cursor' = cursor + length l
        str' = concat [before, l, after]

data Quit = Quit
  deriving (Typeable, Show)
instance Exception Quit

main = withGLFW $ do
  font <- Draw.openFont defaultFont
  openWindow (Size 800 600) [] Window >>= assert "Open window failed"

  modelVar <- newIORef (Model 4 "Text")

  charCallback $= charHandler font modelVar
  keyCallback $= keyHandler font modelVar
  windowCloseCallback $= throwIO Quit
  forever $ do
    waitEvents
    model <- readIORef modelVar
    Draw.clearRender . (Draw.scale (20/800) (20/600) %%) . fst $ widget font model
    swapBuffers
    threadDelay 10000

widget :: Draw.Font -> Model -> (Draw.Image (), EventMap Model)
widget font = make font "<empty>" 1

updateModel font event model =
    fromMaybe model .
    lookup event .
    snd $
    widget font model

charHandler font modelVar char Press = do
    putStrLn $ "Char press: " ++ show char
    modifyIORef modelVar . updateModel font $ CharEvent char
charHandler _    _        char Release = do
    putStrLn ("Char released: " ++ show char)
    return ()

keyHandler  font modelVar key Press   = do
    putStrLn $ "Key pressed: " ++ show key
    modifyIORef modelVar . updateModel font $ KeyEvent key

keyHandler  _    _        key Release = do
    putStrLn $ "Key released: " ++ show key
    return ()
