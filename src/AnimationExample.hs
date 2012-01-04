{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative((<$), pure)
import Control.Arrow((***))
import Data.ByteString.Char8() -- IsString instance
import Data.IORef
import Data.Monoid(Monoid(..))
import Data.Vector.Vector2 (Vector2(..))
import Data.Fixed(mod')
import Graphics.DrawingCombinators((%%))
import Graphics.DrawingCombinators.Utils(drawTextLines)
import qualified Graphics.UI.Bottle.MainLoop as MainLoop
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Data.Map.Ordered as OrderedMap
import qualified Graphics.DrawingCombinators as Draw
import qualified System.Info

defaultFont :: String -> FilePath
defaultFont "darwin" = "/Library/Fonts/Arial.ttf"
defaultFont _ = "/usr/share/fonts/truetype/freefont/FreeSerifBold.ttf"

red :: Draw.Color
red = Draw.Color 1 0 0 1

exampleFrame :: Bool -> Draw.R -> Draw.Font -> Anim.Frame
exampleFrame b x font =
  Anim.Frame . OrderedMap.fromList . (if b then reverse else id) $ items
  where
    items =
      [(["shraga"],
        Anim.PositionedImage (() <$ circle)
        (Anim.Rect (Vector2 x 0) (Vector2 100 100))),
       (["shraga", "name"],
        Anim.PositionedImage (() <$ drawTextLines font 1 ["Shraga"])
        (Anim.Rect (Vector2 x 100) (Vector2 20 20))),
       (["yosi"],
        Anim.PositionedImage (() <$ red `Draw.tint` circle)
        (Anim.Rect (Vector2 (800 - x) 0) (Vector2 200 100))),
       (["yosi", "name"],
        Anim.PositionedImage (() <$ red `Draw.tint` drawTextLines font 1 ["Yosi"])
        (Anim.Rect (Vector2 (800 - x) 100) (Vector2 20 20)))
      ]
    circle =
      Draw.scale 0.5 0.5 %% Draw.translate (1, 1) %% Draw.circle

main :: IO ()
main = do
  font <- Draw.openFont (defaultFont System.Info.os)
  xRef <- newIORef (False, 100)
  imageRef <- newIORef mempty
  let
    mkDestFrame = do
      (order, x) <- readIORef xRef
      return $ exampleFrame order x font
    mkFrame = do
      dest <- mkDestFrame
      prevFrame <- readIORef imageRef
      let image = Anim.nextFrame dest prevFrame
      writeIORef imageRef image
      return $ Anim.draw image
    eventHandler = modifyIORef xRef (not *** ((`mod'` 1000) . (+100)))
  MainLoop.mainLoop ((pure . pure) eventHandler) (pure mkFrame)
