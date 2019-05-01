module Lamdu.Editor.Fonts
    ( makeGetFonts
    ) where

import qualified Control.Lens.Extended as Lens
import           Data.MRUMemo (memoIO)
import qualified GUI.Momentu as M
import           Lamdu.Config.Sampler (Sample, sData, sConfigPath, sThemeData)
import           Lamdu.Config.Theme (Theme(..))
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Config.Theme.Fonts (FontSize, Fonts(..))
import qualified Lamdu.Config.Theme.Fonts as Fonts
import qualified Lamdu.Font as Font
import           System.FilePath ((</>))
import qualified System.FilePath as FilePath

import           Lamdu.Prelude

prependConfigPath :: Sample -> Fonts FilePath -> Fonts FilePath
prependConfigPath sample =
    Lens.mapped %~ f
    where
        dir = FilePath.takeDirectory (sample ^. sData . sConfigPath)
        f "" = "" -- Debug font!
        f x = dir </> x

assignFontSizes :: Theme -> Fonts FilePath -> Fonts (FontSize, FilePath)
assignFontSizes theme fonts =
    fonts
    <&> (,) baseTextSize
    & Fonts.help . _1 .~ helpTextSize
    where
        baseTextSize = theme ^. Theme.baseTextSize
        helpTextSize = theme ^. Theme.help . Theme.helpTextSize

curSampleFonts :: Sample -> Fonts (FontSize, FilePath)
curSampleFonts sample =
    sample ^. sThemeData . Theme.fonts
    & prependConfigPath sample
    & assignFontSizes (sample ^. sThemeData)

defaultFontPath :: FilePath -> FilePath
defaultFontPath configPath =
    configDir </> "fonts/Purisa.ttf"
    where
        configDir = FilePath.takeDirectory configPath

makeGetFonts ::
    Font.LCDSubPixelEnabled ->
    IO (M.Zoom -> Sample -> IO (Fonts M.Font))
makeGetFonts subpixel =
    Font.new subpixel & uncurry & memoIO
    <&> f
    where
        f cachedLoadFonts zoom sample =
            do
                sizeFactor <- M.getZoomFactor zoom
                cachedLoadFonts
                    ( defaultFontPath (sample ^. sData . sConfigPath)
                    , curSampleFonts sample <&> _1 *~ sizeFactor
                    )
