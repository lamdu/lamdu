module Lamdu.Editor.Fonts
    ( makeGetFonts
    ) where

import qualified Control.Lens.Extended as Lens
import           Data.MRUMemo (memoIO)
import qualified GUI.Momentu as M
import           Lamdu.Config.Sampler (Sampler, Sample)
import qualified Lamdu.Config.Sampler as Sampler
import           Lamdu.Config.Theme (Theme(..))
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Config.Theme.Fonts (FontSize, Fonts(..))
import qualified Lamdu.Config.Theme.Fonts as Fonts
import qualified Lamdu.Font as Font
import           System.FilePath ((</>))
import qualified System.FilePath as FilePath

import           Lamdu.Prelude

sampleConfigPath :: Lens' Sample FilePath
sampleConfigPath = Sampler.sData . Sampler.sConfig . Sampler.filePath

prependConfigPath :: Sample -> Fonts FilePath -> Fonts FilePath
prependConfigPath sample =
    Lens.mapped %~ f
    where
        dir = FilePath.takeDirectory (sample ^. sampleConfigPath)
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
    sample ^. Sampler.sThemeData . Theme.fonts
    & prependConfigPath sample
    & assignFontSizes (sample ^. Sampler.sThemeData)

defaultFontPath :: FilePath -> FilePath
defaultFontPath configPath =
    configDir </> "fonts/Purisa.ttf"
    where
        configDir = FilePath.takeDirectory configPath

makeGetFonts ::
    Sampler -> Font.LCDSubPixelEnabled ->
    IO (M.Zoom -> IO (Fonts M.Font))
makeGetFonts configSampler subpixel =
    Font.new subpixel & uncurry & memoIO
    <&> f
    where
        f cachedLoadFonts zoom =
            do
                sizeFactor <- M.getZoomFactor zoom
                sample <- Sampler.getSample configSampler
                cachedLoadFonts
                    ( defaultFontPath (sample ^. sampleConfigPath)
                    , curSampleFonts sample <&> _1 *~ sizeFactor
                    )
