module Lamdu.Editor.Fonts
    ( makeGetFonts
    ) where

import qualified Control.Lens as Lens
import           Data.IORef
import           Data.MRUMemo (memoIO)
import           GUI.Momentu (Font, Zoom)
import qualified GUI.Momentu.Zoom as Zoom
import           Lamdu.Config.Sampler (Sampler, Sample)
import qualified Lamdu.Config.Sampler as Sampler
import           Lamdu.Config.Theme (Theme(..))
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Config.Theme.Fonts (FontSize, Fonts(..))
import qualified Lamdu.Config.Theme.Fonts as Fonts
import qualified Lamdu.Font as Font
import qualified Lamdu.I18N.Language as Language
import           System.FilePath ((</>))
import qualified System.FilePath as FilePath
import           System.Mem (performGC)

import           Lamdu.Prelude

sampleConfigPath :: Lens' Sample FilePath
sampleConfigPath = Sampler.sData . Sampler.sConfig . Sampler.primaryPath

prependConfigPath :: Sample -> Fonts Theme.FontSel -> Fonts FilePath
prependConfigPath sample =
    Lens.mapped %~ f
    where
        dir = FilePath.takeDirectory (sample ^. sampleConfigPath)
        fonts = sample ^. Sampler.sLanguageData . Language.lFonts
        f sel = dir </> fonts ^# Theme.fontSel sel

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
    IO (Zoom -> IO (Fonts Font))
makeGetFonts configSampler subpixel =
    do
        timeTillGc <- newIORef gcEvery
        let maybePerformGC =
                atomicModifyIORef timeTillGc
                (\ttl -> if ttl == 0 then (gcEvery, performGC) else (ttl-1, pure ()))
                & join
        fmap f . memoIO $
            \(path, fonts) ->
            do
                -- If we don't force full GC once in a while, these
                -- fonts linger and are uncollected. They leak not
                -- only memory but file descriptors, and those run out
                -- first, crashing lamdu if fonts are reloaded too frequently
                maybePerformGC
                Font.new subpixel path fonts
    where
        gcEvery :: Int
        gcEvery = 10
        f cachedLoadFonts zoom =
            do
                sizeFactor <- Zoom.getZoomFactor zoom
                sample <- Sampler.getSample configSampler
                cachedLoadFonts
                    ( defaultFontPath (sample ^. sampleConfigPath)
                    , curSampleFonts sample <&> _1 *~ sizeFactor
                    )
