module Main where

import qualified Control.Lens as Lens
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Diff as AesonDiff
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSChar
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Proxy (Proxy(..), asProxyTypeOf)
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Align (Aligned(..))
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.Animation (R)
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.Rect as Rect
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Options as Options
import           GUI.Momentu.View (View(..))
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.GridView as GridView
import           Lamdu.Config (Config)
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.Data.Export.JSON as JsonFormat
import qualified Lamdu.Paths as Paths
import qualified Lamdu.Themes as Themes
import qualified TestColorSchemes
import qualified TestStdlib
import           Test.Framework
import           Test.Framework.Providers.HUnit (testCase)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.HUnit
import           Test.Lamdu.Instances ()

import           Lamdu.Prelude

jsonCodecMigrationTest :: IO ()
jsonCodecMigrationTest = JsonFormat.fileImportAll "test/old-codec-factorial.json" & void

propGridSensibleSize :: NonEmpty (NonEmpty (Aligned (Vector2 R))) -> Bool
propGridSensibleSize viewConfs =
    size == grid ^. Element.size &&
    isFinite (size ^. _1) && isFinite (size ^. _2) &&
    and ((>=) <$> size <*> minGridSize views) &&
    Lens.allOf (traverse . traverse) goodPlacement placements &&
    Lens.allOf (traverse . traverse . Align.alignmentRatio . traverse) goodAlignment alignments
    where
        isFinite x = not (isNaN x || isInfinite x)
        views = viewsFromConf viewConfs
        (alignments, grid) = GridView.make views
        (size, placements) = GridView.makePlacements views
        goodPlacement (Aligned alignment (place, view)) =
            vSize == place ^. Rect.size &&
            pos vSize && pos (place ^. Rect.topLeft) &&
            and ((<=) <$> place ^. Rect.bottomRight <*> size * 1.001) &&
            Lens.allOf traverse goodAlignment alignment
            where
                pos x = and ((<=) <$> 0 <*> x)
                vSize = view ^. Element.size
        goodAlignment x = isFinite x && 0 <= x && x <= 1

minGridSize ::
    (Traversable vert, Element.SizedElement view) =>
    vert (NonEmpty (Aligned view)) -> Vector2 R
minGridSize views =
    Vector2 (sum colWidths) (sum rowHeights) * 0.999 -- Due to float inaccuracies
    where
        colWidths =
            views <&> Lens.mapped %~ (^. Element.width)
            & foldl1 (NonEmpty.zipWith max)
        rowHeights = views <&> Lens.mapped %~ (^. Element.height) <&> maximum

viewsFromConf :: NonEmpty (NonEmpty (Aligned (Vector2 R))) -> NonEmpty (NonEmpty (Aligned View))
viewsFromConf viewConfs =
    viewConfs
    <&> onTail (take minRowTailLen)
    <&> traverse . Align.value %~ (`View` mempty)
    where
        minRowTailLen = minimum (viewConfs ^.. traverse <&> NonEmpty.tail <&> length)
        onTail f (x :| xs) = x :| f xs

verticalDisambigTest :: IO ()
verticalDisambigTest =
    do
        check Responsive.LayoutClear (Vector2 1 2)
        check Responsive.LayoutVertical (Vector2 1.5 2)
    where
        check ctx expect
            | size == expect = pure ()
            | otherwise =
                assertString ("unexpected size " <> show size <> ", expected " <> show expect)
            where
                size = rendered ^. Align.tValue . Widget.wSize
                rendered =
                    (box ^. Responsive.render)
                    Responsive.LayoutParams
                    { Responsive._layoutMode = Responsive.LayoutNarrow 1.9
                    , Responsive._layoutContext = ctx
                    }
                box =
                    Options.box disambig [unitItem, unitItem]
                    <&> (<>[]) -- to avoid ambiguous type var
        unitItem = Element.assymetricPad 0 1 Element.empty
        disambig =
            Options.disambiguationNone
            & Options.disambVert .~ Element.assymetricPad (Vector2 0.5 0) 0

verifyJson :: (Aeson.FromJSON t, Aeson.ToJSON t) => Proxy t -> FilePath -> IO ()
verifyJson proxy jsonPath =
    do
        configPath <- Paths.getDataFileName jsonPath
        json <-
            LBS.readFile configPath <&> Aeson.eitherDecode >>=
            \case
            Left err ->
                do
                    assertString ("Failed to load " <> configPath <> ": " <> err)
                    fail "Test failure"
            Right x -> pure x
        case Aeson.fromJSON json <&> (`asProxyTypeOf` proxy) of
            Aeson.Error msg -> assertString ("Failed decoding " <> configPath <> " from json: " <> msg)
            Aeson.Success val
                | rejson == json -> pure ()
                | otherwise ->
                    assertString ("json " <> configPath <> " contains unexpected data:\n" <>
                        LBSChar.unpack (AesonPretty.encodePretty (Aeson.toJSON (AesonDiff.diff rejson json))))
                where
                    rejson = Aeson.toJSON val

configParseTest :: IO ()
configParseTest =
    do
        verifyJson (Proxy :: Proxy Config) "config.json"
        Themes.getFiles >>= traverse_ (verifyJson (Proxy :: Proxy Theme))

main :: IO ()
main =
    defaultMainWithOpts tests mempty
    where
        tests =
            TestStdlib.tests ++
            [ TestColorSchemes.test
            , testCase "json-codec-migration" jsonCodecMigrationTest
            , testCase "vertical-disambguation" verticalDisambigTest
            , testCase "config-parses" configParseTest
            , testProperty "grid-sensible-size" propGridSensibleSize
                & plusTestOptions mempty
                { topt_maximum_generated_tests = Just 1000
                }
            ]
