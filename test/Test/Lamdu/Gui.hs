module Test.Lamdu.Gui (verifyLayers) where

import           Data.List (group, sort)
import qualified GUI.Momentu.Animation as Anim
import qualified GUI.Momentu.Element as Element

import           Test.Lamdu.Prelude

verifyLayers :: Element.Layers -> IO ()
verifyLayers view =
    case clashingIds of
    [] -> pure ()
    _ -> assertString ("Clashing anim ids: " <> show clashingIds)
    where
        animIds = view ^.. Element.layers . traverse . Anim.frameImages . traverse . Anim.iAnimId
        clashingIds = sort animIds & group >>= tail

