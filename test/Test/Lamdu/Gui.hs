module Test.Lamdu.Gui (verifyLayers) where

import           Data.List (group, sort)
import           GHC.Stack (prettyCallStack, callStack)
import qualified GUI.Momentu.Animation as Anim
import qualified GUI.Momentu.Element as Element

import           Test.Lamdu.Prelude

verifyLayers :: HasCallStack => String -> Element.LayeredImage -> Either String ()
verifyLayers msg view =
    case clashingIds of
    [] -> Right ()
    _ -> Left (prettyCallStack callStack <> "/" <> msg <> ": Clashing anim ids: " <> show clashingIds)
    where
        animIds = view ^.. Element.layers . traverse . Anim.frameImages . traverse . Anim.iAnimId
        clashingIds = sort animIds & group >>= tail
