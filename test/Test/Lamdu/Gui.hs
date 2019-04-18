module Test.Lamdu.Gui (verifyLayers) where

import           Data.List (group, sort)
import qualified GUI.Momentu.Animation as Anim
import qualified GUI.Momentu.Element as Element
import           GHC.Stack (prettyCallStack, callStack)

import           Test.Lamdu.Prelude

verifyLayers :: HasCallStack => Element.Layers -> Either String ()
verifyLayers view =
    case clashingIds of
    [] -> Right ()
    _ -> Left (prettyCallStack callStack <> ": Clashing anim ids: " <> show clashingIds)
    where
        animIds = view ^.. Element.layers . traverse . Anim.frameImages . traverse . Anim.iAnimId
        clashingIds = sort animIds & group >>= tail
