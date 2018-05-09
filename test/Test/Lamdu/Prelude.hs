-- | A prelude for Lamdu tests

module Test.Lamdu.Prelude
    ( module X
    ) where

import Lamdu.Prelude as X
import Test.Framework as X
import Test.Framework.Providers.HUnit as X (testCase)
import Test.Framework.Providers.QuickCheck2 as X (testProperty)
import Test.HUnit as X (assertString)
import Test.Lamdu.Instances ()
import Text.PrettyPrint.HughesPJClass as X (prettyShow)
