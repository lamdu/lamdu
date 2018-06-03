-- | A prelude for Lamdu tests

module Test.Lamdu.Prelude
    ( module X
    , runTest
    ) where

import Lamdu.Prelude as X
import Test.Framework as X
import Test.Framework.Providers.HUnit as X (testCase)
import Test.Framework.Providers.QuickCheck2 as X (testProperty)
import Test.HUnit as X (assertString, assertEqual)
import Test.Lamdu.Instances ()
import Text.PrettyPrint.HughesPJClass as X (prettyShow)

-- | Useful for running test in a terminal-incapable terminal, with
-- colors disabled
runTest :: Test -> IO ()
runTest test =
    defaultMainWithOpts [test] mempty { ropt_color_mode = Just ColorNever }
