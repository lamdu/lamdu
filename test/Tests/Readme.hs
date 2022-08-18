module Tests.Readme (test) where

import qualified Control.Lens as Lens
import qualified Data.Aeson.Lens as LensAeson
import           Data.List.Lens
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Yaml as Yaml

import           Test.Lamdu.Prelude

test :: TestTree
test =
    do
        travisYaml <-
            Yaml.decodeFileEither ".travis.yml"
            >>= either (fail . Yaml.prettyPrintParseException) pure
            :: IO Yaml.Value
        let travisDeps =
                travisYaml
                ^.. LensAeson.key "before_install" . LensAeson.values
                . LensAeson._String . Lens.to Text.unpack . prefixed aptget
                >>= words & filter notFlag
        readme <- readFile "README.md"
        let readmeUbuntu =
                lines readme
                & dropWhile (/= "#### ubuntu")
                & tail
                & takeWhile (Lens.hasn't (prefixed "#### "))
        let readmeDeps =
                readmeUbuntu ^.. traverse . prefixed aptget
                >>= words & filter notFlag
        let missingDeps = Set.difference (Set.fromList travisDeps) (Set.fromList readmeDeps)
        unless (Set.null missingDeps)
            (assertString ("Travis deps not in README: " ++ show missingDeps))
    & testCase "readme-ubuntu-deps"
    where
        aptget = "sudo apt-get install "
        notFlag = Lens.hasn't (prefixed "-")
