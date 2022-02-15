{-# LANGUAGE TypeFamilies, OverloadedLists #-}

module Lamdu.Data.Export.JSON.Migration.ToVersion16 (migrate, replVar) where

import qualified Control.Lens as Lens
import           Control.Lens.Extended ((~~>))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Lens (_Object)
import           Data.List (sortOn)
import           Data.String (IsString(..))
import           Data.Vector.Lens (vector)
import           Lamdu.Data.Export.JSON.Migration.Common (migrateToVer)

import           Lamdu.Prelude

migrateVal :: Aeson.Value -> [Aeson.Value]
migrateVal x =
    case x ^? _Object . Lens.ix "repl" . _Object of
    Nothing -> [x]
    Just repl ->
        [ _Object #
            ( (repl & Lens.ix "val" %~ fixVal)
                <> "def" ~~> replVar
                <> "defPresentationMode" ~~> "Verbose"
                <> "tag" ~~> Aeson.Null
                <> "typ" ~~> (_Object # foralledType)
            )
        , _Object # "openDefPane" ~~> replVar
        ]
    where
        fixVal orig =
            Aeson.object
            [ ("id", "7265706c-686f-6c65-6672-61676170706c")
            , ("applyArg", orig)
            , ( "applyFunc",
                Aeson.object
                [ ("hole", Aeson.object [])
                , ("id", "7265706c-686f-6c65-7265-706c686f6c65")
                ] )
            ]
        foralledType =
            "typeVars" ~~> Aeson.Array ["61"] <>
            "schemeType" ~~> (_Object # "typeVar" ~~> "61")

replVar :: IsString a => a
replVar = "7265706c7265706c7265706c7265706c"

-- Fixing the order is necessary for re-export tests to succeed
fixOrder :: [Aeson.Value] -> [Aeson.Value]
fixOrder vals =
    pre <> sortOn sortOrder defs
    where
        sortOrder val =
            ( val ^? _Object . Lens.ix "openDefPane"
            , val ^? _Object . Lens.ix "def"
            )
        (pre, defs) = break (Lens.has (_Object . Lens.ix "def")) vals

migrate :: Aeson.Value -> Either Text Aeson.Value
migrate = migrateToVer 16 (pure . (Lens.from vector %~ fixOrder . (>>= migrateVal)))
