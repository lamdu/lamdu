{-# LANGUAGE TemplateHaskell #-}
-- | The themes/ config format
module Lamdu.Config.Theme.TextColors where

import qualified Control.Lens as Lens
import           Data.Aeson.TH (deriveJSON)
import qualified Data.Aeson.Types as Aeson
import           Data.List.Lens (prefixed)
import qualified GUI.Momentu.Draw as Draw

import           Lamdu.Prelude

data TextColors = TextColors
    { _baseColor :: Draw.Color
    , _nomColor :: Draw.Color
    , _literalColor :: Draw.Color
    , _grammarColor :: Draw.Color
    , _caseTailColor :: Draw.Color
    , _recordTailColor :: Draw.Color
    , _lightLambdaUnderlineColor :: Draw.Color
    , _foreignModuleColor :: Draw.Color
    , _foreignVarColor :: Draw.Color
    , _presentationChoiceColor :: Draw.Color
    , _actionTextColor :: Draw.Color
    , _infoTextColor :: Draw.Color
    , _typeTextColor :: Draw.Color
    -- Names:
    , _collisionSuffixTextColor :: Draw.Color
    , _definitionColor :: Draw.Color
    , _parameterColor :: Draw.Color
    , _letColor :: Draw.Color
    , _recordTagColor :: Draw.Color
    , _caseTagColor :: Draw.Color
    , _argTagColor :: Draw.Color
    } deriving (Eq, Show)
deriveJSON Aeson.defaultOptions
    {Aeson.fieldLabelModifier = (^?! prefixed "_")}
    ''TextColors

Lens.makeLenses ''TextColors
