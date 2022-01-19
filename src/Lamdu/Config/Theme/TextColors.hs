{-# LANGUAGE TemplateHaskell #-}
-- | The themes/ config format
module Lamdu.Config.Theme.TextColors where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit

import           Lamdu.Prelude

data TextColors = TextColors
    { _baseColor :: Draw.Color
    , _emptyEditColors :: TextEdit.Modes Draw.Color
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
    , _variableColor :: Draw.Color
    , _recordTagColor :: Draw.Color
    , _caseTagColor :: Draw.Color
    , _argTagColor :: Draw.Color
    } deriving (Eq, Show, Generic)
JsonTH.derivePrefixed "_" ''TextColors

Lens.makeLenses ''TextColors
