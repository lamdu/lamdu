{-# LANGUAGE TemplateHaskell #-}
module Test.Momentu.Env where

import qualified Control.Lens as Lens
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.Glue as Glue

import           Test.Lamdu.Prelude

data Env = Env
    { _eDirLayout :: Dir.Layout
    , _eDirTexts :: Dir.Texts Text
    , _eGlueTexts :: Glue.Texts Text
    }

env :: Env
env =
    Env
    { _eDirLayout = Dir.LeftToRight -- TODO: Test other layout directions
    , _eDirTexts =
        Dir.Texts
        { Dir._left = "left"
        , Dir._right = "right"
        , Dir._up = "up"
        , Dir._down = "down"
        , Dir._navigation = "navigation"
        , Dir._move = "move"
        }
    , _eGlueTexts =
        Glue.Texts
        { Glue._stroll = "stroll"
        , Glue._back = "back"
        , Glue._ahead = "ahead"
        }
    }

Lens.makeLenses ''Env

instance Dir.HasLayoutDir Env where layoutDir = eDirLayout
instance Dir.HasTexts Env where texts = eDirTexts
instance Glue.HasTexts Env where texts = eGlueTexts
