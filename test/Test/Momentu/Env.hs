{-# LANGUAGE TemplateHaskell #-}
module Test.Momentu.Env where

import qualified Control.Lens as Lens
import qualified GUI.Momentu.Direction as Dir

import           Test.Lamdu.Prelude

data Env = Env
    { _eLayoutDir :: Dir.Layout
    , _eDirTexts :: Dir.Texts Text
    }

env :: Env
env =
    Env
    { _eLayoutDir = Dir.LeftToRight -- TODO: Test other layout directions
    , _eDirTexts =
        Dir.Texts
        { Dir._left = "left"
        , Dir._right = "right"
        , Dir._up = "up"
        , Dir._down = "down"
        , Dir._navigation = "navigation"
        , Dir._move = "move"
        }
    }

Lens.makeLenses ''Env

instance Dir.HasLayoutDir Env where layoutDir = eLayoutDir
instance Dir.HasTexts Env where texts = eDirTexts
