{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Eval.JS.Types
    ( JSDebugPaths(..), jsDebugCodePath, jsDebugNodeOutputPath, jsDebugInteractivePath
    ) where

import qualified Control.Lens as Lens
import           Lamdu.Prelude

data JSDebugPaths a = JSDebugPaths
    { _jsDebugCodePath :: Maybe a
    , _jsDebugNodeOutputPath :: Maybe a
    , _jsDebugInteractivePath :: Maybe a
    } deriving (Functor, Foldable, Traversable)

Lens.makeLenses ''JSDebugPaths
