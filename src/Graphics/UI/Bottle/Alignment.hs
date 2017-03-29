-- | Alignment data-type
{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, TypeSynonymInstances, MultiParamTypeClasses #-}
module Graphics.UI.Bottle.Alignment
    ( Alignment(..), ratio
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Tuple
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.UI.Bottle.Animation as Anim

newtype Alignment = Alignment { _ratio :: Vector2 Anim.R } -- ^ 0..1
    deriving (Num, Fractional)
Lens.makeLenses ''Alignment

instance Field1 Alignment Alignment Anim.R Anim.R where
    _1 = ratio . _1

instance Field2 Alignment Alignment Anim.R Anim.R where
    _2 = ratio . _2


