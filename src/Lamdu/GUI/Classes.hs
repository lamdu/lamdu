-- | Type-class for monads where tags can be named

{-# LANGUAGE MultiParamTypeClasses #-}

module Lamdu.GUI.Classes
    ( SetTagName(..), InfoMonad(..)
    ) where

import qualified Lamdu.Calc.Type as T

import           Lamdu.Prelude

class SetTagName m o where
    setTagName :: m (T.Tag -> Text -> o ())

-- Consider replacing with MonadTrans? Would necessitate parameter order change in GuiM
class InfoMonad m i where
    liftInfo :: i a -> m a
