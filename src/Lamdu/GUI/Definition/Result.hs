{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}

module Lamdu.GUI.Definition.Result (DefRes(..), _DefRes) where

import qualified Control.Lens as Lens
import           Control.Monad.Trans.Class (MonadTrans)
import           Control.Monad.Trans.FastWriter (WriterT)
import           Data.Monoid (Any)

import           Lamdu.Prelude

-- The results of definition pane can either execute underlying db transactions (edits),
-- or output to execute the definition (the writer result).
newtype DefRes m a = DefRes (WriterT Any m a)
    deriving newtype (Functor, Applicative, Monad, MonadTrans)

Lens.makePrisms ''DefRes
