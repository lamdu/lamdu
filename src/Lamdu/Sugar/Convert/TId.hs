{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}

module Lamdu.Sugar.Convert.TId
    ( JumpToNominal(..)
    , convert
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Unit (Unit(..))
import           Control.Monad.Transaction (Transaction, MonadTransaction)
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Sugar.Convert.NameRef (jumpToNominal)
import           Lamdu.Sugar.Internal
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

class JumpToNominal m o where
    jumpToNom :: m (T.NominalId -> o EntityId)

instance Applicative m => JumpToNominal m Unit where
    jumpToNom = pure (pure Unit)

instance (MonadReader env m, Anchors.HasCodeAnchors env n, Monad n) => JumpToNominal m (Transaction n) where
    jumpToNom = Lens.view Anchors.codeAnchors <&> jumpToNominal

convert :: (MonadTransaction n m, JumpToNominal m o) => T.NominalId -> m (TId InternalName o)
convert tid =
    (TId ?? tid)
    <$> taggedName Nothing tid
    <*> (jumpToNom ?? tid)
