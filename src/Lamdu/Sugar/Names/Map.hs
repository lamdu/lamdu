{-# LANGUAGE TemplateHaskell, TypeFamilies, GeneralizedNewtypeDeriving #-}
-- | fmap for names
module Lamdu.Sugar.Names.Map
    ( runMapNames
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Reader (Reader, runReader, MonadReader)
import           Lamdu.Sugar.Names.CPS (liftCPS)
import           Lamdu.Sugar.Names.Walk (MonadNaming(..))

import           Lamdu.Prelude

newtype Env a b = Env { _envMapping :: a -> b }
Lens.makeLenses ''Env

newtype MapNames (m :: * -> *) a b x = MapNames (Reader (Env a b) x)
    deriving (Functor, Applicative, Monad, MonadReader (Env a b))

instance Monad m => MonadNaming (MapNames m a b) where
    type OldName (MapNames m a b) = a
    type NewName (MapNames m a b) = b
    type IM (MapNames m a b) = m
    opRun = Lens.view envMapping <&> \f -> pure . runMapNames f
    opWithName _ _ name = liftCPS (Lens.view envMapping) ?? name
    opGetName _ _ name = Lens.view envMapping ?? name

runMapNames :: (a -> b) -> MapNames m a b x -> x
runMapNames f (MapNames act) = runReader act (Env f)
