{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, TemplateHaskell #-}

module Lamdu.Sugar.Names.Add.Pass0LoadNames
    ( Pass0LoadNames, runPass0LoadNames
    , P0Name(..), P0Env(..)
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Reader (ReaderT(..))
import qualified Control.Monad.Reader as Reader
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Tag as Tag
import           Lamdu.Sugar.Internal
import           Lamdu.Sugar.Names.CPS (CPS(..))
import           Lamdu.Sugar.Names.Walk (MonadNameWalk(..))

import           Lamdu.Prelude

data P0Name = P0Name
    { _p0TagName :: Tag.TextsInLang
    , _p0IsOperator :: Tag.IsOperator
    , _p0InternalName :: InternalName
    }

newtype P0Env i = P0Env
    { _p0GetName :: T.Tag -> i (Tag.IsOperator, Tag.TextsInLang)
    }
Lens.makeLenses ''P0Env

newtype Pass0LoadNames i a =
    Pass0LoadNames { unPass0LoadNames :: ReaderT (P0Env i) i a }
    deriving newtype (Functor, Applicative, Monad, MonadReader (P0Env i))

runPass0LoadNames :: P0Env i -> Pass0LoadNames i a -> i a
runPass0LoadNames r = (`runReaderT` r) . unPass0LoadNames

instance Monad i => MonadNameWalk (Pass0LoadNames i) where
    type OldName (Pass0LoadNames i) = InternalName
    type NewName (Pass0LoadNames i) = P0Name
    type IM (Pass0LoadNames i) = i
    opRun = Reader.asks runPass0LoadNames
    opWithName _ _ n = CPS $ \inner -> (,) <$> getP0Name n <*> inner
    opGetName _ _ _ = getP0Name
    opWithNewTag _ _ = id

p0lift :: Monad i => i a -> Pass0LoadNames i a
p0lift = Pass0LoadNames . lift

getP0Name :: Monad i => InternalName -> Pass0LoadNames i P0Name
getP0Name internalName =
    Lens.view p0GetName ?? internalName ^. inTag >>= p0lift
    <&> \(isOp, x) -> P0Name x isOp internalName
