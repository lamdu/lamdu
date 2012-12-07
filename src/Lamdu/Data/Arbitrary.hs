{-# OPTIONS -fno-warn-orphans #-} -- Arbitrary Data.Expression
{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module Lamdu.Data.Arbitrary () where

import Control.Applicative (Applicative(..), (<$>), (<*))
import Control.Arrow (second)
import Control.Monad (join)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State (StateT, evalStateT)
import Data.Maybe (maybeToList)
import Data.Store.Guid (Guid)
import Test.QuickCheck (Arbitrary(..), Gen)
import qualified Control.Lens as Lens
import qualified Control.Lens.TH as LensTH
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State as State
import qualified Data.Store.Guid as Guid
import qualified Lamdu.Data as Data
import qualified Test.QuickCheck.Gen as Gen

data Env def = Env
  { _envScope :: [Guid]
  , __envMakeDef :: Maybe (Gen def)
  }
LensTH.makeLenses ''Env

type GenExpr def = ReaderT (Env def) (StateT [Guid] Gen)

next :: GenExpr def Guid
next = lift $ State.gets head <* State.modify tail

arbitraryLambda :: Arbitrary a => GenExpr def (Data.Lambda (Data.Expression def a))
arbitraryLambda = do
  guid <- next
  Data.Lambda guid <$> arbitraryExpr <*> (Reader.local . Lens.over envScope) (guid :) arbitraryExpr

arbitraryApply :: Arbitrary a => GenExpr def (Data.Apply (Data.Expression def a))
arbitraryApply = Data.Apply <$> arbitraryExpr <*> arbitraryExpr

arbitraryLeaf :: GenExpr def (Data.Leaf def)
arbitraryLeaf = do
  Env scope mGenDefI <- Reader.ask
  join . liftGen . Gen.elements $
    [ Data.LiteralInteger <$> liftGen arbitrary
    , pure Data.Set
    , pure Data.IntegerType
    , pure Data.Hole
    ] ++
    map (pure . Data.GetVariable . Data.ParameterRef) scope ++
    map (fmap (Data.GetVariable . Data.DefinitionRef) . liftGen)
      (maybeToList mGenDefI)

liftGen :: Gen a -> GenExpr def a
liftGen = lift . lift

arbitraryBody :: Arbitrary a => GenExpr def (Data.ExpressionBodyExpr def a)
arbitraryBody =
  join . liftGen . Gen.frequency . (map . second) pure $
  [ weight 1  $ Data.ExpressionLambda <$> arbitraryLambda
  , weight 1  $ Data.ExpressionPi     <$> arbitraryLambda
  , weight 5  $ Data.ExpressionApply  <$> arbitraryApply
  , weight 10 $ Data.ExpressionLeaf   <$> arbitraryLeaf
  ]
  where
    weight = (,)

arbitraryExpr :: Arbitrary a => GenExpr def (Data.Expression def a)
arbitraryExpr = Data.Expression <$> arbitraryBody <*> liftGen arbitrary

nameStream :: [Guid]
nameStream = map Guid.fromString names
  where
    alphabet = map (:[]) ['a'..'z']
    names = (alphabet ++) $ (++) <$> names <*> alphabet

exprGen :: Arbitrary a => Maybe (Gen def) -> Gen (Data.Expression def a)
exprGen makeDefI =
  (`evalStateT` nameStream) .
  (`runReaderT` Env [] makeDefI) $
  arbitraryExpr

-- TODO: This instance doesn't know which Definitions exist in the
-- world so avoids DefinitionRef and only has valid ParameterRefs to
-- its own lambdas.
instance Arbitrary a => Arbitrary (Data.Expression def a) where
  arbitrary = exprGen Nothing
