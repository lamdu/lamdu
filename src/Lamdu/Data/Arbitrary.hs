{-# OPTIONS -fno-warn-orphans #-} -- Arbitrary Data.Expression
{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module Lamdu.Data.Arbitrary () where

import Control.Applicative (Applicative(..), (<$>), (<*))
import Control.Lens ((%~))
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
import qualified Lamdu.Data.Expression as Expression
import qualified Test.QuickCheck.Gen as Gen

data Env def = Env
  { _envScope :: [Guid]
  , __envMakeDef :: Maybe (Gen def)
  }
LensTH.makeLenses ''Env

type GenExpr def = ReaderT (Env def) (StateT [Guid] Gen)

next :: GenExpr def Guid
next = lift $ State.gets head <* State.modify tail

arbitraryLambda :: Arbitrary a => GenExpr def (Expression.Lambda (Expression.Expression def a))
arbitraryLambda = do
  guid <- next
  Expression.Lambda guid <$> arbitraryExpr <*>
    Reader.local (envScope %~ (guid :)) arbitraryExpr

arbitraryApply :: Arbitrary a => GenExpr def (Expression.Apply (Expression.Expression def a))
arbitraryApply = Expression.Apply <$> arbitraryExpr <*> arbitraryExpr

arbitraryLeaf :: GenExpr def (Expression.Leaf def)
arbitraryLeaf = do
  Env scope mGenDefI <- Reader.ask
  join . liftGen . Gen.elements $
    [ Expression.LiteralInteger <$> liftGen arbitrary
    , pure Expression.Set
    , pure Expression.IntegerType
    , pure Expression.Hole
    ] ++
    map (pure . Expression.GetVariable . Expression.ParameterRef) scope ++
    map (fmap (Expression.GetVariable . Expression.DefinitionRef) . liftGen)
      (maybeToList mGenDefI)

liftGen :: Gen a -> GenExpr def a
liftGen = lift . lift

arbitraryBody :: Arbitrary a => GenExpr def (Expression.BodyExpr def a)
arbitraryBody =
  join . liftGen . Gen.frequency . (Lens.mapped . Lens._2 %~ pure) $
  [ weight 1  $ Expression.BodyLambda <$> arbitraryLambda
  , weight 1  $ Expression.BodyPi     <$> arbitraryLambda
  , weight 5  $ Expression.BodyApply  <$> arbitraryApply
  , weight 10 $ Expression.BodyLeaf   <$> arbitraryLeaf
  ]
  where
    weight = (,)

arbitraryExpr :: Arbitrary a => GenExpr def (Expression.Expression def a)
arbitraryExpr = Expression.Expression <$> arbitraryBody <*> liftGen arbitrary

nameStream :: [Guid]
nameStream = map Guid.fromString names
  where
    alphabet = map (:[]) ['a'..'z']
    names = (alphabet ++) $ (++) <$> names <*> alphabet

exprGen :: Arbitrary a => Maybe (Gen def) -> Gen (Expression.Expression def a)
exprGen makeDefI =
  (`evalStateT` nameStream) .
  (`runReaderT` Env [] makeDefI) $
  arbitraryExpr

-- TODO: This instance doesn't know which Definitions exist in the
-- world so avoids DefinitionRef and only has valid ParameterRefs to
-- its own lambdas.
instance Arbitrary a => Arbitrary (Expression.Expression def a) where
  arbitrary = exprGen Nothing
