{-# OPTIONS -fno-warn-orphans #-} -- Arbitrary Data.Expression
{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module Lamdu.Data.Arbitrary () where

import Control.Applicative (Applicative(..), (<$>), (<*))
import Control.Lens ((%~))
import Control.Monad (join)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State (StateT, evalStateT)
import Data.Derive.Arbitrary (makeArbitrary)
import Data.DeriveTH (derive)
import Data.Maybe (maybeToList)
import Data.Store.Guid (Guid)
import Lamdu.Data.Expr (Kind(..))
import Test.QuickCheck (Arbitrary(..), Gen, choose)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State as State
import qualified Data.Store.Guid as Guid
import qualified Lamdu.Data.Expr as Expr
import qualified Test.QuickCheck.Gen as Gen

data Env def par = Env
  { _envScope :: [par]
  , __envMakeDef :: Maybe (Gen def)
  }
Lens.makeLenses ''Env

type GenExpr def par = ReaderT (Env def par) (StateT [par] Gen)

liftGen :: Gen a -> GenExpr def par a
liftGen = lift . lift

next :: GenExpr def par par
next = lift $ State.gets head <* State.modify tail

arbitraryLambda :: Arbitrary a => GenExpr def Guid (Expr.Lam (Expr.Expr def a))
arbitraryLambda = do
  par <- next
  flip Expr.Lam par <$> liftGen arbitrary <*> arbitraryExpr <*>
    Reader.local (envScope %~ (par :)) arbitraryExpr

listOf :: GenExpr def par a -> GenExpr def par [a]
listOf gen = do
  terminate <- liftGen $ Gen.frequency [(1, return True), (2, return False)]
  if terminate
    then return []
    else (:) <$> gen <*> listOf gen

arbitraryRecord :: Arbitrary a => GenExpr def Guid (Expr.Record (Expr.Expr def a))
arbitraryRecord =
  Expr.Record
  <$> liftGen arbitrary
  <*> listOf ((,) <$> arbitraryExpr <*> arbitraryExpr)

arbitraryGetField :: Arbitrary a => GenExpr def Guid (Expr.GetField (Expr.Expr def a))
arbitraryGetField = Expr.GetField <$> arbitraryExpr <*> arbitraryExpr

arbitraryApply :: Arbitrary a => GenExpr def Guid (Expr.Apply (Expr.Expr def a))
arbitraryApply = Expr.Apply <$> arbitraryExpr <*> arbitraryExpr

arbitraryLeaf :: GenExpr def Guid (Expr.Leaf def Guid)
arbitraryLeaf = do
  Env scope mGenDefI <- Reader.ask
  join . liftGen . Gen.elements $
    [ Expr.LiteralInteger <$> liftGen arbitrary
    , pure Expr.Type
    , pure Expr.IntegerType
    , pure Expr.Hole
    ] ++
    map (pure . Expr.GetVariable . Expr.ParameterRef) scope ++
    map (fmap (Expr.GetVariable . Expr.DefinitionRef) . liftGen)
      (maybeToList mGenDefI)

arbitraryBody :: Arbitrary a => GenExpr def Guid (Expr.BodyExpr def a)
arbitraryBody =
  join . liftGen . Gen.frequency . (Lens.mapped . Lens._2 %~ pure) $
  [ weight 2  $ Expr.BodyLam      <$> arbitraryLambda
  , weight 2  $ Expr.BodyRecord   <$> arbitraryRecord
  , weight 2  $ Expr.BodyGetField <$> arbitraryGetField
  , weight 5  $ Expr.BodyApply    <$> arbitraryApply
  , weight 17 $ Expr.BodyLeaf     <$> arbitraryLeaf
  ]
  where
    weight = (,)

arbitraryExpr :: Arbitrary a => GenExpr def Guid (Expr.Expr def a)
arbitraryExpr = Expr.Expr <$> arbitraryBody <*> liftGen arbitrary

guidStream :: [Guid]
guidStream = map Guid.fromString names
  where
    alphabet = map (:[]) ['a'..'z']
    names = (alphabet ++) $ (++) <$> names <*> alphabet

exprGen :: Arbitrary a => Maybe (Gen def) -> Gen (Expr.Expr def a)
exprGen makeDefI =
  (`evalStateT` guidStream) .
  (`runReaderT` Env [] makeDefI) $
  arbitraryExpr

-- TODO: This instance doesn't know which Definitions exist in the
-- world so avoids DefinitionRef and only has valid ParameterRefs to
-- its own lambdas.
instance Arbitrary a => Arbitrary (Expr.Expr def a) where
  arbitrary = exprGen Nothing

derive makeArbitrary ''Expr.Kind
