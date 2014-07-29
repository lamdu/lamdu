{-# OPTIONS -fno-warn-orphans #-} -- Arbitrary E.Val
{-# LANGUAGE FlexibleInstances #-}
module Lamdu.Data.Arbitrary (Name(..)) where

import Control.Applicative (Applicative(..), (<$>), (<*))
import Control.Lens (Lens', (%~))
import Control.Monad (replicateM, join)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State (StateT, evalStateT)
import Data.Map (Map)
import Data.String (IsString(..))
import Lamdu.Expr.Scheme (Scheme)
import Test.QuickCheck (Arbitrary(..), Gen)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State as State
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Lamdu.Expr as E
import qualified Test.QuickCheck.Gen as Gen

data Env = Env
  { _envScope :: [E.ValVar]
  , __envGlobals :: Map E.GlobalId Scheme
  }
envScope :: Lens' Env [E.ValVar]
envScope f e = mkEnv <$> f (_envScope e)
  where
    mkEnv x = e { _envScope = x }

type GenExpr = ReaderT (Env) (StateT [E.ValVar] Gen)

liftGen :: Gen a -> GenExpr a
liftGen = lift . lift

next :: GenExpr E.ValVar
next = lift $ State.gets head <* State.modify tail

arbitraryLam :: Arbitrary a => GenExpr (E.Lam (E.Val a))
arbitraryLam = do
  par <- next
  E.Lam par <$> Reader.local (envScope %~ (par :)) arbitraryExpr

arbitraryRecExtend :: Arbitrary a => GenExpr (E.RecExtend (E.Val a))
arbitraryRecExtend =
  E.RecExtend <$> liftGen arbitrary <*> arbitraryExpr <*> arbitraryExpr

arbitraryGetField :: Arbitrary a => GenExpr (E.GetField (E.Val a))
arbitraryGetField = E.GetField <$> arbitraryExpr <*> liftGen arbitrary

arbitraryApply :: Arbitrary a => GenExpr (E.Apply (E.Val a))
arbitraryApply = E.Apply <$> arbitraryExpr <*> arbitraryExpr

arbitraryLeaf :: GenExpr E.ValLeaf
arbitraryLeaf = do
  Env locals globals <- Reader.ask
  join . liftGen . Gen.elements $
    [ E.VLiteralInteger <$> liftGen arbitrary
    , pure E.VHole
    , pure E.VRecEmpty
    ] ++
    map (pure . E.VVar) locals ++
    map (pure . E.VGlobal) (Map.keys globals)

arbitraryBody :: Arbitrary a => GenExpr (E.ValBody (E.Val a))
arbitraryBody =
  join . liftGen . Gen.frequency . (Lens.mapped . Lens._2 %~ pure) $
  [ weight 2  $ E.VAbs         <$> arbitraryLam
  , weight 2  $ E.VRecExtend   <$> arbitraryRecExtend
  , weight 2  $ E.VGetField    <$> arbitraryGetField
  , weight 5  $ E.VApp         <$> arbitraryApply
  , weight 17 $ E.VLeaf        <$> arbitraryLeaf
  ]
  where
    weight = (,)

arbitraryExpr :: Arbitrary a => GenExpr (E.Val a)
arbitraryExpr = E.Val <$> liftGen arbitrary <*> arbitraryBody

class Name n where
  names :: [n]

exprGen :: Arbitrary a => Map E.GlobalId Scheme -> Gen (E.Val a)
exprGen globals =
  (`evalStateT` names) .
  (`runReaderT` Env [] globals) $
  arbitraryExpr

instance Name E.ValVar where
  names = fromString . (: []) <$> ['a'..]

instance Arbitrary E.Identifier where
  arbitrary = E.Identifier . BS.pack <$> replicateM 8 arbitrary

instance Arbitrary E.Tag where
  arbitrary = E.Tag <$> arbitrary

-- TODO: This instance doesn't know which Definitions exist in the
-- world so avoids DefinitionRef and only has valid ParameterRefs to
-- its own lambdas.
instance Arbitrary a => Arbitrary (E.Val a) where
  arbitrary = exprGen Map.empty
