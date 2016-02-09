{-# LANGUAGE PolymorphicComponents #-}
-- | Compile Lamdu vals to Javascript

module Lamdu.Compiler.Javascript
    ( Actions(..), M, run
    , compileValI
    ) where

import           Control.Lens.Operators
import           Control.Monad (void)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Reader (ReaderT(..))
import qualified Control.Monad.Trans.Reader as Reader
import           Control.MonadA (MonadA)
import           Data.Store.Transaction (Transaction)
import           Lamdu.Expr.IRef (ValI(..))
import qualified Lamdu.Expr.IRef as ExprIRef
import           Lamdu.Expr.Val (Val)
import           Text.PrettyPrint.HughesPJClass (pPrint)

type T = Transaction

newtype Actions m = Actions
    { runTransaction :: forall a. T m a -> IO a
    }

type M m a = ReaderT (Actions m) IO a

run :: Actions m -> M m a -> IO a
run actions = (`runReaderT` actions)

trans :: T m b -> M m b
trans act =
    do
        runTrans <- Reader.asks runTransaction
        runTrans act & lift

-- | Compile a given val and all the transitively used definitions
-- (FIXME: currently, compilation goes to stdout)
compileValI :: MonadA m => ValI m -> M m ()
compileValI valI =
    ExprIRef.readVal valI & trans
    >>= compileVal

compileVal :: Val (ValI m) -> M m ()
compileVal val =
    liftIO $ putStrLn $ "Compiling " ++ show (pPrint (void val))
