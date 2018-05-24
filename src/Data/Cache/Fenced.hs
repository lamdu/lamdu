-- | A caching mechanism with explicit life-time annotations called
-- "fences" that are used to evict all objects that weren't used since
-- the previous "fence"
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Cache.Fenced
    ( Decl, function
    , Cache, make, fence
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Writer (WriterT(..))
import qualified Control.Monad.Writer as Writer
import           Data.IORef
import qualified Data.Map as Map
import           System.IO.Unsafe (unsafePerformIO)

import           Lamdu.Prelude

newtype Cache = Cache
    { fence :: IO ()
    }

newtype Decl a = Decl (WriterT (IO ()) IO a)
    deriving (Functor, Applicative)

function :: Ord a => (a -> b) -> Decl (a -> b)
function f =
    do
        cache <- newIORef Map.empty & lift
        nextCache <- newIORef Map.empty & lift
        do
            readIORef nextCache >>= writeIORef cache
            writeIORef nextCache Map.empty
            & Writer.tell
        let memo k =
                readIORef cache <&> (^. Lens.at k)
                >>= \case
                Just v -> v <$ addToCache v nextCache
                Nothing ->
                    v <$ traverse_ (addToCache v) [cache, nextCache]
                    where
                        v = f k
                & unsafePerformIO
                where
                    addToCache v ref = modifyIORef ref (Lens.at k ?~ v)
        pure memo
    & Decl

make :: Decl a -> IO (Cache, a)
make (Decl act) =
    runWriterT act
    <&> \(x, declFences) -> (Cache declFences, x)
