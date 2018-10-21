module Lamdu.Calc.Term.Utils
    ( culledSubexprPayloads
    ) where

import qualified Control.Lens as Lens
import           Data.Tree.Diverse (Ann(..))
import           Lamdu.Calc.Term (Val, termChildren)

import           Lamdu.Prelude

-- | Return all subexprs until the given cut-point
culledSubexprPayloads :: (a -> Bool) -> Val a -> [a]
culledSubexprPayloads cut =
    go
    where
        go (Ann pl body)
            | cut pl = []
            | otherwise = pl : body ^. termChildren . Lens.to go
