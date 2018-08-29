module Lamdu.Calc.Val.Utils
    ( culledSubexprPayloads
    ) where

import qualified Control.Lens as Lens
import           Data.Tree.Diverse (Node(..), Ann(..))
import           Lamdu.Calc.Term (Val, termChildren)

import           Lamdu.Prelude

-- | Return all subexprs until the given cut-point
culledSubexprPayloads :: (a -> Bool) -> Val a -> [a]
culledSubexprPayloads cut =
    go
    where
        go (Node (Ann pl body))
            | cut pl = []
            | otherwise = pl : body ^. termChildren . Lens.to go
