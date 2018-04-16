module Lamdu.Calc.Val.Utils
    ( culledSubexprPayloads
    ) where

import qualified Control.Lens as Lens
import           Lamdu.Calc.Val.Annotated (Val(Val))

import           Lamdu.Prelude

-- | Return all subexprs until the given cut-point
culledSubexprPayloads :: (a -> Bool) -> Val a -> [a]
culledSubexprPayloads cut =
    go
    where
        go (Val pl body)
            | cut pl = []
            | otherwise = body ^. Lens.folded . Lens.to go
