module Lamdu.Eval.ToExpr
    ( fromValHead
    ) where

import Control.Lens.Operators
import Lamdu.Eval
import Lamdu.Eval.Val
import Lamdu.Expr.Val (Val(..))
import qualified Control.Lens as Lens
import qualified Lamdu.Expr.Pure as P
import qualified Lamdu.Expr.Val as V

fromValHead :: Monad m => ValHead pl -> EvalT pl m (Val ())
fromValHead HRecEmpty = return P.recEmpty
fromValHead (HRecExtend recExtend) =
    Lens.traverse fromThunkId recExtend <&> Val () . V.BRecExtend
fromValHead (HLiteralInteger i) = P.litInt i & return
-- Not converting functions to exprs for now
fromValHead (HBuiltin _) = return P.hole
fromValHead (HFunc _) = return P.hole

fromThunkId :: Monad m => ThunkId -> EvalT pl m (Val ())
fromThunkId t = whnfThunk t >>= fromValHead
