{-# LANGUAGE RankNTypes #-}
module Lamdu.Data.Ops.Subexprs
    ( onMatchingSubexprs
    , toHole
    , onGetVars
    , getVarsToHole
    ) where


import qualified Control.Lens as Lens
import           Hyper (Tree, Pure, _Pure)
import qualified Lamdu.Calc.Lens as ExprLens
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Data.Ops as DataOps
import           Lamdu.Expr.IRef (ValP)
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

onMatchingSubexprs ::
    Applicative m => (a -> m ()) -> Lens.Fold (Tree Pure V.Term) b -> Val a -> m ()
onMatchingSubexprs action predicate =
    Lens.itraverseOf_ (ExprLens.subExprPayloads . Lens.ifiltered (\i _ -> Lens.has predicate i))
    (const action)

toHole :: Monad m => ValP m -> T m ()
toHole = void . DataOps.setToHole

onGetVars ::
    Monad m => (ValP m -> T m ()) -> V.Var ->
    Val (ValP m) -> T m ()
onGetVars f var =
    onMatchingSubexprs f (_Pure . V._BLeaf . V._LVar . Lens.only var)

getVarsToHole :: Monad m => V.Var -> Val (ValP m) -> T m ()
getVarsToHole = onGetVars toHole
