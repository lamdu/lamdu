{-# LANGUAGE RankNTypes #-}
module Lamdu.Data.Ops.Subexprs
    ( onMatchingSubexprs
    , onMatchingSubexprsWithPath
    , toHole
    , onGetVars
    , getVarsToHole
    ) where


import qualified Control.Lens as Lens
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Data.Ops as DataOps
import           Lamdu.Expr.IRef (ValP)
import qualified Lamdu.Expr.Lens as ExprLens
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

onMatchingSubexprs ::
    Applicative m => (a -> m ()) -> Lens.Fold (Val ()) b -> Val a -> m ()
onMatchingSubexprs action predicate =
    Lens.itraverseOf_ (ExprLens.subExprPayloads . Lens.ifiltered (\i _ -> Lens.has predicate i))
    (const action)

onMatchingSubexprsWithPath ::
    Applicative m => (a -> m ()) -> ([Val ()] -> Bool) -> Val a -> m ()
onMatchingSubexprsWithPath action predicate =
    Lens.itraverseOf_ (ExprLens.payloadsIndexedByPath . Lens.ifiltered (\i _ -> predicate i))
    (const action)

toHole :: Monad m => ValP m -> T m ()
toHole = void . DataOps.setToHole

onGetVars ::
    Monad m => (ValP m -> T m ()) -> V.Var ->
    Val (ValP m) -> T m ()
onGetVars f var =
    onMatchingSubexprs f (ExprLens.valVar . Lens.only var)

getVarsToHole :: Monad m => V.Var -> Val (ValP m) -> T m ()
getVarsToHole = onGetVars toHole
