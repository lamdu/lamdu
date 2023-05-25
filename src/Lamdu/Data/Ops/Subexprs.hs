{-# LANGUAGE GADTs #-}
module Lamdu.Data.Ops.Subexprs
    ( onMatchingSubexprs
    , toHole
    , onGetVars
    , getVarsToHole
    ) where


import qualified Control.Lens as Lens
import           Data.Monoid (Any)
import           Hyper
import           Hyper.Recurse (unwrap)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Data.Ops as DataOps
import           Lamdu.Expr.IRef (HRef)
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

onMatchingSubexprs ::
    Applicative m =>
    (a # V.Term -> m ()) ->
    Lens.Getting Any (Pure # V.Term) b ->
    Ann a # V.Term ->
    m ()
onMatchingSubexprs action predicate x =
    when (Lens.has predicate (unwrap (const (^. hVal)) x)) (action (x ^. hAnn))
    *>
    htraverse_
    ( \case
        HWitness V.W_Term_Term -> onMatchingSubexprs action predicate
        _ -> const (pure ())
    ) (x ^. hVal)

toHole :: Monad m => HRef m # V.Term -> T m ()
toHole = void . DataOps.setToHole

onGetVars ::
    Monad m =>
    (HRef m # V.Term -> T m ()) -> V.Var -> Ann (HRef m) # V.Term -> T m ()
onGetVars f var = onMatchingSubexprs f (_Pure . V._BLeaf . V._LVar . Lens.only var)

getVarsToHole :: Monad m => V.Var -> Ann (HRef m) # V.Term -> T m ()
getVarsToHole = onGetVars toHole
