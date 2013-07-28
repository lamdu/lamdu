module Lamdu.Data.Infer.Optimize
  ( optimizeContext
  ) where

import Control.Lens.Operators
import Control.Monad.Trans.State (StateT(..), state)
import Lamdu.Data.Infer.Internal
import qualified Control.Lens as Lens
import qualified Lamdu.Data.Infer.ExprRefs as ExprRefs

optimizeContext :: Monad m => StateT (Context def) m (RefD def -> RefD def)
optimizeContext = do
  refRename <- Lens.zoom ctxExprRefs . state $ ExprRefs.optimize (rdRefs %~)
  ctxDefTVs . Lens.mapped . tvRefs %= refRename
  _ <- error "TODO: Also fix rule refs!"
  return refRename
