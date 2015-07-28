module InferWrappers where

import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad.Trans.State (evalStateT)
import           Data.Functor.Identity (Identity(..))
import qualified Data.Map as Map
import           DefinitionTypes
import           Lamdu.Expr.Val (Val)
import qualified Lamdu.Expr.Val as V
import           Lamdu.Infer (Infer)
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Infer.Error as InferErr
import           Lamdu.Infer.Load (loadInfer, Loader(..))
import           Lamdu.Infer.Unify (unify)

infer :: Infer.Scope -> Val a -> Infer (Val (Infer.Payload, a))
infer = Infer.infer definitionTypes

runNewContext :: Infer a -> Either InferErr.Error a
runNewContext = (`evalStateT` Infer.initialContext) . Infer.run

{-# INLINE loader #-}
loader :: Monad m => Loader m
loader =
    Loader
    { loadTypeOf = return . (Infer.loadedGlobalTypes definitionTypes Map.!)
    , loadNominal = return . (Infer.loadedNominals definitionTypes Map.!)
    }

loadInferScope :: Infer.Scope -> Val a -> Infer (Val (Infer.Payload, a))
loadInferScope scope = runIdentity . loadInfer loader scope

-- WARNING: Returned inferred val requires update
loadInferInto :: Infer.Payload -> Val a -> Infer (Val (Infer.Payload, a))
loadInferInto pl val = do
    inferredVal <- loadInferScope (pl ^. Infer.plScope) val
    let inferredType = inferredVal ^. V.payload . _1 . Infer.plType
    unify inferredType (pl ^. Infer.plType)
    return inferredVal

loadInferDef :: Val a -> Infer (Val (Infer.Payload, a))
loadInferDef = loadInferScope Infer.emptyScope
