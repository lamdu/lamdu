{-# LANGUAGE DeriveFunctor, DeriveDataTypeable, TemplateHaskell #-}
module Lamdu.Data.Infer.ImplicitVariables
  ( add, Payload(..)
  ) where

import Control.Lens.Operators
import Control.Monad (join, void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, evalStateT, mapStateT, state)
import Control.MonadA (MonadA)
import Data.Binary (Binary(..), getWord8, putWord8)
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Store.Guid (Guid)
import Data.Traversable (traverse)
import Data.Typeable (Typeable)
import System.Random (RandomGen, random)
import qualified Control.Lens as Lens
import qualified Data.Foldable as Foldable
import qualified Data.Store.Guid as Guid
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Infer as Infer
import qualified Lamdu.Data.Infer.Deref as Deref
import qualified Lamdu.Data.Infer.Load as Load

data Payload a = Stored a | AutoGen Guid
  deriving (Eq, Ord, Show, Functor, Typeable)
derive makeBinary ''Payload

isUnrestrictedHole :: Expr.Expression def [Deref.Restriction def] -> Bool
isUnrestrictedHole (Expr.Expression (Expr.BodyLeaf Expr.Hole) []) = True
isUnrestrictedHole _ = False

uneither :: (Show l, Monad m) => StateT s (Either l) a -> StateT s m a
uneither = mapStateT (either (error . show) return)

add ::
  (MonadA m, Show def, Eq def, RandomGen g) =>
  g -> def ->
  Expr.Expression (Load.LoadedDef def) (Infer.ScopedTypedValue def, a) ->
  StateT (Infer.Context def) m
  (Expr.Expression (Load.LoadedDef def) (Infer.ScopedTypedValue def, Payload a))
add gen def expr = do
  derefedExpr <- uneither $ Deref.expr expr
  let derefedMakers = Foldable.toList derefedExpr <&> fst
  varsAdded <-
    derefedMakers
    & traverse onEachDerefed
    & (`evalStateT` gen)
    <&> (^.. Lens.traverse . Lens._Just)
  wrap varsAdded expr
    & uneither
    <&> Lens.mapped . Lens._2 %~ eitherToPayload
  where
    wrap [] x = return $ x <&> Lens._2 %~ Right
    wrap (paramId:rest) x = do
      paramTypeRef <- Infer.freshHole emptyScope
      wrap rest x
        >>= Infer.lambdaWrap paramId paramTypeRef
        <&> Lens.mapped . Lens._2 %~ join . toEither paramId
    toEither paramId Nothing = Left $ Guid.augment "implicitLam" paramId
    toEither _ (Just x) = Right x
    emptyScope = Infer.emptyScope def
    onEachDerefed mkDerefed = do
      derefed <- lift $ uneither mkDerefed
      if isUnrestrictedHole (derefed ^. Deref.dType)
        then do
          paramId <- state random
          let
            typeRef =
              derefed ^. Deref.dScopedTypedValue . Infer.stvTV . Infer.tvType
          lift . uneither $ do
            getVarValRef <-
              (ExprLens.pureExpr . ExprLens.bodyParameterRef # paramId)
              & Infer.infer emptyScope
              <&> (^. Expr.ePayload . Lens._1 . Infer.stvTV . Infer.tvVal)
            void $ Infer.unifyRefs getVarValRef typeRef
          return $ Just paramId
        else
          return Nothing

eitherToPayload :: Either Guid a -> Payload a
eitherToPayload (Left guid) = AutoGen guid
eitherToPayload (Right x) = Stored x
