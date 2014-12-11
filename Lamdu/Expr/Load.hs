{-# LANGUAGE TypeFamilies, FlexibleContexts, DeriveGeneric #-}
module Lamdu.Expr.Load
  ( loadDefinitionClosure
  , ExprPropertyClosure, exprPropertyOfClosure
  ) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Binary (Binary)
import Data.Function.Decycle (decycleOn)
import Data.Store.Property (Property(Property))
import Data.Store.Transaction (Transaction)
import Data.Traversable (Traversable)
import GHC.Generics (Generic)
import Lamdu.Data.Definition (Definition(..))
import Lamdu.Expr.IRef (DefI, DefI)
import Lamdu.Expr.Val (Val(..))
import qualified Control.Lens as Lens
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Val as V

type T = Transaction

-- | SubexpressionIndex is a Foldable-index into V.Body (i.e:
-- 0 or 1 for BodyApply func/arg)
type SubexpressionIndex = Int

data ExprPropertyClosure m
  = DefinitionExprProperty (DefI m) (ExprIRef.ValI m) Definition.ExportedType
  | SubexpressionProperty (ExprIRef.ValI m) (V.Body (ExprIRef.ValI m)) SubexpressionIndex
  deriving (Show, Generic)
instance Binary (ExprPropertyClosure m)

exprPropertyOfClosure :: MonadA m => ExprPropertyClosure m -> ExprIRef.ValIProperty m
exprPropertyOfClosure (DefinitionExprProperty defI bodyI bodyType) =
  Property bodyI
  (Transaction.writeIRef defI . Definition.BodyExpr . (`Definition.Expr` bodyType))
exprPropertyOfClosure (SubexpressionProperty exprI body index) =
  Property (body ^?! lens)
  (ExprIRef.writeValBody exprI . flip (lens .~) body)
  where
    lens :: Traversable t => Lens.IndexedTraversal' SubexpressionIndex (t a) a
    lens = Lens.element index

irefOfClosure :: MonadA m => ExprPropertyClosure m -> ExprIRef.ValI m
irefOfClosure = Property.value . exprPropertyOfClosure

loadExprClosure ::
  MonadA m => ExprPropertyClosure m -> T m (Val (ExprPropertyClosure m))
loadExprClosure =
  decycleOn irefOfClosure loop
  where
    loop Nothing closure =
      error $ "Recursive IRef structure: " ++ show (irefOfClosure closure)
    loop (Just recurse) closure =
      ExprIRef.readValBody iref
      >>= onBody
      <&> Val closure
      where
        onBody body = body & Lens.traversed %%@~ loadElement body
        iref = irefOfClosure closure
        loadElement body i _ = recurse $ SubexpressionProperty iref body i

-- TODO: Return DefinitionClosure
loadDefinitionClosure ::
  MonadA m => DefI m -> T m (Definition (Val (ExprPropertyClosure m)) (DefI m))
loadDefinitionClosure defI = do
  body <- Transaction.readIRef defI
  (`Definition` defI) <$>
    case body of
    Definition.BodyExpr (Definition.Expr bodyI exportedType) ->
      fmap (Definition.BodyExpr . (`Definition.Expr` exportedType)) .
      loadExprClosure $ DefinitionExprProperty defI bodyI exportedType
    Definition.BodyBuiltin (Definition.Builtin name typ) ->
      return $ Definition.BodyBuiltin (Definition.Builtin name typ)
