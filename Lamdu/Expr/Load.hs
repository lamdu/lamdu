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
import Data.Store.IRef (Tag)
import Data.Store.Property (Property(Property))
import Data.Store.Transaction (Transaction)
import Data.Traversable (Traversable)
import GHC.Generics (Generic)
import Lamdu.Data.Definition (Definition(..))
import Lamdu.Expr.IRef (DefI, DefIM)
import Lamdu.Expr.Val (Val(..))
import qualified Control.Lens as Lens
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Val as V

type T = Transaction

type ExprI = ExprIRef.ValI

-- | SubexpressionIndex is a Foldable-index into V.Body (i.e:
-- 0 or 1 for BodyApply func/arg)
type SubexpressionIndex = Int

data ExprPropertyClosure t
  = DefinitionContentExprProperty (DefI t) (ExprI t) Definition.ExportedType
  | SubexpressionProperty (ExprI t) (V.Body (ExprI t)) SubexpressionIndex
  deriving (Show, Generic)
instance Binary (ExprPropertyClosure t)

exprPropertyOfClosure :: MonadA m => ExprPropertyClosure (Tag m) -> ExprIRef.ValIProperty m
exprPropertyOfClosure (DefinitionContentExprProperty defI bodyExpr bodyType) =
  Property bodyExpr
  (Transaction.writeIRef defI . (`Definition.Body` bodyType) . Definition.ContentExpr)
exprPropertyOfClosure (SubexpressionProperty exprI body index) =
  Property (body ^?! lens)
  (ExprIRef.writeValBody exprI . flip (lens .~) body)
  where
    lens :: Traversable t => Lens.IndexedTraversal' SubexpressionIndex (t a) a
    lens = Lens.element index

irefOfClosure :: MonadA m => ExprPropertyClosure (Tag m) -> ExprI (Tag m)
irefOfClosure = Property.value . exprPropertyOfClosure

loadExprClosure ::
  MonadA m => ExprPropertyClosure (Tag m) -> T m (Val (ExprPropertyClosure (Tag m)))
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
  MonadA m => DefIM m -> T m (Definition (Val (ExprPropertyClosure (Tag m))) (DefIM m))
loadDefinitionClosure defI = do
  Definition.Body bodyContent bodyType <- Transaction.readIRef defI
  (`Definition` defI) . (`Definition.Body` bodyType) <$>
    case bodyContent of
    Definition.ContentExpr bodyI ->
      fmap Definition.ContentExpr . loadExprClosure $
      DefinitionContentExprProperty defI bodyI bodyType
    Definition.ContentBuiltin (Definition.Builtin name) ->
      return . Definition.ContentBuiltin $ Definition.Builtin name
