{-# LANGUAGE TypeFamilies, FlexibleContexts, DeriveFunctor, TemplateHaskell, DeriveDataTypeable #-}
module Lamdu.Data.Expression.Load
  ( loadDefinitionClosure
  , ExprPropertyClosure, exprPropertyOfClosure
  ) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Binary (Binary(..), getWord8, putWord8)
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Function.Decycle (decycleOn)
import Data.Store.IRef (Tag)
import Data.Store.Property (Property(Property))
import Data.Store.Transaction (Transaction)
import Data.Traversable (Traversable)
import Data.Typeable (Typeable)
import Lamdu.Data.Definition (Definition(..))
import Lamdu.Data.Expression.IRef (DefI, DefIM)
import qualified Control.Lens as Lens
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.IRef as ExprIRef

type T = Transaction

type ExprI = ExprIRef.ExpressionI

-- | SubexpressionIndex is a Foldable-index into Expr.Body (i.e:
-- 0 or 1 for BodyApply func/arg)
type SubexpressionIndex = Int

data ExprPropertyClosure t
  = DefinitionTypeProperty (DefI t) (Definition.Body (ExprI t))
  | DefinitionContentExpressionProperty (DefI t) (ExprI t) (ExprI t)
  | SubexpressionProperty (ExprI t) (Expr.Body (DefI t) (ExprI t)) SubexpressionIndex
  deriving (Eq, Ord, Show, Typeable)
derive makeBinary ''ExprPropertyClosure

exprPropertyOfClosure :: MonadA m => ExprPropertyClosure (Tag m) -> ExprIRef.ExpressionProperty m
exprPropertyOfClosure (DefinitionTypeProperty defI (Definition.Body bodyContent bodyType)) =
  Property bodyType (Transaction.writeIRef defI . Definition.Body bodyContent)
exprPropertyOfClosure (DefinitionContentExpressionProperty defI bodyExpr bodyType) =
  Property bodyExpr
  (Transaction.writeIRef defI . (`Definition.Body` bodyType) . Definition.ContentExpression)
exprPropertyOfClosure (SubexpressionProperty exprI body index) =
  Property (body ^?! lens)
  (ExprIRef.writeExprBody exprI . flip (lens .~) body)
  where
    lens :: Traversable t => Lens.IndexedTraversal' SubexpressionIndex (t a) a
    lens = Lens.element index

irefOfClosure :: MonadA m => ExprPropertyClosure (Tag m) -> ExprI (Tag m)
irefOfClosure = Property.value . exprPropertyOfClosure

loadExpressionClosure ::
  MonadA m => ExprPropertyClosure (Tag m) ->
  T m (ExprIRef.ExpressionM m (ExprPropertyClosure (Tag m)))
loadExpressionClosure =
  decycleOn irefOfClosure loop
  where
    loop Nothing closure =
      error $ "Recursive IRef structure: " ++ show (irefOfClosure closure)
    loop (Just recurse) closure =
      ExprIRef.readExprBody iref
      >>= onBody
      <&> (`Expr.Expression` closure)
      where
        onBody body = body & Lens.traversed %%@~ loadElement body
        iref = irefOfClosure closure
        loadElement body i _ = recurse $ SubexpressionProperty iref body i

-- TODO: Return DefinitionClosure
loadDefinitionClosure ::
  MonadA m => DefIM m ->
  T m (Definition (ExprIRef.ExpressionM m (ExprPropertyClosure (Tag m))) (DefIM m))
loadDefinitionClosure defI = do
  def <- Transaction.readIRef defI
  bodyType <- loadExpressionClosure $ DefinitionTypeProperty defI def
  (`Definition` defI) . (`Definition.Body` bodyType) <$>
    case def ^. Definition.bodyContent of
    Definition.ContentExpression bodyI ->
      fmap Definition.ContentExpression . loadExpressionClosure $
      DefinitionContentExpressionProperty defI bodyI $ def ^. Definition.bodyType
    Definition.ContentBuiltin (Definition.Builtin name) ->
      return . Definition.ContentBuiltin $ Definition.Builtin name
