{-# OPTIONS -O2 -Wall #-}
module Editor.CodeEdit.NextArg(makeAddArgHandler)
where

import Data.Store.IRef (IRef)
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import Editor.MonadF (MonadF)
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Editor.CodeEdit.Ancestry as A
import qualified Editor.Data as Data
import qualified Editor.DataOps as DataOps
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget

-- Return the target function to add "next arg" for
addNextArgTargetExpression
  :: MonadF m
  => A.ExpressionAncestry m
  -> Transaction.Property ViewTag m (IRef Data.Expression)
  -> Transaction.Property ViewTag m (IRef Data.Expression)
addNextArgTargetExpression (A.AncestryItemApply (A.ApplyParent A.ApplyArg A.Prefix _ parentPtr) : _) _ = parentPtr
addNextArgTargetExpression (A.AncestryItemApply (A.ApplyParent A.ApplyFunc A.InfixLeft _ parentPtr) : _) _ = parentPtr
addNextArgTargetExpression _ expressionPtr = expressionPtr

makeAddArgHandler
  :: MonadF m
  => A.ExpressionAncestry m
  -> Transaction.Property ViewTag m (IRef Data.Expression)
  -> Transaction ViewTag m (String, Transaction ViewTag m Widget.Id)
makeAddArgHandler ancestry expressionPtr =
  case getNextArg ancestry of
    Nothing -> return addArg
    Just holeCandidateI -> do
      holeCandidate <- Property.get $ Transaction.fromIRef holeCandidateI
      return $ case holeCandidate of
        Data.ExpressionHole _ -> ("Move to next arg", return (WidgetIds.fromIRef holeCandidateI))
        _ -> addArg
  where
    addArg =
      ("Add next arg",
       WidgetIds.diveIn . DataOps.callWithArg $ addNextArgTargetExpression ancestry expressionPtr)

getNextArg :: A.ExpressionAncestry m -> Maybe (IRef Data.Expression)
getNextArg (
  A.AncestryItemApply (A.ApplyParent A.ApplyFunc _ apply _) :
  _) = Just $ Data.applyArg apply
getNextArg (
  A.AncestryItemApply (A.ApplyParent A.ApplyArg A.Prefix _ _) :
  A.AncestryItemApply (A.ApplyParent A.ApplyFunc _ parentApply _) :
  _) = Just $ Data.applyArg parentApply
getNextArg _ = Nothing
