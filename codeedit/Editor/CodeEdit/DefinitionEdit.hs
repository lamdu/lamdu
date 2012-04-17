module Editor.CodeEdit.DefinitionEdit(make, makeParts, addJumps) where

import Control.Arrow (second)
import Control.Monad (liftM)
import Data.List.Utils (atPred)
import Data.Monoid (Monoid(..))
import Data.Store.IRef (IRef)
import Data.Store.Transaction (Transaction)
import Data.Vector.Vector2 (Vector2(..))
import Editor.Anchors (ViewTag)
import Editor.CTransaction (CTransaction, TWidget, transaction, getP)
import Editor.MonadF (MonadF)
import Graphics.UI.Bottle.Widget (Widget)
import Graphics.UI.Bottle.Widgets.Grid (GridElement)
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.ExpressionEdit.FuncEdit as FuncEdit
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.CodeEdit.Types as ETypes
import qualified Editor.Config as Config
import qualified Editor.Data as Data
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Direction as Direction
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator

data Side = LHS | RHS
  deriving (Show, Eq)

makeNameEdit :: Monad m => IRef a -> TWidget t m
makeNameEdit definitionI =
  BWidgets.wrapDelegated FocusDelegator.NotDelegating
  (BWidgets.setTextColor Config.definitionColor .
   BWidgets.makeNameEdit Config.unnamedStr definitionI) $
  WidgetIds.fromIRef definitionI

makeLHSEdit
  :: MonadF m
  => ETypes.ExpressionEditMaker m
  -> ETypes.ExpressionAncestry m
  -> IRef a
  -> [Sugar.FuncParam m]
  -> TWidget ViewTag m
makeLHSEdit makeExpressionEdit ancestry definitionI params = do
  nameEdit <- makeNameEdit definitionI
  paramsEdits <- mapM (FuncEdit.makeParamEdit makeExpressionEdit ancestry) params
  return $ BWidgets.hboxSpaced (nameEdit : paramsEdits)

-- from lhs->rhs and vice-versa:
addJumps
  :: [(Maybe Side, Box.BoxElement f)]
  -> [(Maybe Side, Graphics.UI.Bottle.Widgets.Grid.GridElement f)]
addJumps defKBoxElements =
  addEventMap LHS RHS "right-hand side" Config.jumpToRhsKeys Direction.fromLeft .
  addEventMap RHS LHS "left-hand side"  Config.jumpToLhsKeys Direction.fromRight $
  defKBoxElements
  where
    addEventMap srcSide destSide doc keys dir =
      atPred (== Just srcSide)
      (addJumpsTo doc keys dir $ Box.getElement (Just destSide) defKBoxElements)
    addJumpsTo doc keys dir =
      Box.atBoxElementSdwd . Widget.atSdwdEventMap . flip mappend .
      jumpToExpressionEventMap doc keys dir
    jumpToExpressionEventMap doc keys dir destElement =
      maybe mempty
      (makeJumpForEnter doc keys dir destElement) .
      Widget.sdwdMaybeEnter $ Box.boxElementSdwd destElement
    makeJumpForEnter doc keys dir destElement enter =
      E.fromEventTypes keys ("Jump to "++doc) .
      Widget.enterResultEvent . enter . dir $
      Box.boxElementRect destElement

makeParts
  :: MonadF m
  => ETypes.ExpressionEditMaker m
  -> ETypes.ExpressionAncestry m
  -> IRef a
  -> ETypes.ExpressionPtr m
  -> CTransaction ViewTag m [(Maybe Side, Widget (Transaction ViewTag m))]
makeParts makeExpressionEdit ancestry definitionI exprPtr = do
  exprI <- getP exprPtr
  sExpr <- transaction $ Sugar.getExpression exprPtr
  let
    func =
      case sExpr of
      Sugar.ExpressionFunc x -> x
      _ -> Sugar.Func [] exprPtr
  lhsEdit <- makeLHSEdit makeExpressionEdit ancestry definitionI $ Sugar.fParams func
  equals <- BWidgets.makeLabel "=" (WidgetIds.fromIRef definitionI)
  rhsEdit <-
    makeExpressionEdit
    (ETypes.AncestryItemLambda (ETypes.LambdaParent func exprI) : ancestry)
    (Sugar.fBody func)
  return $
    zipWith (second . Widget.align . (`Vector2` 0.5)) [1, 0.5, 0]
    [(Just LHS, lhsEdit),
     (Nothing, equals),
     (Just RHS, rhsEdit)]

make
  :: MonadF m
  => ETypes.ExpressionEditMaker m
  -> IRef Data.Definition
  -> TWidget ViewTag m
make makeExpressionEdit definitionI =
  liftM
  ( Box.toWidget . (Box.atBoxContent . fmap) addJumps .
    BWidgets.hboxSpacedK Nothing
  ) $
  makeParts makeExpressionEdit [] definitionI exprPtr
  where
    exprPtr =
      Property.composeLabel Data.defBody Data.atDefBody
      (Transaction.fromIRef definitionI)
