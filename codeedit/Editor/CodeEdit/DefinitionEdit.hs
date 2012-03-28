{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.DefinitionEdit(make, makeParts) where

import Control.Arrow (second)
import Control.Monad (liftM)
import Data.List.Utils (atPred)
import Data.Monoid (Monoid(..))
import Data.Store.IRef (IRef)
import Data.Store.Transaction (Transaction)
import Data.Vector.Vector2 (Vector2(..))
import Editor.Anchors (ViewTag)
import Editor.CTransaction (CTransaction, TWidget, assignCursor, transaction)
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

makeNameEdit
  :: Monad m => Widget.Id -> IRef a -> TWidget t m
makeNameEdit myId definitionI =
  assignCursor myId (WidgetIds.delegating nameEditAnimId) .
  BWidgets.wrapDelegated FocusDelegator.NotDelegating
  (BWidgets.setTextColor Config.definitionColor .
   BWidgets.makeNameEdit Config.unnamedStr definitionI) $
  nameEditAnimId
  where
    nameEditAnimId = Widget.joinId myId ["name"]

makeLHSEdit
  :: MonadF m
  => Widget.Id
  -> IRef a
  -> [Sugar.FuncParam m]
  -> TWidget ViewTag m
makeLHSEdit myId definitionI params = do
  nameEdit <- makeNameEdit myId definitionI
  paramsEdits <- mapM FuncEdit.makeParamEdit params
  return $ BWidgets.hboxSpaced (nameEdit : paramsEdits)

-- from lhs->rhs and vice-versa:
addJumps
  :: (Show key, Eq key)
  => key -> key
  -> [(key, Box.BoxElement f)]
  -> [(key, Graphics.UI.Bottle.Widgets.Grid.GridElement f)]
addJumps lhs rhs defKBoxElements =
  addEventMap lhs rhs "right-hand side" Config.jumpToRhsKeys Direction.fromLeft .
  addEventMap rhs lhs "left-hand side" Config.jumpToLhsKeys Direction.fromRight $
  defKBoxElements
  where
    addEventMap srcSide destSide doc keys dir =
      atPred (== srcSide)
      (addJumpsTo doc keys dir $ Box.getElement destSide defKBoxElements)
    addJumpsTo doc keys dir =
      Box.atBoxElementUio . Widget.atUioEventMap . mappend .
      jumpToExpressionEventMap doc keys dir
    jumpToExpressionEventMap doc keys dir destElement =
      maybe mempty
      (makeJumpForEnter doc keys dir destElement) .
      Widget.uioMaybeEnter $ Box.boxElementUio destElement
    makeJumpForEnter doc keys dir destElement enter =
      E.fromEventTypes keys ("Jump to "++doc) .
      Widget.enterResultEvent . enter . dir $
      Box.boxElementRect destElement

make
  :: MonadF m
  => ETypes.ExpressionEditMaker m
  -> IRef Data.Definition
  -> TWidget ViewTag m
make makeExpressionEdit definitionI =
  liftM
  ( Box.toWidget . (Box.atBoxContent . fmap) (addJumps "lhs" "rhs") .
    BWidgets.hboxSpacedK ("space" :: String)
  ) $
  makeParts makeExpressionEdit [] definitionI exprPtr
  where
    exprPtr =
      Property.composeLabel Data.defBody Data.atDefBody
      (Transaction.fromIRef definitionI)

makeParts
  :: MonadF m
  => ETypes.ExpressionEditMaker m
  -> ETypes.ExpressionAncestry m
  -> IRef a
  -> ETypes.ExpressionPtr m
  -> CTransaction ViewTag m [(String, Widget (Transaction ViewTag m))]
makeParts makeExpressionEdit ancestry definitionI exprPtr = do
  sExpr <- transaction $ Sugar.getExpression exprPtr
  let
    func =
      case sExpr of
      Sugar.ExpressionFunc x -> x
      _ -> Sugar.Func [] exprPtr
  lhsEdit <- makeLHSEdit myId definitionI $ Sugar.fParams func
  equals <- BWidgets.makeLabel "=" myId
  rhsEdit <-
    makeExpressionEdit
    (ETypes.AncestryItemLambda (ETypes.LambdaParent func exprPtr) : ancestry)
    (Sugar.fBody func)
  return $
    zipWith (second . Widget.align . (`Vector2` 0.5)) [1, 0.5, 0]
    [("lhs", lhsEdit),
     ("equals", equals),
     ("rhs", rhsEdit)]
  where
    myId = WidgetIds.fromIRef definitionI
