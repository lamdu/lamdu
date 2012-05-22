module Editor.CodeEdit.DefinitionEdit(make, makeParts, addJumps) where

import Control.Arrow (second)
import Control.Monad (liftM)
import Data.List.Utils (atPred, pairList)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.IRef (IRef)
import Data.Store.Transaction (Transaction)
import Data.Vector.Vector2 (Vector2(..))
import Editor.Anchors (ViewTag)
import Editor.CTransaction (CTransaction, TWidget, transaction)
import Editor.CodeEdit.ExpressionEdit.ExpressionMaker(ExpressionEditMaker)
import Editor.MonadF (MonadF)
import Graphics.UI.Bottle.Widget (Widget)
import Graphics.UI.Bottle.Widgets.Grid (GridElement)
import qualified Data.List as List
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.ExpressionEdit.FuncEdit as FuncEdit
import qualified Editor.CodeEdit.Sugar as Sugar
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

makeNameEdit :: Monad m => Widget.Id -> Guid -> TWidget t m
makeNameEdit myId ident =
  BWidgets.wrapDelegated FocusDelegator.NotDelegating
  (BWidgets.setTextColor Config.definitionColor .
   BWidgets.makeNameEdit Config.unnamedStr ident)
  myId

makeLHSEdit
  :: MonadF m
  => ExpressionEditMaker m
  -> Widget.Id
  -> Guid
  -> [Sugar.FuncParam m]
  -> TWidget ViewTag m
makeLHSEdit makeExpressionEdit myId ident params = do
  nameEdit <- makeNameEdit myId ident
  liftM (BWidgets.gridHSpaced . List.transpose .
         map pairList . ((nameEdit, nameTypeFiller) :) . map scaleDownType) .
    mapM (FuncEdit.makeParamEdit makeExpressionEdit) $ params
  where
    scaleDownType = second $ Widget.scale Config.typeScaleFactor
    -- no type for def name (yet):
    nameTypeFiller = BWidgets.spaceWidget

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
  => ExpressionEditMaker m
  -> Widget.Id
  -> Guid
  -> Sugar.ExpressionRef m
  -> CTransaction ViewTag m [(Maybe Side, Widget (Transaction ViewTag m))]
makeParts makeExpressionEdit myId ident exprRef = do
  let
    sExpr = Sugar.rExpression exprRef
    func =
      case sExpr of
      Sugar.ExpressionFunc _ x -> x
      _ -> Sugar.Func [] exprRef
  lhsEdit <- makeLHSEdit makeExpressionEdit myId ident $ Sugar.fParams func
  equals <- BWidgets.makeLabel "=" myId
  rhsEdit <- makeExpressionEdit $ Sugar.fBody func
  return $
    zipWith (second . Widget.align . (`Vector2` 0.5)) [1, 0.5, 0.5, 0.5, 0]
    [(Just LHS, lhsEdit)
    ,(Nothing, BWidgets.spaceWidget)
    ,(Nothing, equals)
    ,(Nothing, BWidgets.spaceWidget)
    ,(Just RHS, rhsEdit)
    ]

makeBuiltinView
  :: MonadF m
  => ExpressionEditMaker m
  -> Widget.Id
  -> Sugar.Builtin m
  -> TWidget ViewTag m
makeBuiltinView makeExpressionEdit myId (Sugar.Builtin (Data.FFIName modulePath name) sType) = do
  moduleName <-
    BWidgets.setTextColor Config.foreignModuleColor $
    BWidgets.makeLabel (concatMap (++ ".") modulePath) myId
  varName <-
    BWidgets.setTextColor Config.foreignVarColor $
    BWidgets.makeLabel name myId
  colon <- BWidgets.makeLabel ":" myId
  typeEdit <- makeExpressionEdit sType
  BWidgets.makeFocusableView myId $
    BWidgets.hboxSpaced [BWidgets.hbox [moduleName, varName], colon, typeEdit]


make
  :: MonadF m
  => ExpressionEditMaker m
  -> IRef Data.Definition
  -> TWidget ViewTag m
make makeExpressionEdit defI = do
  def <- transaction $ Sugar.convertDefinition defI
  let
    ident = Sugar.drGuid def
    myId = WidgetIds.fromGuid ident
  case Sugar.drDef def of
    Sugar.DefinitionExpression sExpr -> do
      liftM
        ( Box.toWidget . (Box.atBoxContent . fmap) addJumps .
          BWidgets.hboxK
        ) $
        makeParts makeExpressionEdit myId ident sExpr
    Sugar.DefinitionBuiltin builtin ->
      makeBuiltinView makeExpressionEdit myId builtin
