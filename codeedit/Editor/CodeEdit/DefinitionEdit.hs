module Editor.CodeEdit.DefinitionEdit(make, makeParts, addJumps) where

import Control.Arrow (second)
import Control.Monad (liftM)
import Data.List.Split (splitOn)
import Data.List.Utils (atPred, pairList)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.Property (Property(..))
import Data.Store.Transaction (Transaction)
import Data.Vector.Vector2 (Vector2(..))
import Editor.Anchors (ViewTag)
import Editor.CTransaction (CTransaction, TWidget, assignCursor, readCursor)
import Editor.CodeEdit.ExpressionEdit.ExpressionMaker(ExpressionEditMaker)
import Editor.MonadF (MonadF)
import Graphics.UI.Bottle.Widget (Widget)
import Graphics.UI.Bottle.Widgets.Grid (GridElement)
import qualified Data.List as List
import qualified Editor.Anchors as Anchors
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.ExpressionEdit.FuncEdit as FuncEdit
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.Config as Config
import qualified Editor.Data as Data
import qualified Editor.Data.Ops as DataOps
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Direction as Direction
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator

data Side = LHS | RHS
  deriving (Show, Eq)

makeNameEdit :: MonadF m => Widget.Id -> Guid -> TWidget t m
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
  -> Maybe (Transaction ViewTag m Guid)
  -> [Sugar.FuncParam m]
  -> TWidget ViewTag m
makeLHSEdit makeExpressionEdit myId ident mAddFirstParameter params = do
  nameEdit <-
    (liftM . Widget.weakerEvents) addFirstParamEventMap $
    makeNameEdit myId ident
  liftM (BWidgets.gridHSpaced . List.transpose .
         map pairList . ((nameEdit, nameTypeFiller) :) . map scaleDownType) .
    mapM (FuncEdit.makeParamEdit makeExpressionEdit) $ params
  where
    addFirstParamEventMap =
      maybe mempty
      (Widget.actionEventMapMovesCursor Config.addNextParamKeys
       "Add parameter" .
       liftM (FocusDelegator.delegatingId . WidgetIds.paramId))
      mAddFirstParameter
    scaleDownType = second $ Widget.scale Config.typeScaleFactor
    -- no type for def name (yet):
    nameTypeFiller = BWidgets.spaceWidget

-- from lhs->rhs and vice-versa:
addJumps
  :: Monad m
  => Widget.Id
  -> [(Maybe Side, Box.BoxElement (Transaction ViewTag m))]
  -> [(Maybe Side, Graphics.UI.Bottle.Widgets.Grid.GridElement (Transaction ViewTag m))]
addJumps cursor defKBoxElements =
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
      (Anchors.savePreJumpPosition cursor >>) .
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
  lhsEdit <-
    makeLHSEdit makeExpressionEdit myId ident
    (Sugar.lambdaWrap (Sugar.rActions exprRef)) (Sugar.fParams func)
  equals <- BWidgets.makeLabel "=" $ Widget.toAnimId myId
  rhsEdit <- makeExpressionEdit $ Sugar.fBody func
  return $
    zipWith (second . Widget.align . (`Vector2` 0.5)) [1, 0.5, 0.5, 0.5, 0]
    [(Just LHS, lhsEdit)
    ,(Nothing, BWidgets.spaceWidget)
    ,(Nothing, equals)
    ,(Nothing, BWidgets.spaceWidget)
    ,(Just RHS, rhsEdit)
    ]

makeBuiltinEdit
  :: MonadF m
  => ExpressionEditMaker m
  -> Widget.Id
  -> Sugar.Builtin m
  -> TWidget ViewTag m
makeBuiltinEdit makeExpressionEdit myId (Sugar.Builtin (Data.FFIName modulePath name) setFFIName sType) =
  assignCursor myId (WidgetIds.builtinFFIName myId) $ do
    moduleName <- makeNamePartEditor Config.foreignModuleColor modulePathStr modulePathSetter WidgetIds.builtinFFIPath
    varName <- makeNamePartEditor Config.foreignVarColor name nameSetter WidgetIds.builtinFFIName
    dot <- BWidgets.makeLabel "." $ Widget.toAnimId myId
    colon <- BWidgets.makeLabel ":" $ Widget.toAnimId myId
    typeEdit <- makeExpressionEdit sType
    return $ BWidgets.hboxSpaced [BWidgets.hbox [moduleName, dot, varName], colon, typeEdit]
  where
    makeNamePartEditor color namePartStr setter makeWidgetId =
      BWidgets.setTextColor color .
      BWidgets.wrapDelegated FocusDelegator.NotDelegating
      (BWidgets.makeWordEdit (Property (return namePartStr) setter)) $
      makeWidgetId myId
    maybeSetter f = maybe (const (return ())) f setFFIName
    modulePathStr = List.intercalate "." modulePath
    modulePathSetter = maybeSetter $ \ffiNameSetter ->
      ffiNameSetter . (`Data.FFIName` name) . splitOn "."
    nameSetter = maybeSetter $ \ffiNameSetter -> ffiNameSetter . Data.FFIName modulePath


make
  :: MonadF m
  => ExpressionEditMaker m
  -> Sugar.DefinitionRef m
  -> TWidget ViewTag m
make makeExpressionEdit def = do
  cursor <- readCursor
  case Sugar.drDef def of
    Sugar.DefinitionExpression sExpr ->
      liftM
        ( Box.toWidget . (Box.atBoxContent . fmap) (addJumps cursor) .
          BWidgets.hboxK
        ) $
        makeParts makeExpressionEdit myId ident sExpr
    Sugar.DefinitionBuiltin builtin ->
      (liftM . Widget.weakerEvents) builtinEventMap $
      makeBuiltinEdit makeExpressionEdit myId builtin
  where
    replaceWithExpression setter = do
      newExprI <- DataOps.newHole
      ~() <- setter $ Data.DefinitionExpression newExprI
      return $ WidgetIds.fromIRef newExprI
    builtinEventMap =
      maybe mempty
      (Widget.actionEventMapMovesCursor
       Config.replaceBuiltinWithExpressionKeys
       "Replace with Expression" .
       replaceWithExpression) $
      Sugar.defReplace actions
    actions = Sugar.drActions def
    ident = Sugar.defGuid actions
    myId = WidgetIds.fromGuid ident
