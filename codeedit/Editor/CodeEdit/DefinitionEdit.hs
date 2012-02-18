{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.DefinitionEdit(make) where

import Control.Monad (liftM)
import Data.List.Utils (enumerate, atPred)
import Data.Monoid (Monoid(..))
import Data.Store.IRef (IRef)
import Editor.Anchors (ViewTag)
import Editor.CTransaction (TWidget, getP, assignCursor)
import Editor.MonadF (MonadF)
import Graphics.UI.Bottle.Rect(Rect)
import Graphics.UI.Bottle.Widgets.Grid(GridElement)
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.ExpressionEdit as ExpressionEdit
import qualified Editor.CodeEdit.Types as ETypes
import qualified Editor.Config as Config
import qualified Editor.Data as Data
import qualified Editor.DataOps as DataOps
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

make :: MonadF m => IRef Data.Definition -> TWidget ViewTag m
make definitionI = do
  Data.Definition params _ <- getP definitionRef
  nameEdit <- makeNameEdit myId definitionI
  equals <- BWidgets.makeTextView "=" $ Widget.joinId myId ["equals"]

  let
    replaceEventMap =
      Widget.actionEventMapMovesCursor Config.delKeys "Replace" .
      ETypes.diveIn $ DataOps.replace bodyRef

  rhsEdit <-
    liftM (Widget.weakerEvents replaceEventMap) $
    ExpressionEdit.make [] definitionI bodyRef

  let
    paramEventMap i paramI =
      Widget.actionEventMapMovesCursor Config.delKeys
      "Delete parameter" $ do
        DataOps.delParameter definitionRef paramI
        Data.Definition newParams _ <- Property.get definitionRef
        return $
          (myId : map WidgetIds.fromIRef newParams)
          !! min (1+i) (length newParams)
    makeParamEdit (i, paramI) =
      (liftM . Widget.weakerEvents) (paramEventMap i paramI) .
      BWidgets.wrapDelegated FocusDelegator.NotDelegating
      (BWidgets.setTextColor Config.parameterColor .
       BWidgets.makeNameEdit ("<unnamed param " ++ show i ++ ">") paramI) $
      WidgetIds.fromIRef paramI

  paramsEdits <- mapM makeParamEdit $ enumerate params

  return .
    Widget.weakerEvents eventMap .
    Box.toWidget .
    (Box.atBoxContent . fmap) addJumps .
    BWidgets.hboxSpacedK ("space" :: String) $
    [("lhs", BWidgets.hboxSpaced (nameEdit : paramsEdits)),
     ("equals", equals),
     ("rhs", rhsEdit)]

  where
    bodyRef = Property.composeLabel Data.defBody Data.atDefBody definitionRef
    definitionRef = Transaction.fromIRef definitionI
    eventMap =
      Widget.actionEventMapMovesCursor Config.addParamKeys "Add parameter" .
      liftM (WidgetIds.delegating . WidgetIds.fromIRef) $
      DataOps.addParameter definitionRef
    myId = WidgetIds.fromIRef definitionI

makeJumpEventMap
  :: String
  -> [E.EventType]
  -> (Graphics.UI.Bottle.Rect.Rect -> enter)
  -> Graphics.UI.Bottle.Widgets.Grid.GridElement f1
  -> (enter -> Widget.EnterResult f2)
  -> E.EventMap (f2 Widget.EventResult)
makeJumpEventMap doc keys dir destElement enter =
  E.fromEventTypes keys ("Jump to "++doc) .
  Widget.enterResultEvent . enter . dir $
  Box.boxElementRect destElement

jumpToExpressionEventMap
  :: String
  -> [E.EventType]
  -> (Graphics.UI.Bottle.Rect.Rect -> Direction.Direction)
  -> Graphics.UI.Bottle.Widgets.Grid.GridElement f
  -> E.EventMap (f Widget.EventResult)
jumpToExpressionEventMap doc keys dir destElement =
  maybe mempty
  (makeJumpEventMap doc keys dir destElement) .
  Widget.uioMaybeEnter $ Box.boxElementUio destElement

addJumpsToSrc
  :: String
  -> [E.EventType]
  -> (Graphics.UI.Bottle.Rect.Rect -> Direction.Direction)
  -> Graphics.UI.Bottle.Widgets.Grid.GridElement f
  -> Graphics.UI.Bottle.Widgets.Grid.GridElement f
  -> Graphics.UI.Bottle.Widgets.Grid.GridElement f
addJumpsToSrc doc keys dir destElement srcElement =
  (Box.atBoxElementUio . Widget.atUioEventMap . mappend)
  (jumpToExpressionEventMap doc keys dir destElement)
  srcElement

addJumps
  :: [(String, Box.BoxElement f)]
  -> [(String, Graphics.UI.Bottle.Widgets.Grid.GridElement f)]
addJumps defKBoxElements =
  addEventMap "lhs" "rhs" "right-hand side" Config.jumpToRhsKeys Direction.fromLeft .
  addEventMap "rhs" "lhs" "left-hand side" Config.jumpToLhsKeys Direction.fromRight $
  defKBoxElements
  where
    addEventMap srcSide destSide doc keys dir =
      atPred (== srcSide)
      (addJumpsToSrc doc keys dir $ Box.getElement destSide defKBoxElements)
