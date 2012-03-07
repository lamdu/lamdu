{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.DefinitionEdit(make) where

import Control.Monad (liftM)
import Data.List.Utils (enumerate, atPred)
import Data.Monoid (Monoid(..))
import Data.Store.IRef (IRef)
import Editor.Anchors (ViewTag)
import Editor.CTransaction (TWidget, getP, assignCursor)
import Editor.MonadF (MonadF)
import Graphics.UI.Bottle.Widgets.Grid(GridElement)
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.ExpressionEdit as ExpressionEdit
import qualified Editor.CodeEdit.ParamEdit as ParamEdit
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

makeRHSEdit
  :: MonadF m
  => IRef Data.Definition
  -> ETypes.ExpressionPtr m
  -> TWidget ViewTag m
makeRHSEdit = ExpressionEdit.make []

makeLHSEdit
  :: MonadF m
  => Widget.Id
  -> IRef Data.Definition
  -> [IRef Data.Parameter]
  -> TWidget t m
makeLHSEdit myId definitionI params = do
  nameEdit <-
    weakerEvents (addParamEventMap 0) $
    makeNameEdit myId definitionI
  paramsEdits <- mapM makeParamEdit $ enumerate params
  return $ BWidgets.hboxSpaced (nameEdit : paramsEdits)
  where
    makeParamEdit (i, paramI) =
      weakerEvents (paramEventMap i paramI) $ ParamEdit.make paramI
    weakerEvents = liftM . Widget.weakerEvents
    definitionRef = Transaction.fromIRef definitionI
    addParamEventMap i =
      Widget.actionEventMapMovesCursor Config.addNextParamKeys "Add parameter" .
      liftM (WidgetIds.delegating . WidgetIds.fromIRef) $
      DataOps.addParameter i definitionRef
    delParamEventMap i paramI =
      Widget.actionEventMapMovesCursor Config.delKeys
      "Delete parameter" $ do
        DataOps.delParameter definitionRef paramI
        Data.Definition newParams _ <- Property.get definitionRef
        return $
          (myId : map WidgetIds.fromIRef newParams)
          !! min (1+i) (length newParams)
    paramEventMap i paramI = mconcat
      [addParamEventMap (i+1)
      ,delParamEventMap i paramI
      ]

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
    addJumpsTo doc keys dir destElement srcElement =
      (Box.atBoxElementUio . Widget.atUioEventMap . mappend)
      (jumpToExpressionEventMap doc keys dir destElement)
      srcElement
    jumpToExpressionEventMap doc keys dir destElement =
      maybe mempty
      (makeJumpForEnter doc keys dir destElement) .
      Widget.uioMaybeEnter $ Box.boxElementUio destElement
    makeJumpForEnter doc keys dir destElement enter =
      E.fromEventTypes keys ("Jump to "++doc) .
      Widget.enterResultEvent . enter . dir $
      Box.boxElementRect destElement

make :: MonadF m => IRef Data.Definition -> TWidget ViewTag m
make definitionI = do
  Data.Definition params _ <- getP definitionRef
  lhsEdit <- makeLHSEdit myId definitionI params
  equals <- BWidgets.makeLabel "=" myId
  rhsEdit <-
    makeRHSEdit definitionI $
    Property.composeLabel Data.defBody Data.atDefBody definitionRef
  return .
    Box.toWidget . (Box.atBoxContent . fmap) (addJumps "lhs" "rhs") .
    BWidgets.hboxSpacedK ("space" :: String) $
    [("lhs", lhsEdit),
     ("equals", equals),
     ("rhs", rhsEdit)]
  where
    definitionRef = Transaction.fromIRef definitionI
    myId = WidgetIds.fromIRef definitionI
