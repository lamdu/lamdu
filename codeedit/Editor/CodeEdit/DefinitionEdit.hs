{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.DefinitionEdit(make) where

import Control.Monad (liftM)
import Data.List.Utils (enumerate, atPred)
import Data.Monoid (Monoid(..))
import Data.Store.IRef (IRef)
import Editor.Anchors (ViewTag)
import Editor.CTransaction (TWidget, getP, assignCursor)
import Editor.MonadF (MonadF)
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

make :: MonadF m => IRef Data.Definition -> TWidget ViewTag m
make definitionI = do
  Data.Definition params _ <- getP definitionRef
  nameEdit <-
    assignCursor myId nameEditAnimId .
    BWidgets.wrapDelegated FocusDelegator.NotDelegating
    (BWidgets.setTextColor Config.definitionColor .
     BWidgets.makeNameEdit Config.unnamedStr definitionI) $
    nameEditAnimId
  equals <- BWidgets.makeTextView "=" $ Widget.joinId myId ["equals"]

  let
    replaceEventMap =
      Widget.actionEventMapMovesCursor Config.delKeys "Replace" .
      ETypes.diveIn $ DataOps.replace bodyRef

  expressionEdit <-
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

  let
    makeEnterRhsEventMap rhsRect enterRhs =
      E.fromEventTypes Config.jumpToExpressionKeys
      "Jump to expression" . Widget.enterResultEvent . enterRhs $
      Direction.fromLeft rhsRect

    jumpToExpressionEventMap rhsBoxElement =
      maybe mempty (makeEnterRhsEventMap (Box.boxElementRect rhsBoxElement)) .
      Widget.uioMaybeEnter $ Box.boxElementUio rhsBoxElement

    addJumpToExpression defKBoxElements =
      atPred (=="lhs") addEventMapToLhs defKBoxElements
      where
        addEventMapToLhs =
          (Box.atBoxElementUio . Widget.atUioEventMap . mappend)
          (jumpToExpressionEventMap rhsElement)
        rhsElement = Box.getElement "rhs" defKBoxElements

    box =
      Box.toWidget .
      (Box.atBoxContent . fmap) addJumpToExpression .
      BWidgets.hboxSpacedK ("space" :: String) $
      [("lhs", BWidgets.hboxSpaced (nameEdit : paramsEdits)),
       ("equals", equals),
       ("rhs", expressionEdit)]
  return . Widget.weakerEvents eventMap $ box

  where
    bodyRef = Property.composeLabel Data.defBody Data.atDefBody definitionRef
    definitionRef = Transaction.fromIRef definitionI
    eventMap =
      Widget.actionEventMapMovesCursor Config.addParamKeys "Add parameter" .
      liftM (WidgetIds.delegating . WidgetIds.fromIRef) $
      DataOps.addParameter definitionRef
    nameEditAnimId = Widget.joinId myId ["name"]
    myId = WidgetIds.fromIRef definitionI

