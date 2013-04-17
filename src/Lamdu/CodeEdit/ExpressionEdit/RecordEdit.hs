{-# LANGUAGE OverloadedStrings #-}
module Lamdu.CodeEdit.ExpressionEdit.RecordEdit(make) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Monoid (Monoid(..))
import Data.Store.Transaction (Transaction)
import Data.Vector.Vector2 (Vector2(..))
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import qualified Control.Lens as Lens
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid
import qualified Lamdu.BottleWidgets as BWidgets
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.ExpressionEdit.FieldEdit as FieldEdit
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.Config as Config
import qualified Lamdu.WidgetIds as WidgetIds

type T = Transaction

make ::
  MonadA m =>
  Sugar.Record m (Sugar.Expression m) -> Widget.Id -> ExprGuiM m (ExpressionGui m)
make = ExpressionGui.wrapExpression . makeUnwrapped

makeUnwrapped ::
  MonadA m =>
  Sugar.Record m (Sugar.Expression m) -> Widget.Id -> ExprGuiM m (ExpressionGui m)
makeUnwrapped (Sugar.Record k fields mAddField) myId =
  ExprGuiM.assignCursor myId bracketId $ do
    fieldRows <- mapM makeFieldRow fields
    let fieldsWidget = Grid.toWidget $ Grid.make fieldRows
    bracketWidget <-
      ( ExprGuiM.withFgColor (parensColor k)
      . ExprGuiM.widgetEnv . BWidgets.makeFocusableTextView "{" )
      bracketId
    let
      height = Widget.wSize . Lens._2
      bracketHeight = bracketWidget ^. height
      fieldsHeight = fieldsWidget ^. height
      resizedBracketWidget
        | fieldsHeight > 0 =
          Widget.scale (Vector2 1 (fieldsHeight / bracketHeight)) bracketWidget
        | otherwise = bracketWidget
    return . ExpressionGui.fromValueWidget . Widget.weakerEvents eventMap $
      Box.hboxCentered [resizedBracketWidget, fieldsWidget]
  where
    parensColor Sugar.Type = Config.recordTypeParensColor
    parensColor Sugar.Val = Config.recordValParensColor
    sep Sugar.Val = "="
    sep Sugar.Type = ":"
    bracketId = Widget.joinId myId ["{"]
    makeFieldRow (Sugar.RecordField mItemActions field fieldGuid fieldExpr) = do
      fieldRefGui <- FieldEdit.make field $ WidgetIds.fromGuid fieldGuid
      fieldExprGui <- ExprGuiM.makeSubexpresion fieldExpr
      sepGui <-
        ExpressionGui.fromValueWidget <$>
        (ExprGuiM.widgetEnv . BWidgets.makeLabel (sep k) .
         Widget.toAnimId . WidgetIds.fromGuid) fieldGuid
      let itemEventMap = maybe mempty recordItemEventMap mItemActions
      return . ExpressionGui.makeRow $
        [(1, fieldRefGui), (0.5, sepGui), (0, fieldExprGui)]
        & Lens.mapped . Lens._2 . ExpressionGui.egWidget %~
          Widget.weakerEvents itemEventMap
    mkEventMap f mAction keys doc =
      maybe mempty (Widget.keysEventMapMovesCursor keys doc . f) mAction
    eventMap =
      mkEventMap (fmap WidgetIds.fromGuid)
      mAddField Config.recordAddFieldKeys $ E.Doc ["Edit", "Record", "Add First Field"]

recordItemEventMap :: MonadA m => Sugar.ListItemActions m -> Widget.EventHandlers (T m)
recordItemEventMap (Sugar.ListItemActions addNext delete) =
  mconcat
  [ Widget.keysEventMapMovesCursor Config.recordAddFieldKeys
    (E.Doc ["Edit", "Record", "Add Next Field"]) $ WidgetIds.fromGuid <$> addNext
  , Widget.keysEventMapMovesCursor (Config.delForwardKeys ++ Config.delBackwordKeys)
    (E.Doc ["Edit", "Record", "Delete Field"]) $ WidgetIds.fromGuid <$> delete
  ]
