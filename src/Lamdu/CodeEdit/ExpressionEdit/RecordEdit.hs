{-# LANGUAGE OverloadedStrings #-}
module Lamdu.CodeEdit.ExpressionEdit.RecordEdit(make) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Monoid (Monoid(..))
import Data.Vector.Vector2 (Vector2(..))
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import qualified Control.Lens as Lens
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid
import qualified Lamdu.BottleWidgets as BWidgets
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.ExpressionEdit.FieldEdit as FieldEdit
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.Config as Config
import qualified Lamdu.WidgetIds as WidgetIds

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
    makeFieldRow (Sugar.RecordField mDel fieldGuid fieldExpr) = do
      fieldRefGui <- FieldEdit.make fieldGuid $ fromFieldExpr fieldExpr
      fieldExprGui <- ExprGuiM.makeSubexpresion fieldExpr
      sepGui <-
        ExpressionGui.fromValueWidget <$>
        (ExprGuiM.widgetEnv . BWidgets.makeLabel (sep k) .
         Widget.toAnimId . fromFieldExpr) fieldExpr
      let
        delEventMap =
          mkEventMap (fmap (maybe myId fromFieldExprGuid)) mDel
          (Config.delForwardKeys ++ Config.delBackwordKeys) $
          E.Doc ["Edit", "Record", "Field", "Delete"]
      return . ExpressionGui.makeRow $
        [(1, fieldRefGui), (0.5, sepGui), (0, fieldExprGui)]
        & Lens.mapped . Lens._2 . ExpressionGui.egWidget %~
          Widget.weakerEvents delEventMap
    mkEventMap f mAction keys doc =
      maybe mempty (Widget.keysEventMapMovesCursor keys doc . f) mAction
    fromFieldExprGuid = mappend myId . WidgetIds.fromGuid
    fromFieldExpr = fromFieldExprGuid . (^. Sugar.rGuid)
    eventMap =
      mkEventMap (fmap (FocusDelegator.delegatingId . fromFieldExprGuid))
      mAddField Config.recordAddFieldKeys $ E.Doc ["Edit", "Record", "Add Field"]
