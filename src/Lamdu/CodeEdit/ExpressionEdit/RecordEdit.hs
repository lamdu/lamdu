{-# LANGUAGE OverloadedStrings #-}

module Lamdu.CodeEdit.ExpressionEdit.RecordEdit(make) where

import Control.Applicative ((<$>))
import Control.Lens ((^.))
import Control.MonadA (MonadA)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Vector.Vector2 (Vector2(..))
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM, WidgetT)
import qualified Control.Lens as Lens
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid
import qualified Lamdu.BottleWidgets as BWidgets
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.Config as Config
import qualified Lamdu.WidgetEnvT as WE
import qualified Lamdu.WidgetIds as WidgetIds

fieldFDConfig :: FocusDelegator.Config
fieldFDConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKey = E.ModKey E.noMods E.KeyEnter
  , FocusDelegator.startDelegatingDoc = E.Doc ["Edit", "Record", "Field", "Rename"]
  , FocusDelegator.stopDelegatingKey = E.ModKey E.noMods E.KeyEsc
  , FocusDelegator.stopDelegatingDoc = E.Doc ["Edit", "Record", "Field", "Done renaming"]
  }

makeFieldNameEdit ::
  MonadA m => (ExprGuiM.NameSource, String) ->
  Widget.Id -> Guid -> ExprGuiM m (WidgetT m)
makeFieldNameEdit name myId fieldGuid =
  ExprGuiM.wrapDelegated fieldFDConfig FocusDelegator.NotDelegating id
  (ExpressionGui.makeNameEdit name fieldGuid)
  myId

make ::
  MonadA m =>
  Sugar.Record m (Sugar.Expression m) -> Widget.Id -> ExprGuiM m (ExpressionGui m)
make (Sugar.Record k fields mAddField) myId = do
  fieldRows <- mapM makeFieldRow fields
  let fieldsWidget = Grid.toWidget $ Grid.make fieldRows
  bracketWidget <-
    ExprGuiM.atEnv (WE.setTextColor Config.recordParensColor) .
    ExprGuiM.widgetEnv . BWidgets.makeFocusableTextView "{" $ Widget.joinId myId ["{"]
  let
    height = Widget.wSize . Lens._2
    bracketHeight = bracketWidget ^. height
    fieldsHeight = fieldsWidget ^. height
    resizedBracketWidget
      | fieldsHeight > 0 =
        Widget.scale (Vector2 1 (fieldsHeight / bracketHeight)) bracketWidget
      | otherwise = bracketWidget
  return . ExpressionGui.fromValueWidget . Widget.weakerEvents eventMap $
    BWidgets.hboxCenteredSpaced [resizedBracketWidget, fieldsWidget]
  where
    makeFieldRow (Sugar.RecordField mDel fieldGuid fieldExpr) = do
      name <- ExprGuiM.getGuidName fieldGuid
      nameEdit <- makeFieldNameEdit name fieldId fieldGuid
      fieldExprEdit <- (^. ExpressionGui.egWidget) <$> ExprGuiM.makeSubexpresion fieldExpr
      let
        delEventMap =
          mkEventMap id mDel (Config.delForwardKeys ++ Config.delBackwordKeys) $
          E.Doc ["Edit", "Record", "Field", "Delete"]
      -- TODO: Could be equals rather than : for Val
      let
        sep Sugar.Val = "="
        sep Sugar.Type = ":"
      sepEdit <-
        ExprGuiM.widgetEnv . BWidgets.makeLabel (sep k) $ Widget.toAnimId fieldId
      return
        [ (Vector2 1 0.5, Widget.weakerEvents delEventMap nameEdit)
        , (0.5, sepEdit)
        , (Vector2 0 0.5, Widget.weakerEvents delEventMap fieldExprEdit)
        ]
      where
        fieldId = mappend myId $ WidgetIds.fromGuid fieldGuid
    mkEventMap f mAction keys doc =
      maybe mempty
      ( Widget.keysEventMapMovesCursor keys doc
      . fmap (f . WidgetIds.fromGuid)
      ) mAction
    eventMap =
      mkEventMap FocusDelegator.delegatingId mAddField Config.recordAddFieldKeys $
      E.Doc ["Edit", "Record", "Add Field"]
