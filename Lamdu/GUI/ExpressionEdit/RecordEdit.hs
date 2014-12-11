{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.RecordEdit(make) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Monoid (Monoid(..))
import Data.Store.Transaction (Transaction)
import Data.Vector.Vector2 (Vector2(..))
import Lamdu.Config (Config)
import Lamdu.GUI.ExpressionGui (ExpressionGui)
import Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import Lamdu.Sugar.AddNames.Types (Name(..))
import qualified Control.Lens as Lens
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import qualified Lamdu.GUI.ExpressionEdit.TagEdit as TagEdit
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction

make ::
  MonadA m =>
  Sugar.Record (Name m) m (ExprGuiM.SugarExpr m) ->
  Sugar.Payload m ExprGuiM.Payload ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
make reco pl = ExpressionGui.stdWrapParentExpr pl $ makeUnwrapped reco

makeUnwrapped ::
  MonadA m =>
  Sugar.Record (Name m) m (ExprGuiM.SugarExpr m) -> Widget.Id -> ExprGuiM m (ExpressionGui m)
makeUnwrapped (Sugar.Record fields recordTail mAddField) myId =
  ExprGuiM.assignCursor myId tailId $ do
    config <- ExprGuiM.widgetEnv WE.readConfig
    let
      vspace =
        BWidgets.vspaceWidget . realToFrac $
        Config.spaceBetweenAnnotatedArgs config
      padding = realToFrac <$> Config.labeledApplyPadding config
      paddingX = padding & Lens._2 .~ 0
      padWithoutBot = Widget.assymetricPad padding paddingX
      padWithoutTop = Widget.assymetricPad paddingX padding
      makeFieldRow (Sugar.RecordField mDelete tag fieldExpr) = do
        fieldRefGui <- TagEdit.make tag (WidgetIds.fromEntityId (tag ^. Sugar.tagInstance))
        fieldExprGui <- ExprGuiM.makeSubexpression 0 fieldExpr
        let
          itemEventMap = maybe mempty (recordDelEventMap "Field" config) mDelete
          space = ExpressionGui.fromValueWidget BWidgets.stdSpaceWidget
        [(1, scaleTag fieldRefGui), (0.5, space), (0, fieldExprGui)]
          <&> Lens._2 . ExpressionGui.egWidget %~ Widget.weakerEvents itemEventMap
          & ExpressionGui.makeRow
          & (: [replicate 3 (0.5, vspace)])
          & return
      scaleTag =
        ExpressionGui.egWidget %~
        Widget.scale (realToFrac <$> Config.fieldTagScaleFactor config)
    fieldRows <- concat <$> mapM makeFieldRow fields
    let
      fieldsWidget = Grid.make fieldRows & Grid.toWidget & padWithoutBot
      targetWidth =
        case fieldRows of
        [] -> 250
        _ -> fieldsWidget ^. Widget.wSize . Lens._1
      colorScaleRect minWidth f =
        f (Widget.toAnimId tailId)
        & Anim.onImages (Draw.tint (Config.recordTailColor config))
        & Widget.liftView 1
        & Widget.scale (Vector2 (max minWidth targetWidth) 10)
    tailWidget <-
      case recordTail of
      Sugar.ClosedRecord mDeleteTail ->
        colorScaleRect 0 Anim.unitSquare
        & ExprGuiM.widgetEnv . BWidgets.makeFocusableView tailId
        <&> Widget.weakerEvents
            (maybe mempty (recordDelEventMap "Tail" config) mDeleteTail)
      Sugar.RecordExtending rest -> do
        restExpr <-
          ExprGuiM.makeSubexpression 0 rest
          <&> (^. ExpressionGui.egWidget)
          <&> padWithoutTop
        return $ Box.vboxCentered
          [ colorScaleRect (restExpr ^. Widget.wSize . Lens._1) $
            Anim.unitHStripedSquare 20
          , vspace
          , restExpr
          ]
    let
      eventMap =
        mkEventMap (fmap WidgetIds.fromEntityId)
        mAddField (Config.recordAddFieldKeys config) $
        E.Doc ["Edit", "Record", "Add First Field"]
    Box.vboxCentered
      [ fieldsWidget
      , tailWidget
      ]
      & Widget.weakerEvents eventMap
      & ExpressionGui.fromValueWidget
      & ExpressionGui.withBgColor (Config.layerLabeledApplyBG (Config.layers config))
        (Config.labeledApplyBGColor config) (Widget.toAnimId myId ++ ["bg"])
      & return
  where
    tailId = Widget.joinId myId ["tail"]
    mkEventMap f mAction keys doc =
      maybe mempty (Widget.keysEventMapMovesCursor keys doc . f) mAction

recordDelEventMap ::
  MonadA m => String -> Config ->
  T m Sugar.EntityId -> Widget.EventHandlers (T m)
recordDelEventMap name config delete =
  Widget.keysEventMapMovesCursor (Config.delKeys config)
  (E.Doc ["Edit", "Record", "Delete " ++ name]) $ WidgetIds.fromEntityId <$> delete
