{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Lamdu.GUI.ExpressionEdit(make) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Monoid (Monoid(..))
import Data.Store.Transaction (Transaction)
import Graphics.UI.Bottle.Widget (EventHandlers)
import Lamdu.Config (Config)
import Lamdu.GUI.ExpressionEdit.ExpressionGui (ExpressionGui, ParentPrecedence(..))
import Lamdu.GUI.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import qualified Control.Lens as Lens
import qualified Data.List as List
import qualified Graphics.DrawingCombinators.Utils as DrawUtils
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionEdit.ApplyEdit as ApplyEdit
import qualified Lamdu.GUI.ExpressionEdit.AtomEdit as AtomEdit
import qualified Lamdu.GUI.ExpressionEdit.CollapsedEdit as CollapsedEdit
import qualified Lamdu.GUI.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.GUI.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionEdit.GetFieldEdit as GetFieldEdit
import qualified Lamdu.GUI.ExpressionEdit.GetParamsEdit as GetParamsEdit
import qualified Lamdu.GUI.ExpressionEdit.GetVarEdit as GetVarEdit
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit as HoleEdit
import qualified Lamdu.GUI.ExpressionEdit.InferredEdit as InferredEdit
import qualified Lamdu.GUI.ExpressionEdit.LambdaEdit as LambdaEdit
import qualified Lamdu.GUI.ExpressionEdit.ListEdit as ListEdit
import qualified Lamdu.GUI.ExpressionEdit.LiteralEdit as LiteralEdit
import qualified Lamdu.GUI.ExpressionEdit.Modify as Modify
import qualified Lamdu.GUI.ExpressionEdit.PiEdit as PiEdit
import qualified Lamdu.GUI.ExpressionEdit.RecordEdit as RecordEdit
import qualified Lamdu.GUI.ExpressionEdit.TagEdit as TagEdit
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

data IsHole = NotAHole | IsAHole

pasteEventMap ::
  MonadA m =>
  Config -> Sugar.Hole Sugar.Name m (ExprGuiM.SugarExpr m) ->
  Widget.EventHandlers (Transaction m)
pasteEventMap config =
  maybe mempty
  (Widget.keysEventMapMovesCursor
   (Config.pasteKeys config) (E.Doc ["Edit", "Paste"]) .
   fmap WidgetIds.fromGuid) .
  (^? Sugar.holeMActions . Lens._Just . Sugar.holePaste . Lens._Just)

shrinkIfHigherThanLine :: MonadA m => ExpressionGui f -> ExprGuiM m (ExpressionGui f)
shrinkIfHigherThanLine w = do
  fontSize <-
    (^. TextEdit.sTextViewStyle . TextView.styleFontSize) <$>
    ExprGuiM.widgetEnv WE.readTextStyle
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    textHeight = fromIntegral fontSize * DrawUtils.textHeight
    ratio =
      (textHeight / w ^. ExpressionGui.egWidget . Widget.wSize . Lens._2)
      ** realToFrac (Config.holeResultInjectedScaleExponent config)
  return $
    if ratio < 1
    then ExpressionGui.scaleFromTop (realToFrac ratio) w
    else w

make ::
  MonadA m => ParentPrecedence ->
  ExprGuiM.SugarExpr m -> ExprGuiM m (ExpressionGui m)
make parentPrecedence sExpr = assignCursor $ do
  ((isHole, gui), _) <-
    ExprGuiM.listenResultPickers $ makeEditor parentPrecedence sExpr exprId
  exprEventMap <- expressionEventMap isHole sExpr
  maybeShrink gui
    <&>
    ExpressionGui.egWidget %~
    ( maybe Widget.doesntTakeFocus (const id) (pl ^. Sugar.plActions)
    . Widget.weakerEvents exprEventMap
    )
  where
    pl = sExpr ^. Sugar.rPayload
    ExprGuiM.Payload guids isInjecteds = pl ^. Sugar.plData
    exprHiddenGuids = List.delete (pl ^. Sugar.plGuid) guids
    exprId = WidgetIds.fromGuid $ pl ^. Sugar.plGuid
    maybeShrink
      | or isInjecteds = shrinkIfHigherThanLine
      | otherwise = return
    assignCursor f =
      foldr (`ExprGuiM.assignCursorPrefix` exprId) f $
      WidgetIds.fromGuid <$> exprHiddenGuids

makeEditor ::
  MonadA m => ParentPrecedence ->
  ExprGuiM.SugarExpr m -> Widget.Id ->
  ExprGuiM m (IsHole, ExpressionGui m)
makeEditor parentPrecedence sExpr myId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    isAHole hole mkWidget = fmap (handleHole hole) . mkWidget
    handleHole hole widget =
      ( IsAHole
      , widget &
        ExpressionGui.egWidget %~ Widget.weakerEvents (pasteEventMap config hole)
      )
    mkEditor =
      case sExpr ^. Sugar.rBody of
      Sugar.BodyInferred i ->
        isAHole (i ^. Sugar.iHole) $ InferredEdit.make parentPrecedence pl i
      Sugar.BodyHole hole ->
        isAHole hole $ HoleEdit.make pl hole
      Sugar.BodyCollapsed poly ->
        notAHole $ CollapsedEdit.make parentPrecedence poly
      Sugar.BodyApply apply ->
        notAHole $ ApplyEdit.make parentPrecedence pl apply
      Sugar.BodyLam lam@(Sugar.Lam Sugar.KType _ _ _) ->
        notAHole $ PiEdit.make parentPrecedence pl lam
      Sugar.BodyLam lam@(Sugar.Lam Sugar.KVal _ _ _) ->
        notAHole $ LambdaEdit.make parentPrecedence pl lam
      Sugar.BodyLiteralInteger integer ->
        notAHole $ LiteralEdit.makeInt pl integer
      Sugar.BodyAtom atom ->
        notAHole $ AtomEdit.make atom
      Sugar.BodyList list ->
        notAHole $ ListEdit.make pl list
      Sugar.BodyRecord record ->
        notAHole $ RecordEdit.make pl record
      Sugar.BodyGetField getField ->
        notAHole $ GetFieldEdit.make pl getField
      Sugar.BodyTag tag ->
        notAHole $ TagEdit.make pl tag
      Sugar.BodyGetVar gv ->
        notAHole $ GetVarEdit.make pl gv
      Sugar.BodyGetParams gp ->
        notAHole $ GetParamsEdit.make pl gp
  mkEditor myId
  where
    pl = sExpr ^. Sugar.rPayload
    notAHole = (fmap . fmap) ((,) NotAHole)

expressionEventMap ::
  MonadA m =>
  IsHole ->
  ExprGuiM.SugarExpr m ->
  ExprGuiM m (EventHandlers (Transaction m))
expressionEventMap isHole sExpr =
  maybe (return mempty) (actionsEventMap sExpr isHole) $
  sExpr ^. Sugar.rPayload . Sugar.plActions

actionsEventMap ::
  MonadA m =>
  ExprGuiM.SugarExpr m -> IsHole ->
  Sugar.Actions m ->
  ExprGuiM m (EventHandlers (Transaction m))
actionsEventMap sExpr isHole actions = do
  isSelected <- ExprGuiM.widgetEnv . WE.isSubCursor $ WidgetIds.fromGuid exprGuid
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    delKeys = Config.replaceKeys config ++ Config.delKeys config
    replace
      | isSelected = Modify.replaceEventMap config actions
      | otherwise =
        mkEventMap delKeys (E.Doc ["Navigation", "Select parent"])
        FocusDelegator.notDelegatingId $ return exprGuid
    cut
      | isHoleBool = mempty
      | otherwise =
        mkEventMap (Config.cutKeys config) (E.Doc ["Edit", "Cut"]) id $
        actions ^. Sugar.cut
  return $ mconcat
    [ Modify.wrapEventMap config actions
    , replace
    , cut
    ]
  where
    exprGuid = sExpr ^. Sugar.rPayload . Sugar.plGuid
    mkEventMap keys doc f =
      Widget.keysEventMapMovesCursor keys doc .
      fmap (f . WidgetIds.fromGuid)
    isHoleBool =
      case isHole of
      NotAHole -> False
      IsAHole -> True
