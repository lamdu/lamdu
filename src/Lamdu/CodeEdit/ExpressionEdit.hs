{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Lamdu.CodeEdit.ExpressionEdit(make) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Monoid (Monoid(..))
import Data.Store.Transaction (Transaction)
import Data.Traversable (traverse)
import Graphics.UI.Bottle.Widget (EventHandlers)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui, ParentPrecedence(..))
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import Lamdu.Config (Config)
import qualified Control.Lens as Lens
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.CodeEdit.ExpressionEdit.ApplyEdit as ApplyEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.AtomEdit as AtomEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.CollapsedEdit as CollapsedEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.ExpressionEdit.GetFieldEdit as GetFieldEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.GetParamsEdit as GetParamsEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.GetVarEdit as GetVarEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.HoleEdit as HoleEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.InferredEdit as InferredEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.LambdaEdit as LambdaEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.ListEdit as ListEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.LiteralEdit as LiteralEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.PiEdit as PiEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.RecordEdit as RecordEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.TagEdit as TagEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.Wrap as Wrap
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.Config as Config
import qualified Lamdu.WidgetEnvT as WE
import qualified Lamdu.WidgetIds as WidgetIds

data IsHole = NotAHole | IsAHole

pasteEventMap ::
  MonadA m =>
  Config -> Sugar.Hole Sugar.Name m (Sugar.ExpressionN m ()) ->
  Widget.EventHandlers (Transaction m)
pasteEventMap config =
  maybe mempty
  (Widget.keysEventMapMovesCursor
   (Config.pasteKeys config) (E.Doc ["Edit", "Paste"]) .
   fmap WidgetIds.fromGuid) .
  (^? Sugar.holeMActions . Lens._Just . Sugar.holePaste . Lens._Just)

make ::
  MonadA m => ParentPrecedence ->
  Sugar.ExpressionN m () -> ExprGuiM m (ExpressionGui m)
make parentPrecedence sExpr = assignCursor $ do
  ((isHole, widget), _) <-
    ExprGuiM.listenResultPickers $ makeEditor parentPrecedence sExpr exprId
  typeEdits <- traverse (make (ParentPrecedence 0)) $ payload ^. Sugar.plInferredTypes
  let onReadOnly = Widget.doesntTakeFocus
  exprEventMap <- expressionEventMap isHole sExpr
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    addInferredTypes =
      ExpressionGui.addType config ExpressionGui.Background exprId $
      Widget.tint (Config.inferredTypeTint config) .
      Widget.scale (realToFrac <$> Config.typeScaleFactor config) .
      (^. ExpressionGui.egWidget) <$> typeEdits
  return $
    addInferredTypes widget
    & ExpressionGui.egWidget %~
      maybe onReadOnly (const id) (payload ^. Sugar.plActions) .
      Widget.weakerEvents exprEventMap
  where
    payload = sExpr ^. Sugar.rPayload
    exprId = WidgetIds.fromGuid $ sExpr ^. Sugar.rPayload . Sugar.plGuid
    assignCursor f =
      foldr (`ExprGuiM.assignCursorPrefix` exprId) f
      (WidgetIds.fromGuid <$> sExpr ^. Sugar.rPayload . Sugar.plHiddenGuids)

makeEditor ::
  MonadA m => ParentPrecedence ->
  Sugar.ExpressionN m () -> Widget.Id ->
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
    exprGuid = sExpr ^. Sugar.rPayload . Sugar.plGuid
    mkEditor =
      case sExpr ^. Sugar.rBody of
      Sugar.BodyInferred i ->
        isAHole (i ^. Sugar.iHole) $ InferredEdit.make parentPrecedence i exprGuid
      Sugar.BodyHole hole ->
        isAHole hole $ HoleEdit.make hole mNextHoleGuid exprGuid
      Sugar.BodyCollapsed poly ->
        notAHole $ CollapsedEdit.make parentPrecedence poly
      Sugar.BodyApply apply ->
        notAHole $ ApplyEdit.make parentPrecedence sExpr apply
      Sugar.BodyLam lam@(Sugar.Lam Sugar.Type _ _ _) ->
        notAHole $ PiEdit.make parentPrecedence lam
      Sugar.BodyLam lam@(Sugar.Lam Sugar.Val _ _ _) ->
        notAHole $ LambdaEdit.make parentPrecedence lam
      Sugar.BodyLiteralInteger integer ->
        notAHole $ LiteralEdit.makeInt integer
      Sugar.BodyAtom atom ->
        notAHole $ AtomEdit.make atom
      Sugar.BodyList list ->
        notAHole $ ListEdit.make list
      Sugar.BodyRecord record ->
        notAHole $ RecordEdit.make record
      Sugar.BodyGetField getField ->
        notAHole $ GetFieldEdit.make getField
      Sugar.BodyTag tag ->
        notAHole $ TagEdit.make tag
      Sugar.BodyGetVar gv ->
        notAHole $ GetVarEdit.make gv
      Sugar.BodyGetParams gp ->
        notAHole $ GetParamsEdit.make gp
  mkEditor myId
  where
    notAHole = (fmap . fmap) ((,) NotAHole)
    mNextHoleGuid = sExpr ^. Sugar.rPayload . Sugar.plMNextHoleGuid

expressionEventMap ::
  MonadA m =>
  IsHole ->
  Sugar.ExpressionN m () ->
  ExprGuiM m (EventHandlers (Transaction m))
expressionEventMap isHole sExpr =
  maybe (return mempty) (actionsEventMap sExpr isHole) $
  sExpr ^. Sugar.rPayload . Sugar.plActions

actionsEventMap ::
  MonadA m =>
  Sugar.ExpressionN m () -> IsHole ->
  Sugar.Actions m ->
  ExprGuiM m (EventHandlers (Transaction m))
actionsEventMap sExpr isHole actions = do
  isSelected <- ExprGuiM.widgetEnv . WE.isSubCursor $ WidgetIds.fromGuid exprGuid
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    delKeys = Config.replaceKeys config ++ Config.delKeys config
    replace
      | isSelected =
        maybe mempty
        (mkEventMap delKeys (E.Doc ["Edit", "Replace expression"]) FocusDelegator.delegatingId) $
        actions ^. Sugar.mSetToHole
      | otherwise =
        mkEventMap delKeys (E.Doc ["Navigation", "Select parent"])
        FocusDelegator.notDelegatingId $ return exprGuid
    cut
      | isHoleBool = mempty
      | otherwise =
        mkEventMap (Config.cutKeys config) (E.Doc ["Edit", "Cut"]) id $
        actions ^. Sugar.cut
  return $ mconcat
    [ Wrap.eventMap config actions
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
