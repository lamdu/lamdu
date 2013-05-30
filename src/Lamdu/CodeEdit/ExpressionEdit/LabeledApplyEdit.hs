{-# LANGUAGE OverloadedStrings #-}
module Lamdu.CodeEdit.ExpressionEdit.LabeledApplyEdit
  ( make
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Monoid (mappend)
import Data.Store.Guid (Guid)
import Data.Traversable (traverse)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import Lamdu.Data.Anchors (PresentationMode(..))
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid
import qualified Lamdu.BottleWidgets as BWidgets
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.ExpressionEdit.Parens as Parens
import qualified Lamdu.CodeEdit.ExpressionEdit.TagEdit as TagEdit
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Layers as Layers
import qualified Lamdu.WidgetIds as WidgetIds

funcGuid :: Sugar.ExpressionP name m pl -> Maybe Guid
funcGuid f =
  case f ^. Sugar.rBody of
  Sugar.BodyGetVar gv -> Just $ gv ^. Sugar.gvIdentifier
  Sugar.BodyCollapsed c -> Just $ c ^. Sugar.pCompact . Sugar.gvIdentifier
  _ -> Nothing

getPresentationMode :: MonadA m => Sugar.ExpressionN m -> ExprGuiM m PresentationMode
getPresentationMode func =
  case funcGuid func of
  Just guid -> ExprGuiM.getP $ Anchors.assocPresentationMode guid
  Nothing -> return Verbose

make ::
  MonadA m => ExpressionGui.ParentPrecedence ->
  Sugar.LabeledApply Sugar.Name (Sugar.ExpressionN m) ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
make parentPrecedence (Sugar.LabeledApply func args) myId = do
  presentationMode <- getPresentationMode func
  case presentationMode of
    Verbose -> mkBoxedAdjacant args []
    OO ->
      case args of
        ((_firstArgTag, firstArg):rest) -> mkBoxedAdjacant rest . (: []) =<< ExprGuiM.makeSubexpresion 11 firstArg
        _ -> mkBoxedAdjacant args []
    Infix ->
      case args of
        [(_lName, l), (_rName, r)] -> mkParened parentPrecedence func l r myId
        (_lName, l) : (_rName, r) : rest -> do
          lEdit <- ExprGuiM.makeSubexpresion infixPrecedence l
          opEdit <- mkOpEdit 12 func myId
          rEdit <- ExprGuiM.makeSubexpresion infixPrecedence r
          let funcRow = ExpressionGui.hboxSpaced [lEdit, opEdit, rEdit]
          boxedApply (func ^. Sugar.rGuid) rest funcRow myId
        _ -> mkBoxedAdjacant args []
  where
    mkBoxedAdjacant as adjacants = do
      funcEdit <- ExprGuiM.makeSubexpresion 10 func
      let funcRow = ExpressionGui.hboxSpaced $ funcEdit : adjacants
      boxedApply (func ^. Sugar.rGuid) as funcRow myId

makeTagView :: MonadA m => Widget.Id -> Sugar.TagG Sugar.Name -> ExprGuiM m (ExpressionGui m)
makeTagView myId tagG =
  TagEdit.makeView tagG . Widget.toAnimId . mappend myId .
  WidgetIds.fromGuid $ tagG ^. Sugar.tagGuid

makeArgRow ::
  MonadA m => Widget.Id ->
  (Sugar.TagG Sugar.Name, Sugar.ExpressionN m) ->
  ExprGuiM m [(Grid.Alignment, ExprGuiM.WidgetT m)]
makeArgRow myId (tagG, namedArgExpr) = do
  argTagEdit <- makeTagView myId tagG
  argValEdit <- ExprGuiM.makeSubexpresion 0 namedArgExpr
  pure $ ExpressionGui.makeRow
    [ (0, scaleTag argTagEdit)
    , (0.5, space)
    , (0, argValEdit)
    ]
  where
    scaleTag = ExpressionGui.egWidget %~ Widget.scale Config.fieldTagScale
    space = ExpressionGui.fromValueWidget BWidgets.stdSpaceWidget

boxedApply ::
  MonadA m => Guid ->
  [(Sugar.TagG Sugar.Name, Sugar.ExpressionN m)] ->
  ExpressionGui m ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
boxedApply destGuid args funcWidget =
  ExpressionGui.wrapExpression $ \myId ->
  ExprGuiM.assignCursor myId (WidgetIds.fromGuid destGuid) $ do
    argEdits <-
      Grid.toWidget . Grid.make <$> traverse (makeArgRow myId) args
    pure .
      ExpressionGui.withBgColor Layers.labeledApplyBG
      Config.labeledApplyBGColor (Widget.toAnimId myId ++ ["bg"]) $
      ExpressionGui.addBelow 0 [(0, argEdits)] funcWidget

infixPrecedence :: ExpressionGui.Precedence
infixPrecedence = 5

mkParened ::
  MonadA m => ExpressionGui.ParentPrecedence ->
  Sugar.ExpressionN m -> 
  Sugar.ExpressionN m ->
  Sugar.ExpressionN m ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
mkParened parentPrecedence op l r =
  ExpressionGui.wrapParenify parentPrecedence (ExpressionGui.MyPrecedence infixPrecedence)
  Parens.addHighlightedTextParens $ \myId ->
  (ExprGuiM.assignCursor myId . WidgetIds.fromGuid) (op ^. Sugar.rGuid) $
  ExpressionGui.hboxSpaced <$> sequence
  [ ExprGuiM.makeSubexpresion 5 l
  , mkOpEdit 12 op myId
  , ExprGuiM.makeSubexpresion 5 r
  ]

mkOpEdit ::
  MonadA m => ExpressionGui.Precedence ->
  Sugar.ExpressionN m -> Widget.Id ->
  ExprGuiM m (ExpressionGui m)
mkOpEdit prec op myId = do
  isInfixOp <-
    case mGuid of
    Nothing -> return False
    Just guid -> fmap DataOps.isInfix . ExprGuiM.getP $ Anchors.assocNameRef guid
  if isInfixOp
    then ExprGuiM.makeSubexpresion prec op
    else
      ExpressionGui.hbox <$> sequence
      [ mkBacktick "preBacktick"
      , ExprGuiM.makeSubexpresion prec op
      , mkBacktick "postBacktick"
      ]
  where
    mGuid = funcGuid op
    animId suffix = Widget.toAnimId myId ++ [suffix]
    mkBacktick animLabel =
      ExprGuiM.widgetEnv . fmap ExpressionGui.fromValueWidget .
      BWidgets.makeTextView "`" $ animId animLabel
