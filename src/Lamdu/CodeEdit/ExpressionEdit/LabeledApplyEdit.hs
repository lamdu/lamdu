{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Lamdu.CodeEdit.ExpressionEdit.LabeledApplyEdit
  ( make
  , PresentationMode(..)
  , assocPresentationMode
  ) where

import Control.Applicative (pure)
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Binary (Binary(..), getWord8, putWord8)
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Monoid (mappend)
import Data.Store.Guid (Guid)
import Data.Traversable (traverse)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid
import qualified Lamdu.BottleWidgets as BWidgets
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.ExpressionEdit.TagEdit as TagEdit
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Layers as Layers
import qualified Lamdu.WidgetIds as WidgetIds

data PresentationMode = OO | Verbose
  deriving (Eq, Ord, Enum, Bounded, Show)

assocPresentationMode ::
  MonadA m => Guid -> Transaction.MkProperty m PresentationMode
assocPresentationMode = Transaction.assocDataRefDef OO "PresentationMode"

funcGuid :: Sugar.ExpressionP name m pl -> Maybe Guid
funcGuid f =
  case f ^. Sugar.rBody of
  Sugar.BodyGetVar gv -> Just $ gv ^. Sugar.gvIdentifier
  Sugar.BodyCollapsed c -> funcGuid $ c ^. Sugar.pFullExpression
  Sugar.BodyApply a -> funcGuid $ a ^. Expr.applyFunc
  _ -> Nothing

getPresentationMode :: MonadA m => Sugar.ExpressionN m -> ExprGuiM m PresentationMode
getPresentationMode func =
  case funcGuid func of
  Just guid ->
    ExprGuiM.transaction . Transaction.getP $ assocPresentationMode guid
  Nothing -> return Verbose

make ::
  MonadA m =>
  Sugar.LabeledApply Sugar.Name (Sugar.ExpressionN m) ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
make (Sugar.LabeledApply func args) =
  ExpressionGui.wrapExpression $ \myId -> do
    presentationMode <- getPresentationMode func
    ExprGuiM.assignCursor myId (WidgetIds.fromGuid (func ^. Sugar.rGuid)) $ do
      funcEdit <- ExprGuiM.makeSubexpresion 10 func
      let
        makeArg (tagG, namedArgExpr) = do
          let tagGuid = tagG ^. Sugar.tagGuid
          argTagEdit <-
            TagEdit.makeView tagG .
            Widget.toAnimId . mappend myId $
            WidgetIds.fromGuid tagGuid
          argValEdit <- ExprGuiM.makeSubexpresion 0 namedArgExpr
          pure $ ExpressionGui.makeRow
            [ (0, scaleTag argTagEdit)
            , (0.5, space)
            , (0, argValEdit)
            ]
      argEdits <-
        traverse makeArg $
        case presentationMode of
        Verbose -> args
        OO -> tail args
      let
        argEditsGrid = Grid.toWidget $ Grid.make argEdits
      topEdit <-
        case presentationMode of
        Verbose -> return funcEdit
        OO ->
          case args of
          (_, firstArg) : _ -> do
            firstArgEdit <- ExprGuiM.makeSubexpresion 11 firstArg
            return $ ExpressionGui.hboxSpaced [funcEdit, firstArgEdit]
          _ -> error "LabeledApplyEdit must take arg list of at least 2 elements"
      pure .
        ExpressionGui.withBgColor Layers.labeledApplyBG
        Config.labeledApplyBGColor (Widget.toAnimId myId ++ ["bg"]) $
        ExpressionGui.addBelow 0 [(0, argEditsGrid)] topEdit
  where
    space = ExpressionGui.fromValueWidget BWidgets.stdSpaceWidget
    scaleTag = ExpressionGui.egWidget %~ Widget.scale Config.fieldTagScale

derive makeBinary ''PresentationMode
