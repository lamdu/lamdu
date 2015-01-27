module Lamdu.GUI.ExpressionEdit.ApplyEdit
  ( make
  ) where

import           Control.Applicative (Applicative(..))
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.MonadA (MonadA)
import           Data.Store.Transaction (Transaction)
import           Data.Traversable (traverse, sequenceA)
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid
import qualified Graphics.UI.Bottle.Widgets.Layout as Layout
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import qualified Lamdu.GUI.ExpressionEdit.TagEdit as TagEdit
import           Lamdu.GUI.ExpressionGui (ExpressionGui, ParentPrecedence(..))
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.AddNames.Types (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction

infixPrecedence :: ExpressionGui.Precedence
infixPrecedence = 5

prefixPrecedence :: ExpressionGui.Precedence
prefixPrecedence = 10

mkOverrideModifyEventMap ::
  MonadA m => Maybe (Sugar.Actions m) ->
  ExprGuiM m (ExpressionGui m -> ExpressionGui m)
mkOverrideModifyEventMap Nothing = return id
mkOverrideModifyEventMap (Just actions) =
  do
    config <- ExprGuiM.widgetEnv WE.readConfig
    ExpressionGui.egWidget %~
      Widget.strongerEvents (ExprEventMap.modifyEventMap [] config actions)
      & return

make ::
  MonadA m => ParentPrecedence ->
  Sugar.Apply (Name m) (ExprGuiM.SugarExpr m) ->
  Sugar.Payload m ExprGuiM.Payload ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
make (ParentPrecedence parentPrecedence) (Sugar.Apply func specialArgs annotatedArgs) pl =
  ExpressionGui.stdWrapParentExpr pl $ \myId -> do
    let
      mk mPrecedence mkFuncRow
        | isBoxed = mkBoxed mkFuncRow annotatedArgs myId
        | otherwise =
          mkMParened
          (parentPrecedence & ParentPrecedence)
          (mPrecedence <&> ExpressionGui.MyPrecedence) mkFuncRow myId
    overrideModifyEventMap <- mkOverrideModifyEventMap (pl ^. Sugar.plActions)
    case specialArgs of
      Sugar.NoSpecialArgs ->
        ExprGuiM.makeSubexpression (if isBoxed then 0 else parentPrecedence) func
        <&> overrideModifyEventMap
        & mk Nothing
      Sugar.ObjectArg arg ->
        sequenceA
        [ ExprGuiM.makeSubexpression (prefixPrecedence+1) func
          <&> if null annotatedArgs
              then id
              else overrideModifyEventMap
        , ExprGuiM.makeSubexpression prefixPrecedence arg
        ]
        >>= ExpressionGui.hboxSpaced
        & mk (Just prefixPrecedence)
      Sugar.InfixArgs l r ->
        sequenceA
        [ ExprGuiM.makeSubexpression (infixPrecedence+1) l
        , -- TODO: What precedence to give when it must be atomic?:
          ExprGuiM.makeSubexpression 20 func <&> overrideModifyEventMap
        , ExprGuiM.makeSubexpression (infixPrecedence+1) r
        ]
        >>= ExpressionGui.hboxSpaced
        & mk (Just infixPrecedence)
    & ExprGuiM.assignCursor myId funcId
  where
    funcId = func ^. Sugar.rPayload . Sugar.plEntityId & WidgetIds.fromEntityId
    isBoxed = not $ null annotatedArgs

makeArgRows ::
  MonadA m =>
  Sugar.AnnotatedArg (Name m) (ExprGuiM.SugarExpr m) ->
  ExprGuiM m [[(Grid.Alignment, Widget (T m))]]
makeArgRows arg = do
  argTagEdit <- TagEdit.makeParamTag (arg ^. Sugar.aaTag)
  argValEdit <- ExprGuiM.makeSubexpression 0 $ arg ^. Sugar.aaExpr
  vspace <- ExprGuiM.widgetEnv BWidgets.verticalSpace
  space <- ExprGuiM.widgetEnv BWidgets.stdSpaceWidget
  pure
    [ replicate 3 (0.5, vspace)
    , [ argTagEdit ^. Layout.alignedWidget & _1 . _1 .~ 0
      , (0.5, space)
      , argValEdit ^. Layout.alignedWidget & _1 . _1 .~ 0
      ]
    ]

mkBoxed ::
  MonadA m =>
  ExprGuiM m (ExpressionGui m) ->
  [Sugar.AnnotatedArg (Name m) (ExprGuiM.SugarExpr m)] ->
  Widget.Id ->
  ExprGuiM m (ExpressionGui m)
mkBoxed mkFuncRow annotatedArgs myId =
  do
    grid <-
      annotatedArgs
      & traverse makeArgRows
      <&> Grid.toWidget . Grid.make . concat
    mkFuncRow
      <&> ExpressionGui.addBelow 0 [(0, grid)]
      >>= ExpressionGui.addValFrame myId

mkMParened ::
  MonadA m =>
  ParentPrecedence ->
  Maybe ExpressionGui.MyPrecedence ->
  ExprGuiM m  (ExpressionGui m) ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
mkMParened parentPrecedence mPrecedence mkFuncRow =
  const mkFuncRow
  & case mPrecedence of
    Nothing -> id
    Just precedence -> ExpressionGui.parenify parentPrecedence precedence
