{-# LANGUAGE OverloadedStrings #-}

module Lamdu.CodeEdit.ExpressionEdit.LambdaEdit
  ( make, makeParamNameEdit, makeParamEdit
  ) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Maybe (maybeToList)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM, WidgetT)
import qualified Control.Lens as Lens
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.ExpressionEdit.Parens as Parens
import qualified Lamdu.CodeEdit.Settings as Settings
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.Config as Config
import qualified Lamdu.WidgetIds as WidgetIds

paramFDConfig :: FocusDelegator.Config
paramFDConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKeys = [E.ModKey E.noMods E.KeyEnter]
  , FocusDelegator.startDelegatingDoc = E.Doc ["Edit", "Rename parameter"]
  , FocusDelegator.stopDelegatingKeys = [E.ModKey E.noMods E.KeyEsc]
  , FocusDelegator.stopDelegatingDoc = E.Doc ["Edit", "Done renaming"]
  }

makeParamNameEdit ::
  MonadA m =>
  Sugar.Name -> Guid -> Widget.Id ->
  ExprGuiM m (WidgetT m)
makeParamNameEdit name ident =
  ExprGuiM.wrapDelegated paramFDConfig FocusDelegator.NotDelegating id
  (ExprGuiM.withFgColor Config.paramOriginColor .
   ExpressionGui.makeNameEdit name ident)

compose :: [a -> a] -> a -> a
compose = foldr (.) id

-- exported for use in definition sugaring.
makeParamEdit ::
  MonadA m =>
  Widget.Id ->
  Sugar.FuncParam Sugar.Name m (Sugar.ExpressionN m) ->
  ExprGuiM m (ExpressionGui m)
makeParamEdit prevId param =
  (Lens.mapped . ExpressionGui.egWidget %~ Widget.weakerEvents paramEventMap) . assignCursor $ do
    infoMode <- (^. Settings.sInfoMode) <$> ExprGuiM.readSettings
    paramTypeEdit <- ExprGuiM.makeSubexpresion 0 $ param ^. Sugar.fpType
    paramNameEdit <- makeParamNameEdit name (param ^. Sugar.fpGuid) myId
    let typeWidget = paramTypeEdit ^. ExpressionGui.egWidget
    infoWidget <-
      case (infoMode, mActions) of
      (Settings.InfoExamples, Just actions) -> do
        exampleSugar <- ExprGuiM.liftMemoT $ actions ^. Sugar.fpGetExample
        exampleGui <-
          (^. ExpressionGui.egWidget) <$>
          ExprGuiM.makeSubexpresion 0 exampleSugar
        return $ Box.vboxCentered [exampleGui, typeWidget]
      _ -> return typeWidget
    return .
      ExpressionGui.addType ExpressionGui.HorizLine myId [infoWidget] $
      ExpressionGui.fromValueWidget paramNameEdit
  where
    name = param ^. Sugar.fpName
    assignGuidToMe = (`ExprGuiM.assignCursor` myId) . WidgetIds.fromGuid
    sourceIds = param ^. Sugar.fpAltIds ++ maybeToList (param ^. Sugar.fpHiddenLambdaGuid)
    assignCursor = compose $ map assignGuidToMe sourceIds
    myId = WidgetIds.fromGuid $ param ^. Sugar.fpId
    paramEventMap = mconcat
      [ paramDeleteEventMap Config.delForwardKeys "" id
      , paramDeleteEventMap Config.delBackwardKeys " backwards" (const prevId)
      , paramAddNextEventMap
      ]
    mActions = param ^. Sugar.fpMActions
    paramAddNextEventMap =
      maybe mempty
      (Widget.keysEventMapMovesCursor Config.addNextParamKeys (E.Doc ["Edit", "Add next parameter"]) .
       fmap (FocusDelegator.delegatingId . WidgetIds.fromGuid) .
       Lens.view (Sugar.fpListItemActions . Sugar.itemAddNext))
      mActions
    paramDeleteEventMap keys docSuffix onId =
      maybe mempty
      (Widget.keysEventMapMovesCursor keys (E.Doc ["Edit", "Delete parameter" ++ docSuffix]) .
       fmap (onId . WidgetIds.fromGuid) .
       Lens.view (Sugar.fpListItemActions . Sugar.itemDelete))
      mActions

make ::
  MonadA m => ExpressionGui.ParentPrecedence ->
  Sugar.Lam Sugar.Name m (Sugar.ExpressionN m) ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
make parentPrecedence (Sugar.Lam _ param _ body) =
  ExpressionGui.wrapParenify parentPrecedence (ExpressionGui.MyPrecedence 0)
  Parens.addHighlightedTextParens $ \myId ->
  ExprGuiM.assignCursor myId bodyId $ do
    lambdaLabel <-
      ExpressionGui.makeColoredLabel Config.lambdaTextSize Config.lambdaColor "λ" myId
    paramEdit <- makeParamEdit bodyId param
    dotLabel <-
      ExpressionGui.makeColoredLabel Config.rightArrowTextSize Config.rightArrowColor ". " myId
    bodyEdit <- ExprGuiM.makeSubexpresion 0 body
    return $ ExpressionGui.hbox
      [ ExpressionGui.fromValueWidget lambdaLabel
      , paramEdit
      , ExpressionGui.fromValueWidget dotLabel
      , bodyEdit
      ]
  where
    bodyId = WidgetIds.fromGuid $ body ^. Sugar.rGuid
