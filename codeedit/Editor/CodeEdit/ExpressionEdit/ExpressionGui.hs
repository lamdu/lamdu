{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Editor.CodeEdit.ExpressionEdit.ExpressionGui
  ( ExpressionGui(..), atEgWidget, atEgWidgetM
  , Maker
  , fromValueWidget
  , hbox, hboxSpaced
  , addType
  , TypeStyle(..)
  , parenify, wrapExpression, wrapParenify
  ) where

import Control.Lens ((^.))
import Control.Monad (liftM)
import Data.Vector.Vector2 (Vector2(..))
import Editor.CodeEdit.VarAccess (VarAccess, WidgetT)
import Editor.MonadF (MonadF)
import Graphics.UI.Bottle.Widget (R)
import qualified Control.Lens as Lens
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.List as List
import qualified Data.Vector.Vector2 as Vector2
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.Config as Config
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer

data ExpressionGui m = ExpressionGui
  { egWidget :: WidgetT m
  , egAlignment :: R
  }

AtFieldTH.make ''ExpressionGui

atEgWidgetM ::
  Monad m =>
  (WidgetT f -> m (WidgetT f)) ->
  ExpressionGui f -> m (ExpressionGui f)
atEgWidgetM conv (ExpressionGui w a) =
  liftM (`ExpressionGui` a) $ conv w

type Maker m = Sugar.ExpressionRef m -> VarAccess m (ExpressionGui m)

fromValueWidget :: WidgetT m -> ExpressionGui m
fromValueWidget widget = ExpressionGui widget 0.5

hbox :: [ExpressionGui m] -> ExpressionGui m
hbox guis =
  ExpressionGui (Box.toWidget box) $
  case Box.boxContent box of
  ((_, x) : _) -> Grid.elementAlign x ^. Vector2.second
  _ -> error "hbox must not get empty list :("
  where
    box = Box.make Box.horizontal $ map f guis
    f (ExpressionGui widget alignment) = (Vector2 0.5 alignment, widget)

hboxSpaced :: [ExpressionGui m] -> ExpressionGui m
hboxSpaced = hbox . List.intersperse (fromValueWidget BWidgets.spaceWidget)

data TypeStyle = HorizLine | Background

addType ::
  TypeStyle ->
  Widget.Id ->
  [WidgetT m] ->
  ExpressionGui m ->
  ExpressionGui m
addType _ _ [] eg = eg
addType style exprId typeEdits eg =
  ExpressionGui (Box.toWidget box) alignment
  where
    alignment =
      maybe (error "True disappeared from box list?!") (Lens.view Vector2.second . Grid.elementAlign) .
      lookup True $ Box.boxContent box
    box = Box.makeKeyed Box.vertical $
      (True, (Vector2 0.5 (egAlignment eg), widget)) :
      middleElement : [(False, (0.5, typeEdit))]
    middleElement =
      case style of
      HorizLine -> (False, (0.5, Spacer.makeHorizLine underlineId (Vector2 width 1)))
      Background -> (False, (0.5, Spacer.makeWidget 5))
    widget = egWidget eg
    getWidth = Lens.view Vector2.first . Widget.wSize
    width = max (getWidth widget) rawTypeWidth
    rawTypeWidth = getWidth rawTypeEdit
    rawTypeEdit = BWidgets.vboxCentered typeEdits
    typeEdit =
      addBackground .
      (Widget.atWSize . Lens.over Vector2.first . const) width .
      Widget.translate (Vector2 ((width - rawTypeWidth)/2) 0) $ rawTypeEdit
    isError = length typeEdits >= 2
    bgAnimId = Widget.toAnimId exprId ++ ["type background"]
    addBackground = maybe id (Widget.backgroundColor 15 bgAnimId) bgColor
    bgColor
      | isError = Just Config.inferredTypeErrorBGColor
      | otherwise = do
        Background <- Just style
        return Config.inferredTypeBGColor
    underlineId = WidgetIds.underlineId $ Widget.toAnimId exprId

exprFocusDelegatorConfig :: FocusDelegator.Config
exprFocusDelegatorConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKey = Config.enterSubexpressionKey
  , FocusDelegator.startDelegatingDoc = "Enter subexpression"
  , FocusDelegator.stopDelegatingKey = Config.leaveSubexpressionKey
  , FocusDelegator.stopDelegatingDoc = "Leave subexpression"
  }

wrapExpression ::
  (Monad m, MonadF f) =>
  (Widget.Id -> VarAccess m (ExpressionGui f)) ->
  Widget.Id -> VarAccess m (ExpressionGui f)
wrapExpression =
  BWidgets.wrapDelegatedVA exprFocusDelegatorConfig FocusDelegator.Delegating atEgWidget

parenify ::
  Monad m =>
  Sugar.HasParens ->
  (Widget.Id -> ExpressionGui f -> VarAccess m (ExpressionGui f)) ->
  (Widget.Id -> VarAccess m (ExpressionGui f)) ->
  Widget.Id -> VarAccess m (ExpressionGui f)
parenify Sugar.DontHaveParens _ mkWidget myId = mkWidget myId
parenify Sugar.HaveParens addParens mkWidget myId = addParens myId =<< mkWidget myId

wrapParenify ::
  (MonadF f, Monad m) =>
  Sugar.HasParens ->
  (Widget.Id -> ExpressionGui f -> VarAccess m (ExpressionGui f)) ->
  (Widget.Id -> VarAccess m (ExpressionGui f)) ->
  Widget.Id -> VarAccess m (ExpressionGui f)
wrapParenify hasParens addParens =
  wrapExpression . parenify hasParens addParens
