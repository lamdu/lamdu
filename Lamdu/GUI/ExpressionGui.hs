{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionGui
  ( ExpressionGui(..), egWidget
  , fromValueWidget
  , scaleFromTop
  , hbox, hboxSpaced, addBelow
  , addType -- TODO: s/type/info
  , TypeStyle(..)
  , MyPrecedence(..), ParentPrecedence(..), Precedence
  , parenify, wrapParenify
  , wrapExpression, wrapDelegated
  -- ExprGuiM:
  , makeNameEdit
  , withBgColor
  -- TODO: Maybe move to ExpressionGui.Collapser:
  , Collapser(..), makeCollapser
  , makeLabel, makeColoredLabel
  , makeFocusableView
  , makeRow
  , makeNameView
  , addInferredTypes
  , truncateSize
  ) where

import Control.Applicative ((<$>))
import Control.Lens (Lens')
import Control.Lens.Operators
import Control.Monad ((<=<))
import Control.MonadA (MonadA)
import Data.Function (on)
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Data.Vector.Vector2 (Vector2(..))
import Graphics.UI.Bottle.Animation (AnimId, Layer)
import Graphics.UI.Bottle.Widget (Widget)
import Graphics.UI.Bottle.Widgets.Box (KBox)
import Lamdu.Config (Config)
import Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import Lamdu.GUI.ExpressionGui.Types (WidgetT, MyPrecedence(..), ParentPrecedence(..), Precedence, ExpressionGui(..), egWidget, egAlignment)
import qualified Control.Lens as Lens
import qualified Data.List as List
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.EventMap as EventMap
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

fromValueWidget :: WidgetT m -> ExpressionGui m
fromValueWidget widget = ExpressionGui widget 0.5

-- | Scale the given ExpressionGui without moving its top alignment
-- point:
scaleFromTop :: Vector2 Widget.R -> ExpressionGui m -> ExpressionGui m
scaleFromTop ratio (ExpressionGui widget alignment) =
  ExpressionGui (Widget.scale ratio widget) (alignment / (ratio ^. Lens._2))

truncateSize :: Vector2 Widget.R -> ExpressionGui m -> ExpressionGui m
truncateSize newSize (ExpressionGui widget alignment) =
  ExpressionGui
  (widget & Widget.wSize .~ newSize)
  (alignment * (widget ^. Widget.wSize / newSize) ^. Lens._2)

hbox :: [ExpressionGui m] -> ExpressionGui m
hbox guis =
  ExpressionGui (Box.toWidget box) $
  case box ^. Box.boxContent of
  ((_, x) : _) -> x ^. Grid.elementAlign . Lens._2
  _ -> error "hbox must not get empty list :("
  where
    box = Box.make Box.horizontal $ map f guis
    f (ExpressionGui widget alignment) = (Vector2 0.5 alignment, widget)

hboxSpaced :: [ExpressionGui m] -> ExpressionGui m
hboxSpaced = hbox . List.intersperse (fromValueWidget BWidgets.stdSpaceWidget)

fromBox :: KBox Bool (Transaction m) -> ExpressionGui m
fromBox box =
  ExpressionGui (Box.toWidget box) alignment
  where
    alignment =
      maybe (error "True disappeared from box list?!")
        (^. Grid.elementAlign . Lens._2) .
      lookup True $ box ^. Box.boxContent

addBelow ::
  Widget.R ->
  [(Box.Alignment, WidgetT m)] ->
  ExpressionGui m ->
  ExpressionGui m
addBelow egHAlign ws eg =
  fromBox . Box.makeKeyed Box.vertical $
  (True, (Vector2 egHAlign (eg ^. egAlignment), eg ^. egWidget)) :
  map ((,) False) ws

data TypeStyle = HorizLine | Background

wWidth :: Lens' (Widget f) Widget.R
wWidth = Widget.wSize . Lens._1

addType ::
  Config ->
  TypeStyle ->
  Widget.Id ->
  [WidgetT m] ->
  ExpressionGui m ->
  ExpressionGui m
addType _ _ _ [] eg = eg
addType config style exprId typeEdits eg =
  addBelow 0.5 items eg
  where
    items = middleElement : [(0.5, annotatedTypes)]
    middleElement =
      case style of
      HorizLine -> (0.5, Spacer.makeHorizLine underlineId (Vector2 width 1))
      Background -> (0.5, Spacer.makeWidget 5)
    annotatedTypes =
      addBackground . (wWidth .~ width) $
      Widget.translate (Vector2 ((width - typesBox ^. wWidth)/2) 0) typesBox
    width = on max (^. wWidth) (eg ^. egWidget) typesBox
    typesBox = Box.vboxCentered typeEdits
    isError = length typeEdits >= 2
    bgAnimId = Widget.toAnimId exprId ++ ["type background"]
    addBackground = maybe id (Widget.backgroundColor (Config.layerTypes (Config.layers config)) bgAnimId) bgColor
    bgColor
      | isError = Just $ Config.inferredTypeErrorBGColor config
      | otherwise = do
        Background <- Just style
        return $ Config.inferredTypeBGColor config
    underlineId = WidgetIds.underlineId $ Widget.toAnimId exprId

exprFocusDelegatorConfig :: Config -> FocusDelegator.Config
exprFocusDelegatorConfig config = FocusDelegator.Config
  { FocusDelegator.startDelegatingKeys = Config.enterSubexpressionKeys config
  , FocusDelegator.startDelegatingDoc = EventMap.Doc ["Navigation", "Enter subexpression"]
  , FocusDelegator.stopDelegatingKeys = Config.leaveSubexpressionKeys config
  , FocusDelegator.stopDelegatingDoc = EventMap.Doc ["Navigation", "Leave subexpression"]
  }

-- ExprGuiM GUIs (TODO: Move to Monad.hs?)

disallowedNameChars :: [(Char, EventMap.IsShifted)]
disallowedNameChars =
  EventMap.anyShiftedChars "[]\\`()" ++
  [ ('0', EventMap.Shifted)
  , ('9', EventMap.Shifted)
  ]

makeBridge ::
  MonadA m =>
  (Widget.Id -> WE.WidgetEnvT m (Widget f)) ->
  (Widget.Id -> WE.WidgetEnvT m (Widget f)) ->
  Widget.Id -> WE.WidgetEnvT m (Widget f)
makeBridge mkFocused mkUnfocused myId = do
  isFocused <- WE.isSubCursor myId
  (if isFocused then mkFocused else mkUnfocused) myId

nameSrcTint :: Config -> Sugar.NameSource -> Widget f -> Widget f
nameSrcTint config Sugar.AutoGeneratedName = Widget.tint $ Config.autoGeneratedNameTint config
nameSrcTint _ Sugar.StoredName = id

makeNameEdit ::
  MonadA m => Sugar.Name -> Guid -> Widget.Id -> ExprGuiM m (WidgetT m)
makeNameEdit (Sugar.Name nameSrc nameCollision name) ident myId = do
  nameProp <- ExprGuiM.transaction . (^. Transaction.mkProperty) $ Anchors.assocNameRef ident
  collisionSuffixes <-
    ExprGuiM.widgetEnv . makeCollisionSuffixLabels nameCollision $
    Widget.toAnimId myId
  config <- ExprGuiM.widgetEnv WE.readConfig
  nameEdit <-
    fmap (nameSrcTint config nameSrc) .
    ExprGuiM.widgetEnv .
    WE.localEnv (WE.envTextStyle . TextEdit.sEmptyFocusedString .~ bracketedName) $
    makeEditor nameProp
  return . Box.hboxCentered $ nameEdit : collisionSuffixes
  where
    bracketedName = concat ["<", name, ">"]
    makeEditor property =
      makeBridge (makeWordEdit property) (BWidgets.makeFocusableTextView name) myId
    makeWordEdit =
      BWidgets.makeWordEdit <&>
      Lens.mapped . Lens.mapped . Widget.wEventMap %~
      EventMap.filterSChars (curry (`notElem` disallowedNameChars))

wrapDelegated ::
  MonadA m =>
  Sugar.Payload Sugar.Name m a ->
  FocusDelegator.Config -> FocusDelegator.IsDelegating ->
  (Widget.Id -> ExprGuiM m (ExpressionGui m)) ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
wrapDelegated pl fdConfig isDelegating f =
  addInferredTypes pl <=<
  ExprGuiM.wrapDelegated fdConfig isDelegating (egWidget %~) f

wrapExpression ::
  MonadA m =>
  Sugar.Payload Sugar.Name m a ->
  (Widget.Id -> ExprGuiM m (ExpressionGui m)) ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
wrapExpression pl f myId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  wrapDelegated pl (exprFocusDelegatorConfig config) FocusDelegator.Delegating f myId

makeLabel ::
  MonadA m => String -> Widget.Id -> ExprGuiM m (WidgetT f)
makeLabel text myId = ExprGuiM.widgetEnv . BWidgets.makeLabel text $ Widget.toAnimId myId

makeColoredLabel ::
  MonadA m => Int -> Draw.Color -> String -> Widget.Id -> ExprGuiM m (WidgetT f)
makeColoredLabel textSize color text myId =
  ExprGuiM.localEnv (WE.setTextSizeColor textSize color) $
  makeLabel text myId

makeFocusableView ::
  (MonadA m, MonadA n) => Widget.Id -> ExpressionGui n -> ExprGuiM m (ExpressionGui n)
makeFocusableView myId gui =
  ExprGuiM.widgetEnv $
  egWidget (BWidgets.makeFocusableView myId) gui

parenify ::
  MonadA m =>
  ParentPrecedence -> MyPrecedence ->
  (Widget.Id -> ExpressionGui f -> ExprGuiM m (ExpressionGui f)) ->
  (Widget.Id -> ExprGuiM m (ExpressionGui f)) ->
  Widget.Id -> ExprGuiM m (ExpressionGui f)
parenify (ParentPrecedence parent) (MyPrecedence prec) addParens mkWidget myId
  | parent > prec = addParens myId =<< mkWidget myId
  | otherwise = mkWidget myId

wrapParenify ::
  MonadA m =>
  Sugar.Payload Sugar.Name m a ->
  ParentPrecedence -> MyPrecedence ->
  (Widget.Id -> ExpressionGui m -> ExprGuiM m (ExpressionGui m)) ->
  (Widget.Id -> ExprGuiM m (ExpressionGui m)) ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
wrapParenify pl parentPrec prec addParens =
  wrapExpression pl . parenify parentPrec prec addParens

withBgColor :: Layer -> Draw.Color -> AnimId -> ExpressionGui m -> ExpressionGui m
withBgColor layer color animId =
  egWidget %~ Widget.backgroundColor layer animId color

data Collapser m = Collapser
  { cMakeExpanded :: ExprGuiM m (ExpressionGui m)
  , cMakeFocusedCompact :: ExprGuiM m (ExpressionGui m)
  }

makeCollapser ::
  MonadA m =>
  FocusDelegator.Config ->
  (Widget.Id -> Collapser m) ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
makeCollapser fdConfig f =
  ExprGuiM.wrapDelegated fdConfig FocusDelegator.NotDelegating (egWidget %~) $
  \myId -> do
    let Collapser makeExpanded makeFocusedCompact = f myId
    -- TODO: This is just to detect whether cursor is in the full
    -- expression.  Even when it's not displayed, which may be wasteful
    -- (even with laziness, at least the names are going to be read).
    expandedEdit <- makeExpanded
    -- We are inside a focus delegator, so if the cursor is on us it
    -- means user entered our widget.
    if expandedEdit ^. egWidget . Widget.wIsFocused
      then return expandedEdit
      else makeFocusedCompact

makeRow :: [(Widget.R, ExpressionGui m)] -> [(Vector2 Widget.R, WidgetT m)]
makeRow =
  map item
  where
    item (halign, ExpressionGui widget alignment) =
      (Vector2 halign alignment, widget)

makeNameView :: MonadA m => Sugar.Name -> AnimId -> WE.WidgetEnvT m (Widget f)
makeNameView (Sugar.Name nameSrc collision name) animId = do
  label <- BWidgets.makeLabel name animId
  suffixLabels <- makeCollisionSuffixLabels collision $ animId ++ ["suffix"]
  config <- WE.readConfig
  return .
    nameSrcTint config nameSrc .
    Box.hboxCentered $ label : suffixLabels

makeCollisionSuffixLabels ::
  MonadA m => Sugar.NameCollision -> AnimId -> WE.WidgetEnvT m [Widget f]
makeCollisionSuffixLabels Sugar.NoCollision _ = return []
makeCollisionSuffixLabels (Sugar.Collision suffix) animId = do
  config <- WE.readConfig
  let
    onSuffixWidget =
      Widget.backgroundColor (Config.layerNameCollisionBG (Config.layers config))
        (animId ++ ["bg"]) (Config.collisionSuffixBGColor config) .
      Widget.scale (realToFrac <$> Config.collisionSuffixScaleFactor config)
  BWidgets.makeLabel (show suffix) animId
    & (WE.localEnv . WE.setTextColor . Config.collisionSuffixTextColor) config
    <&> (:[]) . onSuffixWidget

addInferredTypes ::
  MonadA m =>
  Sugar.Payload Sugar.Name m a ->
  ExpressionGui m ->
  ExprGuiM m (ExpressionGui m)
addInferredTypes exprPl eg = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  typeEdits <-
    exprPl ^. Sugar.plInferredTypes
    & Lens.traversed . Lens.mapped . Lens.mapped .~
      ExprGuiM.emptyPayload
    & Lens.traversed (ExprGuiM.makeSubexpression 0)
    <&>
      map
      ( Widget.tint (Config.inferredTypeTint config)
      . Widget.scale (realToFrac <$> Config.typeScaleFactor config)
      . (^. egWidget)
      )
  return $ addType config Background exprId typeEdits eg
  where
    exprId = WidgetIds.fromGuid $ exprPl ^. Sugar.plGuid
