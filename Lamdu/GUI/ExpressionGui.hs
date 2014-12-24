{-# LANGUAGE OverloadedStrings, RankNTypes #-}
module Lamdu.GUI.ExpressionGui
  ( ExpressionGui(..), egWidget, egAlignment
  -- General:
  , fromValueWidget
  , scaleFromTop
  , pad
  , hbox, hboxSpaced, addBelow
  , makeRow
  -- Lifted widgets:
  , makeLabel
  , makeFocusableView
  , makeNameView
  , makeNameEdit
  -- Info adding
  , addType
  -- Expression wrapping
  , MyPrecedence(..), ParentPrecedence(..), Precedence
  , parenify
  -- | stdWrap/stdPostProcess means addTypes
  , stdWrap
  , stdWrapDelegated
  , stdWrapParentExpr
  , stdWrapParenify
  , addInferredTypes, addTypeBackground
  ) where

import Control.Applicative ((<$>))
import Control.Lens (Lens')
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Monoid (Monoid(..))
import Data.Store.Property (Property(..))
import Data.Store.Transaction (Transaction)
import Data.Vector.Vector2 (Vector2(..))
import Graphics.UI.Bottle.Animation (AnimId)
import Graphics.UI.Bottle.View (View)
import Graphics.UI.Bottle.Widget (Widget)
import Graphics.UI.Bottle.Widgets.Box (KBox)
import Lamdu.Config (Config)
import Lamdu.GUI.ExpressionGui.Monad (ExprGuiM, HolePickers)
import Lamdu.GUI.ExpressionGui.Types (WidgetT, ExpressionGui(..), egWidget, egAlignment)
import Lamdu.GUI.Precedence (MyPrecedence(..), ParentPrecedence(..), Precedence)
import Lamdu.Sugar.AddNames.Types (Name(..), NameSource(..), NameCollision(..))
import qualified Control.Lens as Lens
import qualified Data.List as List
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as EventMap
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.TypeView as TypeView
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

pad :: Vector2 Widget.R -> ExpressionGui m -> ExpressionGui m
pad padding (ExpressionGui widget alignment) =
  ExpressionGui newWidget $
  (padding ^. Lens._2 + alignment * widget ^. height) / newWidget ^. height
  where
    height = Widget.wSize . Lens._2
    newWidget = Widget.pad padding widget

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

wWidth :: Lens' (Widget f) Widget.R
wWidth = Widget.wSize . Lens._1

addTypeBackground :: Config -> AnimId -> Widget.R -> View -> View
addTypeBackground config animId minWidth typeView =
  typeView
  & Lens._1 .~ newSize
  & Lens._2 %~ Anim.translate (Vector2 ((width - typeWidth) / 2) 0)
  & Lens._2 %~ Anim.backgroundColor bgAnimId bgLayer bgColor newSize
  where
    typeWidth = typeView ^. Lens._1 . Lens._1
    width = max typeWidth minWidth
    newSize = typeView ^. Lens._1 & Lens._1 .~ width
    bgAnimId = animId ++ ["type background"]
    bgLayer = Config.layerTypes $ Config.layers config
    bgColor = Config.typeBoxBGColor config

addType :: Config -> Widget.Id -> View -> ExpressionGui m -> ExpressionGui m
addType config exprId typeView eg =
  addBelow 0.5 items eg
  where
    items =
      [ (0.5, Spacer.makeWidget 5)
      , (0.5, annotatedTypes)
      ]
    annotatedTypes =
      typeView
      & addTypeBackground config (Widget.toAnimId exprId) (eg ^. egWidget . wWidth)
      & uncurry Widget.liftView

parentExprFDConfig :: Config -> FocusDelegator.Config
parentExprFDConfig config = FocusDelegator.Config
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

nameSrcTint :: Config -> NameSource -> Widget f -> Widget f
nameSrcTint config NameSourceAutoGenerated = Widget.tint $ Config.autoGeneratedNameTint config
nameSrcTint _ NameSourceStored = id

makeNameEdit ::
  MonadA m => Name m -> Widget.Id -> ExprGuiM m (WidgetT m)
makeNameEdit (Name nameSrc nameCollision setName name) myId = do
  collisionSuffixes <-
    ExprGuiM.widgetEnv . makeCollisionSuffixLabels nameCollision $
    Widget.toAnimId myId
  config <- ExprGuiM.widgetEnv WE.readConfig
  nameEdit <-
    fmap (nameSrcTint config nameSrc) .
    ExprGuiM.widgetEnv .
    WE.localEnv (WE.envTextStyle . TextEdit.sEmptyFocusedString .~ "") $
    makeEditor nameProp
  return . Box.hboxCentered $ nameEdit : collisionSuffixes
  where
    nameProp = Property storedName setName
    storedName =
      case nameSrc of
      NameSourceAutoGenerated -> ""
      NameSourceStored -> name
    makeEditor property =
      makeBridge (makeWordEdit property) (BWidgets.makeFocusableTextView name) myId
    makeWordEdit =
      BWidgets.makeWordEdit <&>
      Lens.mapped . Lens.mapped . Widget.wEventMap %~
      EventMap.filterSChars (curry (`notElem` disallowedNameChars))

stdWrap ::
  MonadA m => Sugar.Payload m ExprGuiM.Payload ->
  ExprGuiM m (ExpressionGui m) ->
  ExprGuiM m (ExpressionGui m)
stdWrap pl mkGui = wrapExprEventMap pl $ addInferredTypes pl =<< mkGui

stdWrapDelegated ::
  MonadA m =>
  Sugar.Payload m ExprGuiM.Payload ->
  FocusDelegator.Config -> FocusDelegator.IsDelegating ->
  (Widget.Id -> ExprGuiM m (ExpressionGui m)) ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
stdWrapDelegated pl fdConfig isDelegating f =
  stdWrap pl . ExprGuiM.wrapDelegated fdConfig isDelegating (egWidget %~) f

stdWrapParentExpr ::
  MonadA m =>
  Sugar.Payload m ExprGuiM.Payload ->
  (Widget.Id -> ExprGuiM m (ExpressionGui m)) ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
stdWrapParentExpr pl f myId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  stdWrapDelegated pl (parentExprFDConfig config) FocusDelegator.Delegating f myId

makeLabel ::
  MonadA m => String -> Widget.Id -> ExprGuiM m (WidgetT f)
makeLabel text myId = ExprGuiM.widgetEnv . BWidgets.makeLabel text $ Widget.toAnimId myId

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

stdWrapParenify ::
  MonadA m =>
  Sugar.Payload m ExprGuiM.Payload ->
  ParentPrecedence -> MyPrecedence ->
  (Widget.Id -> ExpressionGui m -> ExprGuiM m (ExpressionGui m)) ->
  (Widget.Id -> ExprGuiM m (ExpressionGui m)) ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
stdWrapParenify pl parentPrec prec addParens =
  stdWrapParentExpr pl . parenify parentPrec prec addParens

makeRow :: [(Widget.R, ExpressionGui m)] -> [(Vector2 Widget.R, WidgetT m)]
makeRow =
  map item
  where
    item (halign, ExpressionGui widget alignment) =
      (Vector2 halign alignment, widget)

makeNameView ::
  MonadA m =>
  Name m -> AnimId -> WE.WidgetEnvT (Transaction m) (Widget f)
makeNameView (Name nameSrc collision _ name) animId = do
  label <- BWidgets.makeLabel name animId
  suffixLabels <- makeCollisionSuffixLabels collision $ animId ++ ["suffix"]
  config <- WE.readConfig
  return .
    nameSrcTint config nameSrc .
    Box.hboxCentered $ label : suffixLabels

makeCollisionSuffixLabels ::
  MonadA m => NameCollision -> AnimId -> WE.WidgetEnvT m [Widget f]
makeCollisionSuffixLabels NoCollision _ = return []
makeCollisionSuffixLabels (Collision suffix) animId = do
  config <- WE.readConfig
  let
    onSuffixWidget =
      Widget.backgroundColor (Config.layerNameCollisionBG (Config.layers config))
        (animId ++ ["bg"]) (Config.collisionSuffixBGColor config) .
      Widget.scale (realToFrac <$> Config.collisionSuffixScaleFactor config)
  BWidgets.makeLabel (show suffix) animId
    & (WE.localEnv . WE.setTextColor . Config.collisionSuffixTextColor) config
    <&> (:[]) . onSuffixWidget

wrapExprEventMap ::
  MonadA m =>
  Sugar.Payload m ExprGuiM.Payload ->
  ExprGuiM m (ExpressionGui m) -> ExprGuiM m (ExpressionGui m)
wrapExprEventMap pl action = do
  (res, resultPickers) <- ExprGuiM.listenResultPickers action
  addExprEventMap pl resultPickers res

addExprEventMap ::
  MonadA m =>
  Sugar.Payload m ExprGuiM.Payload -> HolePickers m ->
  ExpressionGui m -> ExprGuiM m (ExpressionGui m)
addExprEventMap pl resultPickers gui = do
  exprEventMap <-
    ExprEventMap.make (gui ^. egWidget . Widget.wIsFocused)
    resultPickers pl
  gui & egWidget %~ Widget.weakerEvents exprEventMap & return

alwaysAddInferredTypes ::
  MonadA m =>
  Sugar.Payload m a ->
  ExpressionGui m ->
  ExprGuiM m (ExpressionGui m)
alwaysAddInferredTypes exprPl eg =
  do
    config <- ExprGuiM.widgetEnv WE.readConfig
    typeView <-
      exprPl ^. Sugar.plInferredType
      & TypeView.make (mappend (Widget.toAnimId exprId) ["type"])
      & ExprGuiM.widgetEnv
    return $ addType config exprId typeView eg
  where
    entityId = exprPl ^. Sugar.plEntityId
    exprId = WidgetIds.fromEntityId entityId

addInferredTypes ::
  MonadA m =>
  Sugar.Payload m ExprGuiM.Payload ->
  ExpressionGui m ->
  ExprGuiM m (ExpressionGui m)
addInferredTypes exprPl eg =
  do
    s <- ExprGuiM.shouldShowType $ exprPl ^. Sugar.plData . ExprGuiM.plShowType
    if s
      then alwaysAddInferredTypes exprPl eg
      else return eg
