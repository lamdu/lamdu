{-# LANGUAGE RecordWildCards, OverloadedStrings, RankNTypes #-}
module Lamdu.GUI.ExpressionGui
  ( ExpressionGui, egWidget, egAlignment
  -- General:
  , fromValueWidget, addBelow, addAbove
  , scaleFromTop
  , pad
  , hbox, hboxSpaced
  , vboxTopFocal, vboxTopFocalSpaced
  , gridTopLeftFocal
  , listWithDelDests
  , makeLabel
  , grammarLabel
  , addValBG, addValFrame
  -- Lifted widgets:
  , makeFocusableView
  , makeNameView
  , makeNameEdit
  , makeNameOriginEdit
  , diveToNameEdit
  -- Info adding
  , addBelowInferredSpacing
  , maybeAddInferredType
  , makeTypeView
  -- Expression wrapping
  , MyPrecedence(..), ParentPrecedence(..), Precedence
  , parenify
  , wrapExprEventMap
  , stdWrap
  , stdWrapIn
  , stdWrapParentExpr
  , stdWrapParenify
  ) where

import           Control.Applicative ((<$>))
import           Control.Lens (Lens, Lens')
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.MonadA (MonadA)
import qualified Data.List as List
import qualified Data.List.Utils as ListUtils
import           Data.Monoid (Monoid(..))
import           Data.Store.Property (Property(..))
import           Data.Store.Transaction (Transaction)
import           Data.Vector.Vector2 (Vector2(..))
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.ModKey (ModKey(..))
import           Graphics.UI.Bottle.View (View)
import qualified Graphics.UI.Bottle.View as View
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.Layout as Layout
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.GLFW as GLFW
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.Expr.Type (Type)
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM, HolePickers)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import           Lamdu.GUI.ExpressionGui.Types (ExpressionGui)
import qualified Lamdu.GUI.Parens as Parens
import           Lamdu.GUI.Precedence (MyPrecedence(..), ParentPrecedence(..), Precedence)
import qualified Lamdu.GUI.TypeView as TypeView
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.AddNames.Types (Name(..), NameSource(..), NameCollision(..))
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction

{-# INLINE egWidget #-}
egWidget ::
  Lens (ExpressionGui m) (ExpressionGui n) (Widget (T m)) (Widget (T n))
egWidget = Layout.alignedWidget . _2

{-# INLINE egAlignment #-}
egAlignment :: Lens' (ExpressionGui m) Layout.Alignment
egAlignment = Layout.alignedWidget . _1

fromValueWidget :: Widget (T m) -> ExpressionGui m
fromValueWidget = Layout.fromCenteredWidget

alignAdd ::
    ([(Widget.R, Widget (T m))] -> ExpressionGui m -> ExpressionGui m) ->
    Widget.R -> [(Widget.R, Widget (T m))] ->
    ExpressionGui m -> ExpressionGui m
alignAdd addFunc hAlign widgets eg =
  eg & egAlignment . _1 .~ hAlign & addFunc widgets

addAbove ::
    Widget.R -> [(Widget.R, Widget (T m))] ->
    ExpressionGui m -> ExpressionGui m
addAbove = alignAdd (Layout.addBefore Layout.Vertical)

addBelow ::
    Widget.R -> [(Widget.R, Widget (T m))] ->
    ExpressionGui m -> ExpressionGui m
addBelow = alignAdd (Layout.addAfter Layout.Vertical)

-- | Scale the given ExpressionGui without moving its top alignment
-- point:
scaleFromTop :: Vector2 Widget.R -> ExpressionGui m -> ExpressionGui m
scaleFromTop = Layout.scaleFromTopLeft

pad :: Vector2 Widget.R -> ExpressionGui m -> ExpressionGui m
pad = Layout.pad

hbox :: [ExpressionGui m] -> ExpressionGui m
hbox = Layout.hbox 0.5

hboxSpaced :: MonadA m => [ExpressionGui f] -> ExprGuiM m (ExpressionGui f)
hboxSpaced guis =
  ExprGuiM.widgetEnv BWidgets.stdSpaceWidget
  <&> Layout.fromCenteredWidget
  <&> (`List.intersperse` guis)
  <&> hbox

vboxTopFocal :: [ExpressionGui m] -> ExpressionGui m
vboxTopFocal [] = Layout.empty
vboxTopFocal (gui:guis) = gui & Layout.addAfter Layout.Vertical guis

vboxTopFocalSpaced ::
  MonadA m => [ExpressionGui m] -> ExprGuiM m (ExpressionGui m)
vboxTopFocalSpaced guis =
  do
    space <- ExprGuiM.widgetEnv BWidgets.verticalSpace
    guis & List.intersperse (Layout.fromCenteredWidget space)
      & vboxTopFocal & return

gridTopLeftFocal :: [[ExpressionGui m]] -> ExpressionGui m
gridTopLeftFocal = Layout.gridTopLeftFocal

wWidth :: Lens' (Widget f) Widget.R
wWidth = Widget.wWidth

addTypeBackground :: Config -> AnimId -> Widget.R -> View -> View
addTypeBackground config animId minWidth typeView =
  typeView
  & View.size .~ newSize
  & View.animFrame %~ Anim.translate (Vector2 ((width - typeWidth) / 2) 0)
  & View.backgroundColor bgAnimId bgLayer bgColor
  where
    width = max typeWidth minWidth
    typeWidth = typeView ^. View.width
    newSize = typeView ^. View.size & _1 .~ width
    bgAnimId = animId ++ ["type background"]
    bgLayer = Config.layerTypes $ Config.layers config
    bgColor = Config.typeBoxBGColor config

makeTypeView :: MonadA m => Widget.R -> Sugar.EntityId -> Type -> ExprGuiM m (Widget f)
makeTypeView minWidth entityId typ =
  do
    config <- WE.readConfig
    TypeView.make animId typ
      <&> addTypeBackground config animId minWidth
      <&> Widget.fromView
    & ExprGuiM.widgetEnv
  where
    animId = Widget.toAnimId $ WidgetIds.fromEntityId entityId

addBelowInferredSpacing ::
  MonadA m => Widget (T m) ->
  ExpressionGui m -> ExprGuiM m (ExpressionGui m)
addBelowInferredSpacing widget eg =
  do
    config <- ExprGuiM.widgetEnv WE.readConfig
    let
      vspacer =
        Widget.fromView $
        Spacer.makeVertical $ realToFrac $
        Config.valInferredSpacing config
    addBelow 0.5 [(0, vspacer), (0.5, widget)] eg & return

addInferredType :: MonadA m => Sugar.EntityId -> Type -> ExpressionGui m -> ExprGuiM m (ExpressionGui m)
addInferredType entityId inferredType eg =
  do
    typeView <- makeTypeView (eg ^. egWidget . wWidth) entityId inferredType
    addBelowInferredSpacing typeView eg

parentExprFDConfig :: Config -> FocusDelegator.Config
parentExprFDConfig config = FocusDelegator.Config
  { FocusDelegator.focusChildKeys = Config.enterSubexpressionKeys config
  , FocusDelegator.focusChildDoc = E.Doc ["Navigation", "Enter subexpression"]
  , FocusDelegator.focusParentKeys = Config.leaveSubexpressionKeys config
  , FocusDelegator.focusParentDoc = E.Doc ["Navigation", "Leave subexpression"]
  }

disallowedNameChars :: String
disallowedNameChars = "[]\\`()"

nameEditFDConfig :: FocusDelegator.Config
nameEditFDConfig = FocusDelegator.Config
  { FocusDelegator.focusChildKeys = [ModKey mempty GLFW.Key'Enter]
  , FocusDelegator.focusChildDoc = E.Doc ["Edit", "Rename"]
  , FocusDelegator.focusParentKeys = [ModKey mempty GLFW.Key'Escape]
  , FocusDelegator.focusParentDoc = E.Doc ["Edit", "Done renaming"]
  }

makeNameOriginEdit :: MonadA m => Name m -> Widget.Id -> ExprGuiM m (Widget (T m))
makeNameOriginEdit name myId =
  do
    config <- Config.name <$> ExprGuiM.widgetEnv WE.readConfig
    makeNameEdit name myId -- myId goes directly to name edit
      & ExprGuiM.withFgColor (color config)
  where
    color =
      case nNameSource name of
      NameSourceAutoGenerated -> Config.autoNameOriginFGColor
      NameSourceStored -> Config.nameOriginFGColor

diveToNameEdit :: Widget.Id -> Widget.Id
diveToNameEdit = FocusDelegator.delegatingId

makeNameEdit ::
  MonadA m => Name m -> Widget.Id -> ExprGuiM m (Widget (T m))
makeNameEdit (Name nameSrc nameCollision setName name) =
  ExprGuiM.wrapDelegated nameEditFDConfig FocusDelegator.NotDelegating id $
  \myId -> do
    collisionSuffixes <-
      ExprGuiM.widgetEnv . makeCollisionSuffixLabels nameCollision $
      Widget.toAnimId myId
    nameEdit <-
      makeWordEdit (Property storedName setName) myId
      & WE.localEnv emptyStringEnv
      & ExprGuiM.widgetEnv
    return . Box.hboxCentered $ nameEdit : collisionSuffixes
  where
    emptyStringEnv env = env
      & WE.envTextStyle . TextEdit.sEmptyFocusedString .~ ""
      & WE.envTextStyle . TextEdit.sEmptyUnfocusedString .~ name
    storedName =
      case nameSrc of
      NameSourceAutoGenerated -> ""
      NameSourceStored -> name
    makeWordEdit =
      BWidgets.makeWordEdit <&>
      Lens.mapped . Lens.mapped . Widget.wEventMap %~
      E.filterChars (`notElem` disallowedNameChars)

stdWrapIn ::
  MonadA m =>
  Lens.Traversal' s (ExpressionGui m) ->
  Sugar.Payload m ExprGuiM.Payload ->
  ExprGuiM m s ->
  ExprGuiM m s
stdWrapIn trav pl mkGui =
  mkGui
  >>= trav %%~ maybeAddInferredTypePl pl
  & wrapExprEventMapIn trav pl

stdWrap ::
  MonadA m => Sugar.Payload m ExprGuiM.Payload ->
  ExprGuiM m (ExpressionGui m) ->
  ExprGuiM m (ExpressionGui m)
stdWrap = stdWrapIn id

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

makeFocusableView ::
  (MonadA m, MonadA n) => Widget.Id -> ExpressionGui n -> ExprGuiM m (ExpressionGui n)
makeFocusableView myId gui =
  ExprGuiM.widgetEnv $
  egWidget (BWidgets.makeFocusableView myId) gui

parenify ::
  (MonadA f, MonadA m) =>
  ParentPrecedence -> MyPrecedence ->
  (Widget.Id -> ExprGuiM m (ExpressionGui f)) ->
  Widget.Id -> ExprGuiM m (ExpressionGui f)
parenify (ParentPrecedence parent) (MyPrecedence prec) mkWidget myId
  | parent > prec =
    mkWidget myId
    >>= ExprGuiM.widgetEnv . Parens.addHighlightedTextParens myId
  | otherwise = mkWidget myId

makeLabel :: MonadA m => String -> AnimId -> ExprGuiM m (ExpressionGui m)
makeLabel text animId = ExprGuiM.makeLabel text animId <&> fromValueWidget

grammarLabel :: MonadA m => String -> AnimId -> ExprGuiM m (ExpressionGui m)
grammarLabel text animId =
  do
    config <- WE.readConfig & ExprGuiM.widgetEnv
    makeLabel text animId
      & ExprGuiM.localEnv
        (WE.setTextSizeColor (Config.baseTextSize config) (Config.grammarColor config))

addValBG :: MonadA m => Widget.Id -> Widget f -> ExprGuiM m (Widget f)
addValBG myId gui =
  do
    config <- ExprGuiM.widgetEnv WE.readConfig
    let layer = Config.layerValFrameBG $ Config.layers config
    let color = Config.valFrameBGColor config
    return $ Widget.backgroundColor layer animId color gui
  where
    animId = Widget.toAnimId myId ++ ["bg"]

addValFrame ::
  MonadA m => Widget.Id -> ExpressionGui m -> ExprGuiM m (ExpressionGui m)
addValFrame myId gui =
  do
    config <- ExprGuiM.widgetEnv WE.readConfig
    gui
      & pad (realToFrac <$> Config.valFramePadding config)
      & egWidget %%~ addValBG myId

stdWrapParenify ::
  MonadA m =>
  Sugar.Payload m ExprGuiM.Payload ->
  ParentPrecedence -> MyPrecedence ->
  (Widget.Id -> ExprGuiM m (ExpressionGui m)) ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
stdWrapParenify pl parentPrec prec = stdWrapParentExpr pl . parenify parentPrec prec

-- TODO: This doesn't belong here
makeNameView ::
  MonadA m =>
  Name m -> AnimId -> WE.WidgetEnvT (Transaction m) (Widget f)
makeNameView (Name _ collision _ name) animId = do
  label <- BWidgets.makeLabel name animId
  suffixLabels <- makeCollisionSuffixLabels collision $ animId ++ ["suffix"]
  return . Box.hboxCentered $ label : suffixLabels

-- TODO: This doesn't belong here
makeCollisionSuffixLabels ::
  MonadA m => NameCollision -> AnimId -> WE.WidgetEnvT m [Widget f]
makeCollisionSuffixLabels NoCollision _ = return []
makeCollisionSuffixLabels (Collision suffix) animId = do
  config <- WE.readConfig
  let
    Config.Name{..} = Config.name config
    onSuffixWidget =
      Widget.backgroundColor (Config.layerNameCollisionBG (Config.layers config))
        (animId ++ ["bg"]) collisionSuffixBGColor .
      Widget.scale (realToFrac <$> collisionSuffixScaleFactor)
  BWidgets.makeLabel (show suffix) animId
    & WE.localEnv (WE.setTextColor collisionSuffixTextColor)
    <&> (:[]) . onSuffixWidget

wrapExprEventMap ::
  MonadA m =>
  Sugar.Payload m ExprGuiM.Payload ->
  ExprGuiM m (ExpressionGui m) ->
  ExprGuiM m (ExpressionGui m)
wrapExprEventMap = wrapExprEventMapIn id

wrapExprEventMapIn ::
  MonadA m =>
  Lens.Traversal' s (ExpressionGui m) ->
  Sugar.Payload m ExprGuiM.Payload ->
  ExprGuiM m s ->
  ExprGuiM m s
wrapExprEventMapIn trav pl action = do
  (res, resultPickers) <- ExprGuiM.listenResultPickers action
  res & trav (addExprEventMap pl resultPickers)

addExprEventMap ::
  MonadA m =>
  Sugar.Payload m ExprGuiM.Payload -> HolePickers m ->
  ExpressionGui m -> ExprGuiM m (ExpressionGui m)
addExprEventMap pl resultPickers gui = do
  exprEventMap <- ExprEventMap.make resultPickers pl & ExprGuiM.widgetEnv
  gui & egWidget %~ Widget.weakerEvents exprEventMap & return

maybeAddInferredTypePl ::
  MonadA m =>
  Sugar.Payload m0 ExprGuiM.Payload ->
  ExpressionGui m -> ExprGuiM m (ExpressionGui m)
maybeAddInferredTypePl pl =
  maybeAddInferredType
  (pl ^. Sugar.plData . ExprGuiM.plShowType)
  (pl ^. Sugar.plInferredType)
  (pl ^. Sugar.plEntityId)

maybeAddInferredType ::
  MonadA m =>
  ExprGuiM.ShowType -> Type -> Sugar.EntityId ->
  ExpressionGui m -> ExprGuiM m (ExpressionGui m)
maybeAddInferredType showType inferredType entityId eg =
  do
    shouldShow <- ExprGuiM.shouldShowType showType
    eg
      & if shouldShow
        then addInferredType entityId inferredType
        else return

listWithDelDests :: k -> k -> (a -> k) -> [a] -> [(k, k, a)]
listWithDelDests before after dest list =
  ListUtils.withPrevNext
  (maybe before dest (list ^? Lens.ix 1))
  (maybe after dest (reverse list ^? Lens.ix 1))
  dest list
