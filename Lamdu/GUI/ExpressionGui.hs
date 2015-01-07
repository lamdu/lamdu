{-# LANGUAGE RecordWildCards, OverloadedStrings, RankNTypes #-}
module Lamdu.GUI.ExpressionGui
  ( ExpressionGui(..), egWidget, egAlignment
  -- General:
  , fromValueWidget
  , scaleFromTop
  , pad
  , hbox, hboxSpaced, addBelow
  , gridDownwards
  , vboxDownwards, vboxDownwardsSpaced
  , makeRow
  , listWithDelDests
  , makeLabel
  , grammarLabel
  , addValBG, addValFrame
  -- Lifted widgets:
  , makeFocusableView
  , makeNameView
  , makeNameEdit
  , makeNameOriginEdit
  -- Info adding
  , addBelowInferredSpacing
  , maybeAddInferredType
  , makeTypeView
  -- Expression wrapping
  , MyPrecedence(..), ParentPrecedence(..), Precedence
  , parenify
  , wrapExprEventMapIn
  , wrapExprEventMap
  , stdWrap
  , stdWrapIn
  , stdWrapParentExpr
  , stdWrapParenify
  ) where

import           Control.Applicative ((<$>))
import           Control.Lens (Lens')
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.MonadA (MonadA)
import           Data.Functor.Identity (Identity(..))
import qualified Data.List as List
import qualified Data.List.Utils as ListUtils
import           Data.Monoid (Monoid(..))
import           Data.Store.Property (Property(..))
import           Data.Store.Transaction (Transaction)
import           Data.Traversable (Traversable(..))
import           Data.Vector.Vector2 (Vector2(..))
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.ModKey (ModKey(..))
import           Graphics.UI.Bottle.View (View)
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import           Graphics.UI.Bottle.Widgets.Box (KBox)
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid
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
import           Lamdu.GUI.ExpressionGui.Types (WidgetT, ExpressionGui(..), egWidget, egAlignment)
import           Lamdu.GUI.Precedence (MyPrecedence(..), ParentPrecedence(..), Precedence)
import qualified Lamdu.GUI.TypeView as TypeView
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.AddNames.Types (Name(..), NameSource(..), NameCollision(..))
import qualified Lamdu.Sugar.Types as Sugar

fromValueWidget :: WidgetT m -> ExpressionGui m
fromValueWidget widget = ExpressionGui widget 0.5

-- | Scale the given ExpressionGui without moving its top alignment
-- point:
scaleFromTop :: Vector2 Widget.R -> ExpressionGui m -> ExpressionGui m
scaleFromTop ratio (ExpressionGui widget alignment) =
  ExpressionGui (Widget.scale ratio widget) (alignment / (ratio ^. _2))

pad :: Vector2 Widget.R -> ExpressionGui m -> ExpressionGui m
pad padding (ExpressionGui widget alignment) =
  ExpressionGui newWidget $
  (padding ^. _2 + alignment * widget ^. height) / newWidget ^. height
  where
    height = Widget.wSize . _2
    newWidget = Widget.pad padding widget

hbox :: [ExpressionGui m] -> ExpressionGui m
hbox guis =
  ExpressionGui (Box.toWidget box) $
  case box ^. Box.boxContent of
  ((_, x) : _) -> x ^. Grid.elementAlign . _2
  _ -> error "hbox must not get empty list :("
  where
    box = Box.make Box.horizontal $ map f guis
    f (ExpressionGui widget alignment) = (Vector2 0.5 alignment, widget)

hboxSpaced :: MonadA m => [ExpressionGui f] -> ExprGuiM m (ExpressionGui f)
hboxSpaced guis =
  ExprGuiM.widgetEnv BWidgets.stdSpaceWidget
  <&> hbox . (`List.intersperse` guis) . fromValueWidget

vboxDownwards :: [(Widget.R, ExpressionGui m)] -> ExpressionGui m
vboxDownwards [] = error "Empty vboxDownwards"
vboxDownwards ((hAlign, x) : xs) =
  addBelow hAlign (xs <&> toAlignedWidget) x
  where
    toAlignedWidget (align, gui) = (Vector2 align 0, gui ^. egWidget)

vboxDownwardsSpaced ::
  MonadA m => [(Widget.R, ExpressionGui m)] -> ExprGuiM m (ExpressionGui m)
vboxDownwardsSpaced guis =
  do
    space <- ExprGuiM.widgetEnv BWidgets.verticalSpace
    guis & List.intersperse (0, fromValueWidget space)
      & vboxDownwards & return

gridDownwards :: [[(Widget.R, ExpressionGui m)]] -> ExpressionGui m
gridDownwards [] = fromValueWidget $ uncurry Widget.liftView $ Spacer.make 0
gridDownwards ([]:rs) = gridDownwards rs
gridDownwards rows =
  ExpressionGui
  { _egWidget = Grid.toWidget grid
  , _egAlignment = grid ^. Grid.gridContent & head & head & (^. _2 . Grid.elementAlign . _2)
  }
  where
    grid =
      rows
      & Lens.traversed . Lens.traversed %~ cell
      & Grid.make
    cell (hAlign, ExpressionGui widget vAlign) = (Vector2 hAlign vAlign, widget)

fromBox :: KBox Bool (Transaction m) -> ExpressionGui m
fromBox box =
  ExpressionGui (Box.toWidget box) alignment
  where
    alignment =
      maybe (error "True disappeared from box list?!")
        (^. Grid.elementAlign . _2) .
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
wWidth = Widget.wSize . _1

addTypeBackground :: Config -> AnimId -> Widget.R -> View -> View
addTypeBackground config animId minWidth typeView =
  typeView
  & _1 .~ newSize
  & _2 %~ Anim.translate (Vector2 ((width - typeWidth) / 2) 0)
  & _2 %~ Anim.backgroundColor bgAnimId bgLayer bgColor newSize
  where
    typeWidth = typeView ^. _1 . _1
    width = max typeWidth minWidth
    newSize = typeView ^. _1 & _1 .~ width
    bgAnimId = animId ++ ["type background"]
    bgLayer = Config.layerTypes $ Config.layers config
    bgColor = Config.typeBoxBGColor config

makeTypeView :: MonadA m => Widget.R -> Sugar.EntityId -> Type -> ExprGuiM m (Widget f)
makeTypeView minWidth entityId typ =
  do
    config <- WE.readConfig
    TypeView.make animId typ
      <&> addTypeBackground config animId minWidth
      <&> uncurry Widget.liftView
    & ExprGuiM.widgetEnv
  where
    animId = Widget.toAnimId $ WidgetIds.fromEntityId entityId

addBelowInferredSpacing ::
  MonadA m => WidgetT m ->
  ExpressionGui m -> ExprGuiM m (ExpressionGui m)
addBelowInferredSpacing typeView eg =
  do
    config <- ExprGuiM.widgetEnv WE.readConfig
    let
      vspacer =
        uncurry Widget.liftView $ Spacer.makeVertical $
        realToFrac $ Config.valInferredSpacing config
      items =
        [ (0.5, vspacer)
        , (0.5, typeView)
        ]
    return $ addBelow 0.5 items eg

addInferredType :: MonadA m => Sugar.EntityId -> Type -> ExpressionGui m -> ExprGuiM m (ExpressionGui m)
addInferredType entityId inferredType eg =
  do
    typeView <- makeTypeView (eg ^. egWidget . wWidth) entityId inferredType
    addBelowInferredSpacing typeView eg

parentExprFDConfig :: Config -> FocusDelegator.Config
parentExprFDConfig config = FocusDelegator.Config
  { FocusDelegator.startDelegatingKeys = Config.enterSubexpressionKeys config
  , FocusDelegator.startDelegatingDoc = E.Doc ["Navigation", "Enter subexpression"]
  , FocusDelegator.stopDelegatingKeys = Config.leaveSubexpressionKeys config
  , FocusDelegator.stopDelegatingDoc = E.Doc ["Navigation", "Leave subexpression"]
  }

-- ExprGuiM GUIs (TODO: Move to Monad.hs?)

disallowedNameChars :: [(Char, E.IsShifted)]
disallowedNameChars =
  E.anyShiftedChars "[]\\`()" ++
  [ ('0', E.Shifted)
  , ('9', E.Shifted)
  ]

-- TODO: Move to BWidgets
makeBridge ::
  MonadA m =>
  (Widget.Id -> WE.WidgetEnvT m (Widget f)) ->
  (Widget.Id -> WE.WidgetEnvT m (Widget f)) ->
  Widget.Id -> WE.WidgetEnvT m (Widget f)
makeBridge mkFocused mkUnfocused myId = do
  isFocused <- WE.isSubCursor myId
  (if isFocused then mkFocused else mkUnfocused) myId

nameEditFDConfig :: FocusDelegator.Config
nameEditFDConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKeys = [ModKey mempty GLFW.Key'Enter]
  , FocusDelegator.startDelegatingDoc = E.Doc ["Edit", "Rename"]
  , FocusDelegator.stopDelegatingKeys = [ModKey mempty GLFW.Key'Escape]
  , FocusDelegator.stopDelegatingDoc = E.Doc ["Edit", "Done renaming"]
  }

makeNameOriginEdit :: MonadA m => Name m -> Widget.Id -> ExprGuiM m (WidgetT m)
makeNameOriginEdit name myId =
  do
    config <- Config.name <$> ExprGuiM.widgetEnv WE.readConfig
    ExprGuiM.withFgColor (color config) $
      ExprGuiM.wrapDelegated nameEditFDConfig FocusDelegator.NotDelegating id
      (makeNameEdit name) myId
  where
    color =
      case nNameSource name of
      NameSourceAutoGenerated -> Config.autoNameOriginFGColor
      NameSourceStored -> Config.nameOriginFGColor

makeNameEdit ::
  MonadA m => Name m -> Widget.Id -> ExprGuiM m (WidgetT m)
makeNameEdit (Name nameSrc nameCollision setName name) myId = do
  collisionSuffixes <-
    ExprGuiM.widgetEnv . makeCollisionSuffixLabels nameCollision $
    Widget.toAnimId myId
  nameEdit <-
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
      E.filterSChars (curry (`notElem` disallowedNameChars))

stdWrapIn ::
  (Traversable t, MonadA m) =>
  Sugar.Payload m ExprGuiM.Payload ->
  ExprGuiM m (t (ExpressionGui m)) ->
  ExprGuiM m (t (ExpressionGui m))
stdWrapIn pl mkGui =
  mkGui
  >>= traverse %%~
      maybeAddInferredType
      (pl ^. Sugar.plData . ExprGuiM.plShowType)
      (pl ^. Sugar.plInferredType)
      (pl ^. Sugar.plEntityId)
  & wrapExprEventMapIn pl

stdWrap ::
  MonadA m => Sugar.Payload m ExprGuiM.Payload ->
  ExprGuiM m (ExpressionGui m) ->
  ExprGuiM m (ExpressionGui m)
stdWrap pl mkGui =
  mkGui <&> Identity
  & stdWrapIn pl
  <&> runIdentity

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
  MonadA m =>
  ParentPrecedence -> MyPrecedence ->
  (Widget.Id -> ExpressionGui f -> ExprGuiM m (ExpressionGui f)) ->
  (Widget.Id -> ExprGuiM m (ExpressionGui f)) ->
  Widget.Id -> ExprGuiM m (ExpressionGui f)
parenify (ParentPrecedence parent) (MyPrecedence prec) addParens mkWidget myId
  | parent > prec = addParens myId =<< mkWidget myId
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
  (Widget.Id -> ExpressionGui m -> ExprGuiM m (ExpressionGui m)) ->
  (Widget.Id -> ExprGuiM m (ExpressionGui m)) ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
stdWrapParenify pl parentPrec prec addParens =
  stdWrapParentExpr pl . parenify parentPrec prec addParens

-- TODO: This doesn't belong here
makeRow :: [(Widget.R, ExpressionGui m)] -> [(Vector2 Widget.R, WidgetT m)]
makeRow =
  map item
  where
    item (halign, ExpressionGui widget alignment) =
      (Vector2 halign alignment, widget)

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
wrapExprEventMap pl action =
  action <&> Identity
  & wrapExprEventMapIn pl
  <&> runIdentity

wrapExprEventMapIn ::
  (MonadA m, Traversable t) =>
  Sugar.Payload m ExprGuiM.Payload ->
  ExprGuiM m (t (ExpressionGui m)) ->
  ExprGuiM m (t (ExpressionGui m))
wrapExprEventMapIn pl action = do
  (res, resultPickers) <- ExprGuiM.listenResultPickers action
  res & traverse (addExprEventMap pl resultPickers)

addExprEventMap ::
  MonadA m =>
  Sugar.Payload m ExprGuiM.Payload -> HolePickers m ->
  ExpressionGui m -> ExprGuiM m (ExpressionGui m)
addExprEventMap pl resultPickers gui = do
  exprEventMap <-
    ExprEventMap.make (gui ^. egWidget . Widget.wIsFocused)
    resultPickers pl
  gui & egWidget %~ Widget.weakerEvents exprEventMap & return

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
