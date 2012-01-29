{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE FlexibleInstances, Rank2Types,
             OverloadedStrings, UndecidableInstances,
             GeneralizedNewtypeDeriving, TemplateHaskell #-}

module Main(main) where

import Control.Monad (when, liftM, unless)
import Control.Monad.Trans.Class (lift)
import Data.ByteString.Char8 (pack)
import Data.List (findIndex, elemIndex, intersperse, delete)
import Data.List.Utils (enumerate, nth, removeAt)
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid(Monoid(..))
import Data.Store.IRef (IRef)
import Data.Store.Property (Property(Property))
import Data.Store.Rev.View (View)
import Data.Store.Transaction (Transaction)
import Data.Vector.Vector2 (Vector2(..))
import Editor.Anchors (DBTag, ViewTag)
import Graphics.UI.Bottle.MainLoop(mainLoopWidget)
import Graphics.UI.Bottle.Sized (Sized)
import Graphics.UI.Bottle.Widget (Widget)
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.Store.Db as Db
import qualified Data.Store.Property as Property
import qualified Data.Store.Rev.Branch as Branch
import qualified Data.Store.Rev.Version as Version
import qualified Data.Store.Rev.View as View
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Anchors as Anchors
import qualified Editor.AnimIds as AnimIds
import qualified Editor.Config as Config
import qualified Editor.Data as Data
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.EventMapDoc as EventMapDoc
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import qualified System.Info

data CTransactionEnv = CTransactionEnv {
  envCursor :: Widget.Cursor,
  envTextStyle :: TextEdit.Style
  }
AtFieldTH.make ''CTransactionEnv

newtype CTransaction t m a = CTransaction {
  unCTransaction :: Reader.ReaderT CTransactionEnv (Transaction t m) a
  }
  deriving (Monad)
AtFieldTH.make ''CTransaction

runCTransaction :: Widget.Cursor -> TextEdit.Style -> CTransaction t m a -> Transaction t m a
runCTransaction cursor style =
  (`Reader.runReaderT` CTransactionEnv cursor style) . unCTransaction

readCursor :: Monad m => CTransaction t m Widget.Cursor
readCursor = CTransaction (Reader.asks envCursor)

readTextStyle :: Monad m => CTransaction t m TextEdit.Style
readTextStyle = CTransaction (Reader.asks envTextStyle)

transaction :: Monad m => Transaction t m a -> CTransaction t m a
transaction = CTransaction . lift

getP :: Monad m => Property (Transaction t m) a -> CTransaction t m a
getP = transaction . Property.get

type TWidget t m = CTransaction t m (Widget (Transaction t m))

class (Monad m, Functor m) => MonadF m
instance (Monad m, Functor m) => MonadF m

makeTextView :: MonadF m => String -> Anim.AnimId -> TWidget t m
makeTextView text animId = do
  style <- readTextStyle
  return $
    TextView.makeWidget (TextEdit.sTextViewStyle style) text animId

_makeFocusableTextView :: MonadF m => String -> Anim.AnimId -> TWidget t m
_makeFocusableTextView text animId = do
  hasFocus <- liftM (animId ==) readCursor
  textView <- makeTextView text animId
  let
    setBackground
      | hasFocus = Widget.backgroundColor AnimIds.backgroundCursorId blue
      | otherwise = id
  return .
    (Widget.atIsFocused . const) hasFocus . setBackground $
    Widget.takesFocus (const (return animId)) textView
  where
    blue = Draw.Color 0 0 1 0.8

makeChoice ::
  (Monad m) =>
  Anim.AnimId -> Transaction.Property t m Int -> Box.Orientation ->
  [TWidget t m] -> TWidget t m
makeChoice selectionAnimId curChoiceRef orientation children = do
  curChoice <- getP curChoiceRef
  focusables <- sequence children
  let
    widget =
      Box.makeBiased orientation curChoice .
      nth curChoice (Widget.backgroundColor selectionAnimId selectedColor) .
      map updateCurChoice $
      enumerate focusables
  return widget
  where
    updateCurChoice (i, focusable) =
      Widget.atEvents (Property.set curChoiceRef i >>) focusable
    selectedColor = Draw.Color 0 0.5 0 1

assignCursor :: Widget.Cursor -> Widget.Cursor -> TWidget t m -> TWidget t m
assignCursor src dest =
  (atCTransaction . Reader.withReaderT . atEnvCursor) replace
  where
    replace cursor
      | cursor == src = dest
      | otherwise = cursor

-- TODO: This logic belongs in the FocusDelegator itself
wrapDelegatedWithKeys ::
  Monad m => FocusDelegator.Keys ->
  FocusDelegator.IsDelegating ->
  (Anim.AnimId -> TWidget t m) -> Anim.AnimId -> TWidget t m
wrapDelegatedWithKeys keys entryState f animId = do
  let
    innerAnimId = AnimIds.delegating animId
    selfAnimId = AnimIds.notDelegating animId
    destAnimId =
      case entryState of
        FocusDelegator.NotDelegating -> selfAnimId
        FocusDelegator.Delegating -> innerAnimId
  assignCursor animId destAnimId $ do
    innerWidget <- f innerAnimId
    let
      cursorSelf = makeDelegator $ Just FocusDelegator.NotDelegating
      cursorNotSelf
        | Widget.isFocused innerWidget =
          makeDelegator $ Just FocusDelegator.Delegating
        | otherwise = makeDelegator Nothing

      makeDelegator delegateState =
        (Widget.atIsFocused . const) (isJust delegateState) $
        FocusDelegator.make entryState delegateState
        selfAnimId keys
        AnimIds.backgroundCursorId innerWidget
    liftM (maybe cursorNotSelf (const cursorSelf) .
             Anim.subId selfAnimId) $
      readCursor

wrapDelegated ::
  Monad m => FocusDelegator.IsDelegating ->
  (Anim.AnimId -> TWidget t m) -> Anim.AnimId -> TWidget t m
wrapDelegated = wrapDelegatedWithKeys FocusDelegator.defaultKeys

makeTextEdit ::
  Monad m =>
  Transaction.Property t m String ->
  Anim.AnimId -> TWidget t m
makeTextEdit textRef animId = do
  text <- getP textRef
  let
    lifter (newText, eventRes) = do
      when (newText /= text) $ Property.set textRef newText
      return eventRes
  cursor <- readCursor
  style <- readTextStyle
  return .
    Widget.atEvents lifter $
    TextEdit.make style cursor text animId

------

addParameter ::
  Monad m => Transaction.Property ViewTag m Data.Definition ->
  Transaction ViewTag m (IRef Data.Parameter)
addParameter definitionRef = do
  newParamI <- Transaction.newIRef Data.Parameter
  Property.pureModify definitionRef . Data.atDefParameters $
    (++ [newParamI])
  return newParamI

delParameter ::
  Monad m => Transaction.Property ViewTag m Data.Definition ->
  IRef Data.Parameter -> Transaction ViewTag m ()
delParameter definitionRef paramI =
  Property.pureModify definitionRef . Data.atDefParameters $
    delete paramI

atEmptyStr :: (String -> String) -> TWidget t m -> TWidget t m
atEmptyStr = atCTransaction . Reader.withReaderT . atEnvTextStyle . TextEdit.atSEmptyString

makeNameEdit :: Monad m => String -> IRef a -> Anim.AnimId -> TWidget t m
makeNameEdit emptyStr iref =
  (atEmptyStr . const) emptyStr . makeTextEdit (Anchors.aNameRef iref)

spaceView :: Sized Anim.Frame
spaceView = Spacer.makeHorizontal 20

spaceWidget :: Widget f
spaceWidget = Widget.liftView spaceView

center :: Vector2 Draw.R
center = Vector2 0.5 0.5

hbox :: [Widget f] -> Widget f
hbox = Box.make Box.horizontal . map (Widget.align center)

-- vbox :: [Widget f] -> Widget f
-- vbox = Box.make Box.vertical . map (Widget.align center)

giveAsArg ::
  Monad m =>
  Property (Transaction ViewTag m) (IRef Data.Expression) ->
  Transaction ViewTag m (IRef Data.Expression)
giveAsArg expressionPtr = do
  expressionI <- Property.get expressionPtr
  newFuncI <- Transaction.newIRef . Data.ExpressionGetVariable . Data.GetVariable =<< Transaction.newIRef ""
  Property.set expressionPtr =<< (Transaction.newIRef . Data.ExpressionApply) (Data.Apply newFuncI expressionI)
  return newFuncI

callWithArg ::
  Monad m =>
  Property (Transaction ViewTag m) (IRef Data.Expression) ->
  Transaction ViewTag m (IRef Data.Expression)
callWithArg expressionPtr = do
  expressionI <- Property.get expressionPtr
  argI <- Transaction.newIRef . Data.ExpressionGetVariable . Data.GetVariable =<< Transaction.newIRef ""
  Property.set expressionPtr =<< (Transaction.newIRef . Data.ExpressionApply) (Data.Apply expressionI argI)
  return argI

makeExpressionEdit :: MonadF m => Bool -> Property (Transaction ViewTag m) (IRef Data.Expression) -> TWidget ViewTag m
makeExpressionEdit isArgument expressionPtr = do
  expressionI <- getP expressionPtr
  let
    expressionRef = Transaction.fromIRef expressionI
    exprKeys = Config.exprFocusDelegatorKeys
    eventMap = mconcat
      [ Widget.actionEventMapMovesCursor
        Config.giveAsArgumentKey "Give as argument"
        mkGiveAsArg
      , Widget.actionEventMapMovesCursor
        Config.callWithArgumentKey "Call with argument"
        mkCallWithArg
      ]
    mkGiveAsArg = fmap (AnimIds.delegating . AnimIds.fromIRef) $ giveAsArg expressionPtr
    mkCallWithArg = fmap (AnimIds.delegating . AnimIds.fromIRef) $ callWithArg expressionPtr
    wrap keys entryState f =
      (liftM . Widget.weakerEvents) eventMap .
      wrapDelegatedWithKeys keys entryState f $
      AnimIds.fromIRef expressionI
    makeDelEvent =
      liftM . Widget.weakerEvents .
      Widget.actionEventMapMovesCursor Config.delKeys "Delete" . setExpr
    setExpr newExprI = do
      Property.set expressionPtr newExprI
      return $ AnimIds.fromIRef newExprI
  expr <- getP expressionRef
  case expr of
    Data.ExpressionGetVariable (Data.GetVariable nameI) ->
      wrap FocusDelegator.defaultKeys FocusDelegator.NotDelegating .
        makeTextEdit $ Transaction.fromIRef nameI
    Data.ExpressionApply (Data.Apply funcI argI) ->
      (liftM . Widget.weakerEvents)
        (Widget.actionEventMapMovesCursor Config.addNextArgumentKey "Add another argument" mkCallWithArg) .
      wrap exprKeys FocusDelegator.Delegating $ \animId -> assignCursor animId (AnimIds.fromIRef argI) $ do
        let label str = makeTextView str $ Anim.joinId animId [pack str]
        before <- label "("
        after <- label ")"
        funcEdit <-
          makeDelEvent argI . makeExpressionEdit False $
          Property (return funcI) (Property.set expressionRef . Data.ExpressionApply . (`Data.Apply` argI))
        argEdit <-
          makeDelEvent funcI . makeExpressionEdit True $
          Property (return argI) (Property.set expressionRef . Data.ExpressionApply . Data.Apply funcI)
        return . hbox $ concat
          [[before | isArgument], [funcEdit], [spaceWidget], [argEdit], [after | isArgument]]

hboxSpaced :: [Widget f] -> Widget f
hboxSpaced = hbox . intersperse spaceWidget

makeDefinitionEdit :: MonadF m => IRef Data.Definition -> TWidget ViewTag m
makeDefinitionEdit definitionI = do
  Data.Definition params _ <- getP definitionRef
  nameEdit <-
    assignCursor animId nameEditAnimId $
    makeNameEdit "<unnamed>" definitionI nameEditAnimId
  equals <- makeTextView "=" $ Anim.joinId animId ["equals"]
  expressionEdit <- makeExpressionEdit False bodyRef
  paramsEdits <- mapM makeParamEdit $ enumerate params
  return .
    Widget.strongerEvents eventMap . hboxSpaced $
    [nameEdit] ++ paramsEdits ++ [equals, expressionEdit]
  where
    makeParamEdit (i, paramI) =
      (liftM . Widget.strongerEvents) (paramEventMap paramI) .
      wrapDelegated FocusDelegator.NotDelegating (makeNameEdit ("<unnamed param " ++ show i ++ ">") paramI) $
      AnimIds.fromIRef paramI
    bodyRef = Property.composeLabel Data.defBody Data.atDefBody definitionRef
    definitionRef = Transaction.fromIRef definitionI
    paramEventMap paramI =
      Widget.actionEventMapMovesCursor Config.delParamKeys "Delete Parameter" .
      (liftM . const) animId $
      delParameter definitionRef paramI
    eventMap =
      Widget.actionEventMapMovesCursor Config.addParamKeys "Add Parameter" .
      liftM (AnimIds.delegating . AnimIds.fromIRef) $
      addParameter definitionRef
    nameEditAnimId = Anim.joinId animId ["name"]
    animId = AnimIds.fromIRef definitionI

-- Apply the transactions to the given View and convert them to
-- transactions on a DB
makeWidgetForView :: MonadF m => View -> TWidget DBTag m
makeWidgetForView view = do
  versionData <- transaction $ Version.versionData =<< View.curVersion view
  cursor <- readCursor
  style <- readTextStyle
  focusable <-
    widgetDownTransaction .
    runCTransaction cursor style .
    (liftM . Widget.atEvents) (>>= applyAndReturn saveCursor) $
    makeDefinitionEdit Anchors.rootIRef
  let undoEventMap = maybe mempty makeUndoEventMap (Version.parent versionData)
  return $ Widget.strongerEvents undoEventMap focusable
  where
    makeUndoEventMap = Widget.actionEventMapMovesCursor Config.undoKeys "Undo" . (>> fetchRevisionCursor) . View.move view
    fetchRevisionCursor = Transaction.run store $ Property.get Anchors.cursor
    store = Anchors.viewStore view
    widgetDownTransaction =
      transaction . Transaction.run store .
      (liftM . Widget.atEvents) (Transaction.run store)
    saveCursor eventResult = do
      isEmpty <- Transaction.isEmpty
      unless isEmpty $ maybeUpdateCursor eventResult

maybeUpdateCursor :: Monad m => Widget.EventResult -> Transaction t m ()
maybeUpdateCursor = maybe (return ()) (Property.set Anchors.cursor) . Widget.eCursor

defaultFont :: String -> FilePath
defaultFont "darwin" = "/Library/Fonts/Arial.ttf"
defaultFont _ = "/usr/share/fonts/truetype/freefont/FreeSerifBold.ttf"

main :: IO ()
main = do
  font <- Draw.openFont (defaultFont System.Info.os)
  Db.withDb "/tmp/codeedit.db" $ runDbStore font . Anchors.dbStore

deleteCurrentBranch :: Monad m => Transaction DBTag m ()
deleteCurrentBranch = do
  branch <- Property.get Anchors.currentBranch
  branches <- Property.get Anchors.branches
  let
    index =
      fromMaybe (error "Invalid current branch!") $
      findIndex ((branch ==) . snd) branches
    newBranches = removeAt index branches
  Property.set Anchors.branches newBranches
  Property.set Anchors.currentBranch . snd $
    newBranches !! min (length newBranches - 1) index

makeBranch :: Monad m => View -> Transaction DBTag m ()
makeBranch view = do
  newBranch <- Branch.new =<< View.curVersion view
  textEditModelIRef <- Transaction.newIRef "New view"
  let viewPair = (textEditModelIRef, newBranch)
  Property.pureModify Anchors.branches (++ [viewPair])
  Property.set Anchors.currentBranch newBranch

applyAndReturn :: Monad m => (a -> m ()) -> a -> m a
applyAndReturn f val = f val >> return val

branchSelectorProperty :: Monad m => View -> [Branch.Branch] -> Property (Transaction DBTag m) Int
branchSelectorProperty view branches =
  Property.pureCompose
    (fromMaybe (error "Selected branch not in branch list") .
     (`elemIndex` branches)) (branches !!) .
  Property.compose return (applyAndReturn (View.setBranch view)) $
  Anchors.currentBranch

makeRootWidget :: MonadF m => TWidget DBTag m
makeRootWidget = do
  view <- getP Anchors.view
  namedBranches <- getP Anchors.branches

  viewEdit <- makeWidgetForView view

  let
    branchIndexRef = branchSelectorProperty view $ map snd namedBranches
    makeBranchNameEdit textEditModelIRef =
      wrapDelegated FocusDelegator.NotDelegating
      (makeTextEdit (Transaction.fromIRef textEditModelIRef)) $
      AnimIds.fromIRef textEditModelIRef
  branchSelector <-
    makeChoice AnimIds.branchSelection branchIndexRef
    Box.vertical $ map (makeBranchNameEdit . fst) namedBranches

  let
    delBranchEventMap
      | null (drop 1 namedBranches) = mempty
      | otherwise = Widget.actionEventMap Config.delBranchKeys "Delete Branch" deleteCurrentBranch
  return .
    (Widget.strongerEvents . mconcat)
      [Widget.actionEventMap Config.quitKeys "Quit" (error "Quit")
      ,Widget.actionEventMap Config.makeBranchKeys "New Branch" (makeBranch view)
      ] .
    Box.make Box.horizontal $
    [viewEdit
    ,Widget.liftView Spacer.makeHorizontalExpanding
    ,Widget.strongerEvents delBranchEventMap branchSelector
    ]

runDbStore :: Draw.Font -> Transaction.Store DBTag IO -> IO a
runDbStore font store = do
  Anchors.initDB store
  addHelp <- EventMapDoc.makeToggledHelpAdder Config.overlayDocKeys helpStyle
  mainLoopWidget $ addHelp =<< makeWidget
  where
    helpStyle = TextView.Style {
      TextView.styleFont = font,
      TextView.styleFontSize = 10
      }
    style = TextEdit.Style {
      TextEdit.sTextViewStyle =
        TextView.Style {
          TextView.styleFont = font,
          TextView.styleFontSize = 25
          },
      TextEdit.sCursorColor = TextEdit.defaultCursorColor,
      TextEdit.sCursorWidth = TextEdit.defaultCursorWidth,
      TextEdit.sTextCursorId = AnimIds.textCursorId,
      TextEdit.sBackgroundCursorId = AnimIds.backgroundCursorId,
      TextEdit.sEmptyString = "<empty>"
      }

    fromCursor cursor = runCTransaction cursor style makeRootWidget
    makeWidget = widgetDownTransaction $ do
      cursor <- Property.get Anchors.cursor
      candidateWidget <- fromCursor cursor
      focusable <-
        if Widget.isFocused candidateWidget
        then return candidateWidget
        else fromCursor (AnimIds.fromIRef Anchors.rootIRef)
      unless (Widget.isFocused focusable) $
        fail "Root cursor did not match"
      return $ Widget.atEvents (>>= attachCursor) focusable

    widgetDownTransaction =
      Transaction.run store .
      (liftM . Widget.atEvents) (Transaction.run store)

    attachCursor eventResult = do
      maybeUpdateCursor eventResult
      return eventResult
