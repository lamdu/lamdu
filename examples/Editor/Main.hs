{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main(main) where

import Control.Arrow (first)
import Control.Monad (liftM, unless)
import Data.ByteString.Char8 (pack)
import Data.List (findIndex, elemIndex, intersperse)
import Data.List.Utils (enumerate, removeAt)
import Data.Maybe (fromMaybe)
import Data.Monoid(Monoid(..))
import Data.Store.IRef (IRef)
import Data.Store.Property (Property(Property))
import Data.Store.Rev.View (View)
import Data.Store.Transaction (Transaction)
import Data.Vector.Vector2 (Vector2(..))
import Editor.Anchors (DBTag, ViewTag)
import Editor.CTransaction (CTransaction, runCTransaction, runNestedCTransaction, transaction, getP, assignCursor, atEmptyStr, TWidget)
import Editor.MonadF (MonadF)
import Graphics.UI.Bottle.MainLoop(mainLoopWidget)
import Graphics.UI.Bottle.Sized (Sized)
import Graphics.UI.Bottle.Widget (Widget)
import qualified Data.Store.Db as Db
import qualified Data.Store.Property as Property
import qualified Data.Store.Rev.Branch as Branch
import qualified Data.Store.Rev.Version as Version
import qualified Data.Store.Rev.View as View
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Anchors as Anchors
import qualified Editor.AnimIds as AnimIds
import qualified Editor.BottleWidgets as BWidgets
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

makeNameEdit :: Monad m => String -> IRef a -> Anim.AnimId -> TWidget t m
makeNameEdit emptyStr iref =
  (atEmptyStr . const) emptyStr . BWidgets.makeWordEdit (Anchors.aNameRef iref)

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

makeExpressionEdit :: MonadF m =>
  Bool -> Property (Transaction ViewTag m) (IRef Data.Expression) ->
  CTransaction ViewTag m (Widget (Transaction ViewTag m), Anim.AnimId)
makeExpressionEdit isArgument expressionPtr = do
  expressionI <- getP expressionPtr
  let
    expressionRef = Transaction.fromIRef expressionI
    exprKeys = Config.exprFocusDelegatorKeys
    mkCallWithArg = fmap (AnimIds.delegating . AnimIds.fromIRef) . callWithArg
    mkGiveAsArg = fmap (AnimIds.delegating . AnimIds.fromIRef) $ giveAsArg expressionPtr
    eventMap = mconcat
      [ Widget.actionEventMapMovesCursor
        Config.giveAsArgumentKeys "Give as argument"
        mkGiveAsArg
      , Widget.actionEventMapMovesCursor
        Config.callWithArgumentKeys "Call with argument" $
        mkCallWithArg expressionPtr
      ]
    weakerEvents = liftM . first . Widget.weakerEvents
    wrap keys entryState f =
      (if isArgument then id else mkCallWithArgEvent) .
      weakerEvents eventMap .
      BWidgets.wrapDelegatedWithKeys keys entryState f first $
      AnimIds.fromIRef expressionI
    mkDelEvent = weakerEvents . Widget.actionEventMapMovesCursor Config.delKeys "Delete" . setExpr
    mkCallWithArgEvent =
      weakerEvents . Widget.actionEventMapMovesCursor Config.addNextArgumentKeys "Add another argument" $
      mkCallWithArg expressionPtr
    setExpr newExprI = do
      Property.set expressionPtr newExprI
      return $ AnimIds.fromIRef newExprI
  expr <- getP expressionRef
  case expr of
    Data.ExpressionGetVariable (Data.GetVariable nameI) ->
      wrap FocusDelegator.defaultKeys FocusDelegator.NotDelegating .
        (fmap . liftM) (flip (,) (AnimIds.fromIRef expressionI)) .
        BWidgets.makeWordEdit $ Transaction.fromIRef nameI
    Data.ExpressionApply (Data.Apply funcI argI) ->
      wrap exprKeys FocusDelegator.Delegating $ \animId ->
        assignCursor animId (AnimIds.fromIRef argI) $ do
          let
            funcIPtr =
              Property (return funcI) $ Property.set expressionRef . Data.ExpressionApply . (`Data.Apply` argI)
            argIPtr =
              Property (return argI) $ Property.set expressionRef . Data.ExpressionApply . (funcI `Data.Apply`)
          (funcEdit, funcAnimId) <- mkDelEvent argI $ makeExpressionEdit False funcIPtr
          (argEdit, _) <- mkCallWithArgEvent . mkDelEvent funcI $ makeExpressionEdit True argIPtr
          let label str = BWidgets.makeTextView str $ Anim.joinId funcAnimId [pack str]
          before <- label "("
          after <- label ")"
          return
            ((hbox . concat) [[before | isArgument], [funcEdit], [spaceWidget], [argEdit], [after | isArgument]]
            ,funcAnimId)

hboxSpaced :: [Widget f] -> Widget f
hboxSpaced = hbox . intersperse spaceWidget

makeDefinitionEdit :: MonadF m => IRef Data.Definition -> TWidget ViewTag m
makeDefinitionEdit definitionI = do
  Data.Definition params _ <- getP definitionRef
  nameEdit <-
    assignCursor animId nameEditAnimId $
    makeNameEdit "<unnamed>" definitionI nameEditAnimId
  equals <- BWidgets.makeTextView "=" $ Anim.joinId animId ["equals"]
  (expressionEdit, _) <- makeExpressionEdit False bodyRef
  paramsEdits <- mapM makeParamEdit $ enumerate params
  return .
    Widget.strongerEvents eventMap . hboxSpaced $
    [nameEdit] ++ paramsEdits ++ [equals, expressionEdit]
  where
    makeParamEdit (i, paramI) =
      (liftM . Widget.strongerEvents) (paramEventMap paramI) .
      BWidgets.wrapDelegated FocusDelegator.NotDelegating (makeNameEdit ("<unnamed param " ++ show i ++ ">") paramI) $
      AnimIds.fromIRef paramI
    bodyRef = Property.composeLabel Data.defBody Data.atDefBody definitionRef
    definitionRef = Transaction.fromIRef definitionI
    paramEventMap paramI =
      Widget.actionEventMapMovesCursor Config.delParamKeys "Delete Parameter" .
      (liftM . const) animId $
      Data.delParameter definitionRef paramI
    eventMap =
      Widget.actionEventMapMovesCursor Config.addParamKeys "Add Parameter" .
      liftM (AnimIds.delegating . AnimIds.fromIRef) $
      Data.addParameter definitionRef
    nameEditAnimId = Anim.joinId animId ["name"]
    animId = AnimIds.fromIRef definitionI

-- Apply the transactions to the given View and convert them to
-- transactions on a DB
makeWidgetForView :: MonadF m => View -> TWidget DBTag m
makeWidgetForView view = do
  versionData <- transaction $ Version.versionData =<< View.curVersion view
  focusable <-
    runNestedCTransaction store .
    (liftM . Widget.atEvents) (>>= applyAndReturn saveCursor) $
    makeDefinitionEdit Anchors.rootIRef
  let undoEventMap = maybe mempty makeUndoEventMap (Version.parent versionData)
  return $ Widget.strongerEvents undoEventMap focusable
  where
    makeUndoEventMap = Widget.actionEventMapMovesCursor Config.undoKeys "Undo" . (>> fetchRevisionCursor) . View.move view
    fetchRevisionCursor = Transaction.run store $ Property.get Anchors.cursor
    store = Anchors.viewStore view
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
      BWidgets.wrapDelegated FocusDelegator.NotDelegating
      (BWidgets.makeTextEdit (Transaction.fromIRef textEditModelIRef)) $
      AnimIds.fromIRef textEditModelIRef
  branchSelector <-
    BWidgets.makeChoice AnimIds.branchSelection branchIndexRef
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
