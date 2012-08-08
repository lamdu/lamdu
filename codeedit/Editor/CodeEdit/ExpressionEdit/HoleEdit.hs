{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Editor.CodeEdit.ExpressionEdit.HoleEdit(make, ResultPicker) where

import Control.Arrow (first, second, (&&&))
import Control.Monad (liftM, mplus)
import Control.Monad.ListT (ListT)
import Data.Function (on)
import Data.Hashable(hash)
import Data.List (isInfixOf, isPrefixOf)
import Data.List.Utils (sortOn)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.Property (Property(..))
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import Editor.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Editor.ITransaction (ITransaction)
import Editor.MonadF (MonadF)
import Editor.OTransaction (OTransaction, TWidget, WidgetT)
import Graphics.UI.Bottle.Animation(AnimId)
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.Char as Char
import qualified Data.List.Class as List
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Editor.Anchors as Anchors
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.Config as Config
import qualified Editor.Data as Data
import qualified Editor.ITransaction as IT
import qualified Editor.OTransaction as OT
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified System.Random as Random

type ResultPicker m = ITransaction ViewTag m Widget.EventResult

data Result = Result
  { resultNames :: [String]
  , resultExpr :: Data.PureGuidExpression
  }
AtFieldTH.make ''Result

type T = Transaction ViewTag

data HoleInfo m = HoleInfo
  { hiHoleId :: Widget.Id
  , hiSearchTerm :: Property (T m) String
  , hiHole :: Sugar.Hole m
  , hiPickResult :: Data.PureGuidExpression -> T m Guid
  , hiGuid :: Guid
  }

pickExpr
  :: Monad m => HoleInfo m -> Data.PureGuidExpression -> ResultPicker m
pickExpr holeInfo expr = do
  guid <- IT.transaction $ hiPickResult holeInfo expr
  return Widget.EventResult
    { Widget.eCursor = Just $ WidgetIds.fromGuid guid
    , Widget.eAnimIdMapping = id -- TODO: Need to fix the parens id
    }

resultPickEventMap
  :: Monad m
  => HoleInfo m -> Data.PureGuidExpression -> Widget.EventHandlers (ITransaction ViewTag m)
resultPickEventMap holeInfo =
  E.keyPresses Config.pickResultKeys "Pick this search result" .
  pickExpr holeInfo

resultToWidget
  :: MonadF m
  => ExpressionGui.Maker m -> HoleInfo m -> Data.PureGuidExpression -> TWidget ViewTag m
resultToWidget makeExpressionEdit holeInfo expr =
  BWidgets.makeFocusableView myId .
  Widget.strongerEvents (resultPickEventMap holeInfo expr) .
  ExpressionGui.egWidget =<< makeExpressionEdit =<<
  (OT.transaction . Sugar.convertExpressionPure) expr
  where
    myId =
      hiHoleId holeInfo `mappend`
      (WidgetIds.fromGuid . Data.geGuid . Data.unPureGuidExpression) expr

makeNoResults :: MonadF m => AnimId -> TWidget t m
makeNoResults myId =
  BWidgets.makeTextView "(No results)" $ mappend myId ["no results"]

makeMoreResults :: MonadF m => AnimId -> TWidget t m
makeMoreResults myId =
  BWidgets.makeTextView "..." $ mappend myId ["more results"]

makeResultVariable ::
  MonadF m => (Guid, Data.VariableRef) -> OTransaction ViewTag m Result
makeResultVariable (guid, varRef) = do
  varName <- OT.getName guid
  return Result
    { resultNames = [varName]
    , resultExpr = toPureGuidExpr $ Data.ExpressionGetVariable varRef
    }

toPureGuidExpr
  :: Data.Expression Data.PureGuidExpression -> Data.PureGuidExpression
toPureGuidExpr = Data.PureGuidExpression . Data.GuidExpression zeroGuid

zeroGuid :: Guid
zeroGuid = Guid.fromString "ZeroGuid"

renamePrefix :: AnimId -> AnimId -> AnimId -> AnimId
renamePrefix srcPrefix destPrefix animId =
  maybe animId (Anim.joinId destPrefix) $
  Anim.subId srcPrefix animId

holeResultAnimMappingNoParens :: HoleInfo m -> Widget.Id -> AnimId -> AnimId
holeResultAnimMappingNoParens holeInfo resultId =
  renamePrefix ("old hole" : Widget.toAnimId resultId) myId .
  renamePrefix myId ("old hole" : myId)
  where
    myId = Widget.toAnimId $ hiHoleId holeInfo

resultOrdering :: String -> Result -> [Bool]
resultOrdering searchTerm result =
  map not
  [ match (==)
  , match isPrefixOf
  , match insensitivePrefixOf
  , match isInfixOf
  ]
  where
    insensitivePrefixOf = isPrefixOf `on` map Char.toLower
    match f = any (f searchTerm) names
    names = resultNames result

makeLiteralResults :: String -> [Result]
makeLiteralResults searchTerm =
  [ makeLiteralIntResult (read searchTerm)
  | not (null searchTerm) && all Char.isDigit searchTerm]
  where
    makeLiteralIntResult integer =
      Result
      { resultNames = [show integer]
      , resultExpr = toPureGuidExpr $ Data.ExpressionLiteralInteger integer
      }

makeAllResults
  :: MonadF m
  => HoleInfo m
  -> OTransaction ViewTag m (ListT (T m) Data.PureGuidExpression)
makeAllResults holeInfo = do
  globals <- OT.getP Anchors.globals
  varResults <-
    mapM makeResultVariable $
    Sugar.holeScope hole ++
    map (Data.variableRefGuid &&& id) globals
  let
    searchTerm = Property.value $ hiSearchTerm holeInfo
    literalResults = makeLiteralResults searchTerm
    nameMatch = any (insensitiveInfixOf searchTerm) . resultNames
    typeMatches = Sugar.holeInferResults hole . resultExpr
  return . (>>= typeMatches) . List.fromList .
    sortOn (resultOrdering searchTerm) .
    filter nameMatch $
    literalResults ++ primitiveResults ++ varResults
  where
    insensitiveInfixOf = isInfixOf `on` map Char.toLower
    hole = hiHole holeInfo
    primitiveResults =
      [ Result
        { resultNames = ["->", "Pi", "→", "→", "Π", "π"]
        , resultExpr =
          toPureGuidExpr . Data.ExpressionPi $
          Data.Lambda holeExpr holeExpr
        }
      , Result
        { resultNames = ["\\", "Lambda", "Λ", "λ"]
        , resultExpr =
          toPureGuidExpr . Data.ExpressionLambda $
          Data.Lambda holeExpr holeExpr
        }
      ]

holeExpr :: Data.PureGuidExpression
holeExpr = toPureGuidExpr Data.ExpressionHole

makeSearchTermWidget
  :: MonadF m
  => HoleInfo m -> Widget.Id
  -> [Data.PureGuidExpression] -> TWidget ViewTag m
makeSearchTermWidget holeInfo searchTermId firstResults =
  liftM
    (Widget.strongerEvents searchTermEventMap .
     (Widget.atWEventMap . E.filterChars) (`notElem` "`[]\\")) $
    BWidgets.makeWordEdit (hiSearchTerm holeInfo) searchTermId
  where
    pickFirstResultEventMaps =
      map (resultPickEventMap holeInfo) $ take 1 firstResults

    searchTermEventMap =
      mconcat pickFirstResultEventMaps `mappend`
      (E.keyPresses Config.newDefinitionKeys
       "Add new as Definition" . makeNewDefinition)
      (pickExpr holeInfo)

    makeNewDefinition holePickResult = do
      newDefI <- IT.transaction $ do
        newDefI <- Anchors.makeDefinition -- TODO: From Sugar
        let
          searchTerm = Property.value $ hiSearchTerm holeInfo
          newName = concat . words $ searchTerm
        Anchors.setP (Anchors.assocNameRef (IRef.guid newDefI)) newName
        Anchors.newPane newDefI
        return newDefI
      let
        defRef =
          toPureGuidExpr . Data.ExpressionGetVariable $
          Data.DefinitionRef newDefI
      -- TODO: Can we use pickResult's animIdMapping?
      eventResult <- holePickResult defRef
      maybe (return ()) (IT.transaction . Anchors.savePreJumpPosition) $
        Widget.eCursor eventResult
      return Widget.EventResult {
        Widget.eCursor = Just $ WidgetIds.fromIRef newDefI,
        Widget.eAnimIdMapping =
          holeResultAnimMappingNoParens holeInfo searchTermId
        }

makeResultsWidget
  :: MonadF m
  => ExpressionGui.Maker m -> HoleInfo m
  -> [Data.PureGuidExpression] -> Bool
  -> OTransaction ViewTag m
     (Maybe Data.PureGuidExpression, WidgetT ViewTag m)
makeResultsWidget makeExpressionEdit holeInfo firstResults moreResults = do
  firstResultsAndWidgets <- mapM resultAndWidget firstResults
  (mResult, firstResultsWidget) <-
    case firstResultsAndWidgets of
      [] -> liftM ((,) Nothing) . makeNoResults $ Widget.toAnimId myId
      xs -> do
        let
          widget = blockDownEvents . BWidgets.vboxAlign 0 $ map snd xs
          mResult =
            listToMaybe . map fst $ filter (Widget.wIsFocused . snd) xs
        return (mResult, widget)
  moreResultsWidgets <-
    if moreResults
    then liftM (: []) . makeMoreResults $ Widget.toAnimId myId
    else return []

  return
    ( mResult
    , Widget.scale Config.holeResultScaleFactor $
      BWidgets.vboxCentered (firstResultsWidget : moreResultsWidgets)
    )
  where
    myId = hiHoleId holeInfo
    resultAndWidget result =
      liftM ((,) result) $ resultToWidget makeExpressionEdit holeInfo result
    blockDownEvents =
      Widget.weakerEvents $
      Widget.keysEventMap
      [E.ModKey E.noMods E.KeyDown]
      "Nothing (at bottom)" (return ())

canonizeResultExpr
  :: HoleInfo m
  -> Data.PureGuidExpression -> Data.PureGuidExpression
canonizeResultExpr holeInfo expr =
  flip Data.canonizeIdentifiers expr . Random.mkStdGen $
  hash (show expr, Guid.bs (hiGuid holeInfo))

makeActiveHoleEdit
  :: MonadF m
  => ExpressionGui.Maker m -> HoleInfo m
  -> OTransaction ViewTag m
     (Maybe Data.PureGuidExpression, WidgetT ViewTag m)
makeActiveHoleEdit makeExpressionEdit holeInfo =
  OT.assignCursor (hiHoleId holeInfo) searchTermId $ do
    OT.markVariablesAsUsed . map fst . Sugar.holeScope $ hiHole holeInfo

    allResults <- makeAllResults holeInfo

    (firstResults, moreResults) <-
      OT.transaction . List.splitAtM Config.holeResultCount $
      liftM (canonizeResultExpr holeInfo) allResults

    searchTermWidget <-
      makeSearchTermWidget holeInfo searchTermId firstResults

    hasMoreResults <- liftM null . OT.transaction . List.toList $ List.take 1 moreResults

    (mResult, resultsWidget) <-
      makeResultsWidget makeExpressionEdit holeInfo firstResults $
      not hasMoreResults
    return
      ( mplus mResult (listToMaybe $ take 1 firstResults)
      , BWidgets.vboxCentered [searchTermWidget, resultsWidget] )
  where
    searchTermId = WidgetIds.searchTermId $ hiHoleId holeInfo

make
  :: MonadF m
  => ExpressionGui.Maker m
  -> Sugar.Hole m -> Guid -> Widget.Id
  -> OTransaction ViewTag m
     (Maybe (ResultPicker m), ExpressionGui m)
make makeExpressionEdit hole guid myId = do
  cursor <- OT.readCursor
  searchTermProp <-
    liftM (Property.pureCompose (fromMaybe "") Just) . OT.transaction $
    Anchors.assocDataRef "searchTerm" guid
  let
    searchText = Property.value searchTermProp
    snippet
      | null searchText = "  "
      | otherwise = searchText
  (liftM . second) ExpressionGui.fromValueWidget $
    case (Sugar.holePickResult hole, Widget.subId myId cursor) of
    (Just holePickResult, Just _) ->
      let
        holeInfo = HoleInfo
          { hiHoleId = myId
          , hiSearchTerm = searchTermProp
          , hiHole = hole
          , hiPickResult = holePickResult
          , hiGuid = guid
          }
      in
        liftM
        (first (fmap (pickExpr holeInfo) . maybeRemovePicker searchText) .
         second (makeBackground 11 Config.focusedHoleBackgroundColor)) $
        makeActiveHoleEdit makeExpressionEdit holeInfo
    _ ->
      liftM
      ((,) Nothing . makeBackground 12 unfocusedColor) .
      BWidgets.makeFocusableTextView snippet $
      WidgetIds.searchTermId myId
  where
    -- TODO: If just one possible result due to type, ignore the
    -- search string
    maybeRemovePicker "" = const Nothing
    maybeRemovePicker _ = id
    unfocusedColor
      | canPickResult = Config.unfocusedHoleBackgroundColor
      | otherwise = Config.unfocusedReadOnlyHoleBackgroundColor
    canPickResult = isJust $ Sugar.holePickResult hole
    makeBackground level =
      Widget.backgroundColor level $
      mappend (Widget.toAnimId myId) ["hole background"]
