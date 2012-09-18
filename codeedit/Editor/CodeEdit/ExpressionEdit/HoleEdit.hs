{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Editor.CodeEdit.ExpressionEdit.HoleEdit
  ( make, makeUnwrapped
  , searchTermWidgetId
  ) where

import Control.Applicative ((<*>))
import Control.Arrow (first, second, (&&&))
import Control.Monad ((<=<), liftM, mplus, msum, void, filterM)
import Control.Monad.ListT (ListT)
import Data.Function (on)
import Data.Hashable(hash)
import Data.List (isInfixOf, isPrefixOf)
import Data.List.Class (List)
import Data.List.Utils (sortOn)
import Data.Maybe (isJust, listToMaybe, maybeToList, mapMaybe, fromMaybe)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.Property (Property(..))
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import Editor.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui(..))
import Editor.CodeEdit.VarAccess (VarAccess, WidgetT)
import Editor.ITransaction (ITransaction)
import Editor.MonadF (MonadF)
import Graphics.UI.Bottle.Animation(AnimId)
import Graphics.UI.Bottle.Widget (Widget)
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
import qualified Editor.CodeEdit.VarAccess as VarAccess
import qualified Editor.Config as Config
import qualified Editor.Data as Data
import qualified Editor.ITransaction as IT
import qualified Editor.Layers as Layers
import qualified Editor.OTransaction as OT
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified System.Random as Random

moreSymbol :: String
moreSymbol = "▷"

moreSymbolSizeFactor :: Fractional a => a
moreSymbolSizeFactor = 0.5

data Group = Group
  { groupNames :: [String]
  , groupBaseExpr :: Data.PureExpression
  }
AtFieldTH.make ''Group

type T = Transaction ViewTag

data HoleInfo m = HoleInfo
  { hiHoleId :: Widget.Id
  , hiSearchTerm :: Property (T m) String
  , hiHole :: Sugar.Hole m
  , hiPickResult :: Sugar.HoleResult -> T m (Guid, Sugar.Actions m)
  , hiGuid :: Guid
  , hiMNextHole :: Maybe (Sugar.Expression m)
  }

pickExpr ::
  Monad m => HoleInfo m -> Sugar.HoleResult -> ITransaction ViewTag m Widget.EventResult
pickExpr holeInfo expr = do
  (guid, _) <- IT.transaction $ hiPickResult holeInfo expr
  return Widget.EventResult
    { Widget.eCursor = Just $ WidgetIds.fromGuid guid
    , Widget.eAnimIdMapping = id -- TODO: Need to fix the parens id
    }

resultPickEventMap
  :: Monad m
  => HoleInfo m -> Sugar.HoleResult -> Widget.EventHandlers (ITransaction ViewTag m)
resultPickEventMap holeInfo holeResult =
  case hiMNextHole holeInfo of
  Just nextHole
    | not (Sugar.holeResultHasHoles holeResult) ->
      mappend (simplePickRes Config.pickResultKeys) .
      E.keyPresses Config.addNextArgumentKeys
      "Pick result and move to next arg" .
      liftM Widget.eventResultFromCursor $ do
        _ <- IT.transaction $ hiPickResult holeInfo holeResult
        return . WidgetIds.fromGuid $ Sugar.rGuid nextHole
  _ -> simplePickRes $ Config.pickResultKeys ++ Config.addNextArgumentKeys
  where
    simplePickRes keys =
      E.keyPresses keys "Pick this search result" $
      pickExpr holeInfo holeResult


data ResultsList = ResultsList
  { rlFirstId :: Widget.Id
  , rlMoreResultsPrefixId :: Widget.Id
  , rlFirst :: Sugar.HoleResult
  , rlMore :: Maybe [Sugar.HoleResult]
  }

randomizeResultExpr
  :: HoleInfo m
  -> Data.Expression a -> Data.Expression a
randomizeResultExpr holeInfo expr =
  flip Data.randomizeGuids expr . Random.mkStdGen $
  hash (show (void expr), Guid.bs (hiGuid holeInfo))

resultsToWidgets
  :: MonadF m
  => ExpressionGui.Maker m -> HoleInfo m -> ResultsList
  -> VarAccess m
     ( WidgetT m
     , Maybe (Sugar.HoleResult, Maybe (WidgetT m))
     )
resultsToWidgets makeExpressionEdit holeInfo results = do
  cursorOnMain <- VarAccess.otransaction $ OT.isSubCursor myId
  extra <-
    if cursorOnMain
    then liftM (Just . (,) canonizedExpr . fmap fst) makeExtra
    else do
      cursorOnExtra <- VarAccess.otransaction $ OT.isSubCursor moreResultsPrefixId
      if cursorOnExtra
        then do
          extra <- makeExtra
          return $ do
            (widget, mResult) <- extra
            result <- mResult
            return (result, Just widget)
        else return Nothing
  liftM (flip (,) extra) .
    maybe return (const addMoreSymbol) (rlMore results) =<<
    toWidget myId canonizedExpr
  where
    makeExtra = maybe (return Nothing) (liftM Just . makeMoreResults) $ rlMore results
    makeMoreResults moreResults = do
      pairs <- mapM moreResult moreResults
      return
        ( Box.vboxAlign 0 $ map fst pairs
        , msum $ map snd pairs
        )
    moreResult expr = do
      mResult <- (liftM . fmap . const) cExpr . VarAccess.otransaction $ OT.subCursor resultId
      widget <- toWidget resultId cExpr
      return (widget, mResult)
      where
        resultId = mappend moreResultsPrefixId $ pureGuidId cExpr
        cExpr = randomizeResultExpr holeInfo expr
    toWidget resultId expr =
      VarAccess.otransaction . BWidgets.makeFocusableView resultId .
      Widget.strongerEvents (resultPickEventMap holeInfo expr) .
      ExpressionGui.egWidget =<<
      makeExpressionEdit . Sugar.removeTypes =<<
      (VarAccess.transaction . Sugar.convertHoleResult) expr
    moreResultsPrefixId = rlMoreResultsPrefixId results
    addMoreSymbol w = do
      moreSymbolLabel <-
        liftM (Widget.scale moreSymbolSizeFactor) .
        VarAccess.otransaction .
        BWidgets.makeLabel moreSymbol $ Widget.toAnimId myId
      return $ BWidgets.hboxCenteredSpaced [w, moreSymbolLabel]
    canonizedExpr = rlFirst results
    myId = rlFirstId results
    pureGuidId = WidgetIds.fromGuid . Data.eGuid

makeNoResults :: MonadF m => AnimId -> VarAccess m (WidgetT m)
makeNoResults myId =
  VarAccess.otransaction .
  BWidgets.makeTextView "(No results)" $ mappend myId ["no results"]

makeVariableGroup ::
  MonadF m => (Sugar.GetVariable, Data.VariableRef) -> VarAccess m Group
makeVariableGroup (getVar, varRef) = VarAccess.withNameFromVarRef getVar $ \(_, varName) ->
  return Group
    { groupNames = [varName]
    , groupBaseExpr = toPureExpr . Data.ExpressionLeaf $ Data.GetVariable varRef
    }

toPureExpr
  :: Data.ExpressionBody Data.PureExpression -> Data.PureExpression
toPureExpr = Data.pureExpression $ Guid.fromString "ZeroGuid"

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

groupOrdering :: String -> Group -> [Bool]
groupOrdering searchTerm result =
  map not
  [ match (==)
  , match isPrefixOf
  , match insensitivePrefixOf
  , match isInfixOf
  ]
  where
    insensitivePrefixOf = isPrefixOf `on` map Char.toLower
    match f = any (f searchTerm) names
    names = groupNames result

makeLiteralGroup :: String -> [Group]
makeLiteralGroup searchTerm =
  [ makeLiteralIntResult (read searchTerm)
  | not (null searchTerm) && all Char.isDigit searchTerm]
  where
    makeLiteralIntResult integer =
      Group
      { groupNames = [show integer]
      , groupBaseExpr = toPureExpr . Data.ExpressionLeaf $ Data.LiteralInteger integer
      }

resultsPrefixId :: HoleInfo m -> Widget.Id
resultsPrefixId holeInfo = mconcat [hiHoleId holeInfo, Widget.Id ["results"]]

toResultsList ::
  Monad m =>
  HoleInfo m -> Data.PureExpression ->
  T m (Maybe ResultsList)
toResultsList holeInfo baseExpr = do
  results <- Sugar.holeInferResults (hiHole holeInfo) baseExpr
  return $
    case results of
    [] -> Nothing
    (x:xs) ->
      let
        canonizedExpr = randomizeResultExpr holeInfo x
        canonizedExprId = WidgetIds.fromGuid $ Data.eGuid canonizedExpr
      in Just ResultsList
        { rlFirstId = mconcat [resultsPrefixId holeInfo, canonizedExprId]
        , rlMoreResultsPrefixId = mconcat [resultsPrefixId holeInfo, Widget.Id ["more results"], canonizedExprId]
        , rlFirst = canonizedExpr
        , rlMore =
          case xs of
          [] -> Nothing
          _ -> Just xs
        }

data ResultType = GoodResult | BadResult

makeResultsList ::
  Monad m => HoleInfo m -> Data.PureExpression ->
  T m (Maybe (ResultType, ResultsList))
makeResultsList holeInfo baseExpr = do
  -- We always want the first, and we want to know if there's more, so
  -- take 2:
  mRes <- toResultsList holeInfo baseExpr
  case mRes of
    Just res -> return . Just $ (GoodResult, res)
    Nothing ->
      (liftM . fmap) ((,) BadResult) . toResultsList holeInfo $ holeApply baseExpr
  where
    holeApply =
      toPureExpr .
      (Data.makeApply . toPureExpr . Data.ExpressionLeaf) Data.Hole

makeAllResults
  :: MonadF m
  => HoleInfo m
  -> VarAccess m (ListT (T m) (ResultType, ResultsList))
makeAllResults holeInfo = do
  paramResults <-
    mapM (makeVariableGroup . first Sugar.GetParameter) $
    Sugar.holeScope hole
  globalResults <-
    mapM (makeVariableGroup . (Sugar.GetDefinition &&& Data.DefinitionRef)) =<<
    VarAccess.getP Anchors.globals
  let
    searchTerm = Property.value $ hiSearchTerm holeInfo
    literalResults = makeLiteralGroup searchTerm
    nameMatch = any (insensitiveInfixOf searchTerm) . groupNames
  return .
    List.catMaybes .
    List.mapL
      (makeResultsList holeInfo . groupBaseExpr) .
    List.fromList .
    sortOn (groupOrdering searchTerm) .
    filter nameMatch $
    literalResults ++
    paramResults ++
    primitiveResults ++
    globalResults
  where
    insensitiveInfixOf = isInfixOf `on` map Char.toLower
    hole = hiHole holeInfo
    primitiveResults =
      [ Group
        { groupNames = ["Set", "Type"]
        , groupBaseExpr = toPureExpr $ Data.ExpressionLeaf Data.Set
        }
      , Group
        { groupNames = ["Integer", "ℤ", "Z"]
        , groupBaseExpr = toPureExpr $ Data.ExpressionLeaf Data.IntegerType
        }
      , Group
        { groupNames = ["->", "Pi", "→", "→", "Π", "π"]
        , groupBaseExpr =
          Data.canonizeGuids . toPureExpr . Data.ExpressionPi $
          Data.Lambda holeExpr holeExpr
        }
      , Group
        { groupNames = ["\\", "Lambda", "Λ", "λ"]
        , groupBaseExpr =
          Data.canonizeGuids . toPureExpr . Data.ExpressionLambda $
          Data.Lambda holeExpr holeExpr
        }
      ]
    holeExpr = toPureExpr $ Data.ExpressionLeaf Data.Hole

addNewDefinitionEventMap ::
  Monad m =>
  HoleInfo m ->
  Widget.EventHandlers (ITransaction ViewTag m)
addNewDefinitionEventMap holeInfo =
  E.keyPresses Config.newDefinitionKeys
  "Add as new Definition" . makeNewDefinition $
  pickExpr holeInfo
  where
    makeNewDefinition holePickResult = do
      newDefI <- IT.transaction $ do
        newDefI <- Anchors.makeDefinition -- TODO: From Sugar
        let
          searchTerm = Property.value $ hiSearchTerm holeInfo
          newName = concat . words $ searchTerm
        Anchors.setP (Anchors.assocNameRef (IRef.guid newDefI)) newName
        Anchors.newPane newDefI
        return newDefI
      defRef <-
        liftM (fromMaybe (error "GetDef should always type-check") . listToMaybe) .
        IT.transaction .
        Sugar.holeInferResults (hiHole holeInfo) .
        toPureExpr . Data.ExpressionLeaf . Data.GetVariable $
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
    searchTermId = WidgetIds.searchTermId $ hiHoleId holeInfo

makeSearchTermWidget
  :: MonadF m
  => HoleInfo m -> Widget.Id
  -> Maybe Sugar.HoleResult
  -> VarAccess m (ExpressionGui m)
makeSearchTermWidget holeInfo searchTermId mResultToPick =
  VarAccess.otransaction .
  liftM
  (flip ExpressionGui (0.5/Config.holeSearchTermScaleFactor) .
   Widget.scale Config.holeSearchTermScaleFactor .
   Widget.strongerEvents pickResultEventMap .
   (Widget.atWEventMap . E.filterChars) (`notElem` "`[]\\")) $
  BWidgets.makeWordEdit (hiSearchTerm holeInfo) searchTermId
  where
    pickResultEventMap =
      maybe mempty (resultPickEventMap holeInfo) mResultToPick

vboxMBiasedAlign ::
  Maybe Box.Cursor -> Box.Alignment -> [Widget f] -> Widget f
vboxMBiasedAlign mChildIndex align =
  maybe Box.toWidget Box.toWidgetBiased mChildIndex .
  Box.makeAlign align Box.vertical

makeResultsWidget
  :: MonadF m
  => ExpressionGui.Maker m -> HoleInfo m
  -> [ResultsList] -> Bool
  -> VarAccess m
     (Maybe Sugar.HoleResult, WidgetT m)
makeResultsWidget makeExpressionEdit holeInfo firstResults moreResults = do
  firstResultsAndWidgets <-
    mapM (resultsToWidgets makeExpressionEdit holeInfo) firstResults
  (mResult, firstResultsWidget) <-
    case firstResultsAndWidgets of
    [] -> liftM ((,) Nothing) . makeNoResults $ Widget.toAnimId myId
    xs -> do
      let
        mResult =
          listToMaybe . mapMaybe snd $
          zipWith (second . fmap . (,)) [0..] xs
      return
        ( mResult
        , blockDownEvents . vboxMBiasedAlign (fmap fst mResult) 0 $
          map fst xs
        )
  let extraWidgets = maybeToList $ snd . snd =<< mResult
  moreResultsWidgets <-
    VarAccess.otransaction $
    if moreResults
    then liftM (: []) . BWidgets.makeLabel "..." $ Widget.toAnimId myId
    else return []
  return
    ( fmap (fst . snd) mResult
    , Widget.scale Config.holeResultScaleFactor .
      BWidgets.hboxCenteredSpaced $
      Box.vboxCentered (firstResultsWidget : moreResultsWidgets) :
      extraWidgets
    )
  where
    myId = hiHoleId holeInfo
    blockDownEvents =
      Widget.weakerEvents $
      Widget.keysEventMap
      [E.ModKey E.noMods E.KeyDown]
      "Nothing (at bottom)" (return ())

adHocTextEditEventMap :: Monad m => Property m String -> Widget.EventHandlers m
adHocTextEditEventMap textProp =
  mconcat . concat $
  [ [ E.filterChars (`notElem` " \n") .
      E.simpleChars "Character" "Append search term character" $
      changeText . flip (++) . (: [])
    ]
  , [ E.keyPresses (map (E.ModKey E.noMods) [E.KeyBackspace, E.KeyDel]) "Delete backwards" $
      changeText init
    | (not . null . Property.value) textProp
    ]
  ]
  where
    changeText f = do
      Property.pureModify textProp f
      return Widget.emptyEventResult

collectResults :: List l => l (ResultType, a) -> List.ItemM l ([a], Bool)
collectResults =
  conclude <=<
  List.splitWhenM (return . (>= Config.holeResultCount) . length . fst) .
  List.scanl step ([], [])
  where
    conclude (notEnoughResults, enoughResultsM) =
      liftM
      ( second (not . null) . splitAt Config.holeResultCount
      . uncurry (on (++) reverse) . last . mappend notEnoughResults
      ) .
      List.toList $
      List.take 2 enoughResultsM
    step results (tag, x) =
      ( case tag of
        GoodResult -> first
        BadResult -> second
      ) (x :) results

makeActiveHoleEdit
  :: MonadF m
  => ExpressionGui.Maker m -> HoleInfo m
  -> VarAccess m (ExpressionGui m)
makeActiveHoleEdit makeExpressionEdit holeInfo = do
  VarAccess.markVariablesAsUsed . map fst =<<
    (filterM
     (checkInfer . toPureExpr .
      Data.ExpressionLeaf . Data.GetVariable . snd) .
     Sugar.holeScope . hiHole)
    holeInfo

  allResults <- makeAllResults holeInfo

  (firstResults, hasMoreResults) <- VarAccess.transaction $ collectResults allResults

  cursor <- VarAccess.otransaction OT.readCursor
  let
    sub = isJust . flip Widget.subId cursor
    shouldBeOnResult = sub $ resultsPrefixId holeInfo
    isOnResult = any sub $ [rlFirstId, rlMoreResultsPrefixId] <*> firstResults
    assignSource
      | shouldBeOnResult && not isOnResult = cursor
      | otherwise = hiHoleId holeInfo
    destId = head (map rlFirstId firstResults ++ [searchTermId])
  VarAccess.assignCursor assignSource destId $ do
    (mSelectedResult, resultsWidget) <-
      makeResultsWidget makeExpressionEdit holeInfo firstResults hasMoreResults
    let
      mResult =
        mplus mSelectedResult .
        fmap rlFirst $ listToMaybe firstResults
      adHocEditor =
        fmap IT.transaction . adHocTextEditEventMap $
        hiSearchTerm holeInfo
      eventMap =
        case mResult of
        Just result
          | null searchTerm -> mempty
          | all (`notElem` Config.operatorChars) searchTerm ->
            (fmap . fmap) Widget.eventResultFromCursor .
            E.charGroup "Operator"
            "Pick this result and apply operator"
            Config.operatorChars . fmap const $
            \x -> IT.transaction $ do
              (_, actions) <- hiPickResult holeInfo result
              liftM (searchTermWidgetId . WidgetIds.fromGuid) $
                Sugar.giveAsArgToOperator actions [x]
        _ -> mempty
    searchTermWidget <- makeSearchTermWidget holeInfo searchTermId mResult
    return .
      ExpressionGui.atEgWidget (Widget.strongerEvents eventMap) $
      ExpressionGui.addBelow
      [ (0.5, Widget.strongerEvents adHocEditor resultsWidget)
      ]
      searchTermWidget
  where
    checkInfer =
      liftM (isJust . listToMaybe) . VarAccess.transaction .
      Sugar.holeInferResults (hiHole holeInfo)
    searchTermId = WidgetIds.searchTermId $ hiHoleId holeInfo
    searchTerm = Property.value $ hiSearchTerm holeInfo

holeFDConfig :: FocusDelegator.Config
holeFDConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKey = E.ModKey E.noMods E.KeyEnter
  , FocusDelegator.startDelegatingDoc = "Enter hole"
  , FocusDelegator.stopDelegatingKey = E.ModKey E.noMods E.KeyEsc
  , FocusDelegator.stopDelegatingDoc = "Leave hole"
  }

make ::
  MonadF m =>
  ExpressionGui.Maker m ->
  Sugar.Hole m -> Maybe (Sugar.Expression m) -> Guid ->
  Widget.Id ->
  VarAccess m (ExpressionGui m)
make makeExpressionEdit hole mNextHole guid =
  BWidgets.wrapDelegatedVA holeFDConfig FocusDelegator.Delegating ExpressionGui.atEgWidget $
  makeUnwrapped makeExpressionEdit hole mNextHole guid

makeUnwrapped ::
  MonadF m =>
  ExpressionGui.Maker m ->
  Sugar.Hole m -> Maybe (Sugar.Expression m) -> Guid ->
  Widget.Id ->
  VarAccess m (ExpressionGui m)
makeUnwrapped makeExpressionEdit hole mNextHole guid myId = do
  cursor <- VarAccess.otransaction OT.readCursor
  searchTermProp <- VarAccess.transaction $ Anchors.assocSearchTermRef guid
  case (Sugar.holePickResult hole, Widget.subId myId cursor) of
    (Just holePickResult, Just _) ->
      let
        holeInfo = HoleInfo
          { hiHoleId = myId
          , hiSearchTerm = searchTermProp
          , hiHole = hole
          , hiPickResult = holePickResult
          , hiGuid = guid
          , hiMNextHole = mNextHole
          }
      in
        (liftM . ExpressionGui.atEgWidget)
        (makeBackground Layers.activeHoleBG Config.holeBackgroundColor .
         Widget.strongerEvents (addNewDefinitionEventMap holeInfo)) $
        makeActiveHoleEdit makeExpressionEdit holeInfo
    _ ->
      liftM
      (ExpressionGui.fromValueWidget .
       makeBackground Layers.inactiveHole unfocusedColor) .
      VarAccess.otransaction $ BWidgets.makeFocusableTextView "  " myId
  where
    unfocusedColor
      | canPickResult = Config.holeBackgroundColor
      | otherwise = Config.unfocusedReadOnlyHoleBackgroundColor
    canPickResult = isJust $ Sugar.holePickResult hole
    makeBackground level =
      Widget.backgroundColor level $
      mappend (Widget.toAnimId myId) ["hole background"]

searchTermWidgetId :: Widget.Id -> Widget.Id
searchTermWidgetId = WidgetIds.searchTermId . FocusDelegator.delegatingId
