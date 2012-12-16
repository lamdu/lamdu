{-# LANGUAGE OverloadedStrings, TypeFamilies, FlexibleContexts #-}
module Lamdu.CodeEdit.ExpressionEdit.HoleEdit
  ( make, makeUnwrapped
  , searchTermWidgetId
  ) where

import Control.Applicative (Applicative(..))
import Control.Arrow (first, second)
import Control.Lens ((^.))
import Control.Monad ((<=<), filterM, mplus, msum, void)
import Control.Monad.ListT (ListT)
import Control.Monad.Trans.State (StateT)
import Control.MonadA (MonadA)
import Data.Cache (Cache)
import Data.Function (on)
import Data.List (isInfixOf, isPrefixOf)
import Data.List.Class (List)
import Data.List.Utils (sortOn, nonEmptyAll)
import Data.Maybe (isJust, listToMaybe, maybeToList, mapMaybe, fromMaybe)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.IRef (Tag)
import Data.Store.Property (Property(..))
import Data.Store.Transaction (Transaction)
import Data.Traversable (traverse)
import Graphics.UI.Bottle.Animation(AnimId)
import Graphics.UI.Bottle.Widget (Widget)
import Lamdu.Anchors (ViewM)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui(..))
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM, WidgetT)
import System.Random.Utils (randFunc)
import qualified Control.Lens as Lens
import qualified Data.Char as Char
import qualified Data.List.Class as List
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.Anchors as Anchors
import qualified Lamdu.BottleWidgets as BWidgets
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.Config as Config
import qualified Lamdu.Data as Data
import qualified Lamdu.Data.IRef as DataIRef
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Layers as Layers
import qualified Lamdu.WidgetEnvT as WE
import qualified Lamdu.WidgetIds as WidgetIds

moreSymbol :: String
moreSymbol = "▷"

moreSymbolSizeFactor :: Fractional a => a
moreSymbolSizeFactor = 0.5

data Group def = Group
  { groupNames :: [String]
  , groupBaseExpr :: Data.Expression def ()
  }

type T = Transaction
type CT m = StateT Cache (T m)

data HoleInfo m = HoleInfo
  { hiHoleId :: Widget.Id
  , hiSearchTerm :: Property (Transaction m) String
  , hiHole :: Sugar.Hole m
  , hiHoleActions :: Sugar.HoleActions m
  , hiGuid :: Guid
  , hiMNextHole :: Maybe (Sugar.Expression m)
  }

pickExpr ::
  MonadA m => HoleInfo m -> Sugar.HoleResult (Tag m) ->
  Transaction m Widget.EventResult
pickExpr holeInfo expr = do
  (guid, _) <- Sugar.holePickResult (hiHoleActions holeInfo) expr
  return Widget.EventResult
    { Widget._eCursor = Just $ WidgetIds.fromGuid guid
    , Widget._eAnimIdMapping = id -- TODO: Need to fix the parens id
    }

resultPickEventMap ::
  MonadA m => HoleInfo m -> Sugar.HoleResult (Tag m) ->
  Widget.EventHandlers (Transaction m)
resultPickEventMap holeInfo holeResult =
  case hiMNextHole holeInfo of
  Just nextHole
    | not (Sugar.holeResultHasHoles holeResult) ->
      mappend (simplePickRes Config.pickResultKeys) .
      E.keyPresses Config.addNextArgumentKeys
      (E.Doc ["Edit", "Result", "Pick and move to next hole"]) .
      fmap Widget.eventResultFromCursor $ do
        _ <- Sugar.holePickResult (hiHoleActions holeInfo) holeResult
        return . WidgetIds.fromGuid $ nextHole ^. Sugar.rGuid
  _ -> simplePickRes $ Config.pickResultKeys ++ Config.addNextArgumentKeys
  where
    simplePickRes keys =
      E.keyPresses keys (E.Doc ["Edit", "Result", "Pick"]) $
      pickExpr holeInfo holeResult


data ResultsList t = ResultsList
  { rlFirstId :: Widget.Id
  , rlMoreResultsPrefixId :: Widget.Id
  , rlFirst :: Sugar.HoleResult t
  , rlMore :: Maybe [Sugar.HoleResult t]
  }

resultsToWidgets
  :: MonadA m
  => HoleInfo m -> ResultsList (Tag m)
  -> ExprGuiM m
     ( WidgetT m
     , Maybe (Sugar.HoleResult (Tag m), Maybe (WidgetT m))
     )
resultsToWidgets holeInfo results = do
  cursorOnMain <- ExprGuiM.widgetEnv $ WE.isSubCursor myId
  extra <-
    if cursorOnMain
    then fmap (Just . (,) canonizedExpr . fmap fst) makeExtra
    else do
      cursorOnExtra <- ExprGuiM.widgetEnv $ WE.isSubCursor moreResultsPrefixId
      if cursorOnExtra
        then do
          extra <- makeExtra
          return $ do
            (widget, mResult) <- extra
            result <- mResult
            return (result, Just widget)
        else return Nothing
  fmap (flip (,) extra) .
    maybe return (const addMoreSymbol) (rlMore results) =<<
    toWidget myId canonizedExpr
  where
    makeExtra = traverse makeMoreResults $ rlMore results
    makeMoreResults moreResults = do
      pairs <- traverse moreResult moreResults
      return
        ( Box.vboxAlign 0 $ map fst pairs
        , msum $ map snd pairs
        )
    moreResult expr = do
      mResult <- (fmap . fmap . const) expr . ExprGuiM.widgetEnv $ WE.subCursor resultId
      widget <- toWidget resultId expr
      return (widget, mResult)
      where
        resultId = mappend moreResultsPrefixId $ exprWidgetId expr
    toWidget resultId expr =
      ExprGuiM.widgetEnv . BWidgets.makeFocusableView resultId .
      Widget.strongerEvents (resultPickEventMap holeInfo expr) .
      Lens.view ExpressionGui.egWidget =<<
      ExprGuiM.makeSubexpresion . Sugar.removeTypes =<<
      (ExprGuiM.transaction . Sugar.holeConvertResult (hiHoleActions holeInfo)) expr
    moreResultsPrefixId = rlMoreResultsPrefixId results
    addMoreSymbol w = do
      moreSymbolLabel <-
        fmap (Widget.scale moreSymbolSizeFactor) .
        ExprGuiM.widgetEnv .
        BWidgets.makeLabel moreSymbol $ Widget.toAnimId myId
      return $ BWidgets.hboxCenteredSpaced [w, moreSymbolLabel]
    canonizedExpr = rlFirst results
    myId = rlFirstId results

makeNoResults :: MonadA m => AnimId -> ExprGuiM m (WidgetT m)
makeNoResults myId =
  ExprGuiM.widgetEnv .
  BWidgets.makeTextView "(No results)" $ mappend myId ["no results"]

mkGroup :: [String] -> Data.ExpressionBodyExpr def () -> Group def
mkGroup names body = Group
  { groupNames = names
  , groupBaseExpr = Data.pureExpression body
  }

makeVariableGroup ::
  MonadA m => Data.VariableRef (DataIRef.DefI (Tag m)) ->
  ExprGuiM m (Group (DataIRef.DefI (Tag m)))
makeVariableGroup varRef =
  ExprGuiM.withNameFromVarRef varRef $ \(_, varName) ->
  return . mkGroup [varName] . Data.ExpressionLeaf $ Data.GetVariable varRef

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

groupOrdering :: String -> Group def -> [Bool]
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

makeLiteralGroup :: String -> [Group def]
makeLiteralGroup searchTerm =
  [ makeLiteralIntResult (read searchTerm)
  | nonEmptyAll Char.isDigit searchTerm
  ]
  where
    makeLiteralIntResult integer =
      Group
      { groupNames = [show integer]
      , groupBaseExpr = Data.pureExpression . Data.ExpressionLeaf $ Data.LiteralInteger integer
      }

resultsPrefixId :: HoleInfo m -> Widget.Id
resultsPrefixId holeInfo = mconcat [hiHoleId holeInfo, Widget.Id ["results"]]

exprWidgetId :: Show def => Data.Expression def a -> Widget.Id
exprWidgetId = WidgetIds.fromGuid . randFunc . show . void

toResultsList ::
  MonadA m => HoleInfo m -> Data.Expression (DataIRef.DefI (Tag m)) () ->
  CT m (Maybe (ResultsList (Tag m)))
toResultsList holeInfo baseExpr = do
  results <- Sugar.holeInferResults (hiHole holeInfo) baseExpr
  return $
    case results of
    [] -> Nothing
    (x:xs) ->
      let canonizedExprId = exprWidgetId baseExpr
      in Just ResultsList
        { rlFirstId = mconcat [resultsPrefixId holeInfo, canonizedExprId]
        , rlMoreResultsPrefixId =
          mconcat [resultsPrefixId holeInfo, Widget.Id ["more results"], canonizedExprId]
        , rlFirst = x
        , rlMore =
          case xs of
          [] -> Nothing
          _ -> Just xs
        }

data ResultType = GoodResult | BadResult

makeResultsList ::
  MonadA m => HoleInfo m -> Group (DataIRef.DefI (Tag m)) ->
  CT m (Maybe (ResultType, ResultsList (Tag m)))
makeResultsList holeInfo group = do
  -- We always want the first, and we want to know if there's more, so
  -- take 2:
  mRes <- toResultsList holeInfo baseExpr
  case mRes of
    Just res -> return . Just $ (GoodResult, res)
    Nothing ->
      (fmap . fmap) ((,) BadResult) . toResultsList holeInfo $ holeApply baseExpr
  where
    baseExpr = groupBaseExpr group
    holeApply =
      Data.pureExpression .
      (Data.makeApply . Data.pureExpression . Data.ExpressionLeaf) Data.Hole

makeAllResults
  :: ViewM ~ m => HoleInfo m
  -> ExprGuiM m (ListT (CT m) (ResultType, ResultsList (Tag m)))
makeAllResults holeInfo = do
  paramResults <-
    traverse (makeVariableGroup . Data.ParameterRef) $
    Sugar.holeScope hole
  globalResults <-
    traverse (makeVariableGroup . Data.DefinitionRef) =<<
    ExprGuiM.getP Anchors.globals
  let
    searchTerm = Property.value $ hiSearchTerm holeInfo
    literalResults = makeLiteralGroup searchTerm
    nameMatch = any (insensitiveInfixOf searchTerm) . groupNames
  return .
    List.catMaybes .
    List.mapL (makeResultsList holeInfo) .
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
      [ mkGroup ["Set", "Type"] $ Data.ExpressionLeaf Data.Set
      , mkGroup ["Integer", "ℤ", "Z"] $ Data.ExpressionLeaf Data.IntegerType
      , mkGroup ["->", "Pi", "→", "→", "Π", "π"] $ Data.makePi (Guid.augment "NewPi" (hiGuid holeInfo)) holeExpr holeExpr
      , mkGroup ["\\", "Lambda", "Λ", "λ"] $ Data.makeLambda (Guid.augment "NewLambda" (hiGuid holeInfo)) holeExpr holeExpr
      ]
    holeExpr = Data.pureExpression $ Data.ExpressionLeaf Data.Hole

addNewDefinitionEventMap ::
  ViewM ~ m => HoleInfo m -> Widget.EventHandlers (Transaction m)
addNewDefinitionEventMap holeInfo =
  E.keyPresses Config.newDefinitionKeys
  (E.Doc ["Edit", "Result", "As new Definition"]) . makeNewDefinition $
  pickExpr holeInfo
  where
    makeNewDefinition holePickResult = do
      newDefI <- DataOps.makeDefinition -- TODO: From Sugar
      let
        searchTerm = Property.value $ hiSearchTerm holeInfo
        newName = concat . words $ searchTerm
      Anchors.setP (Anchors.assocNameRef (IRef.guid newDefI)) newName
      DataOps.newPane newDefI
      defRef <-
        fmap (fromMaybe (error "GetDef should always type-check") . listToMaybe) .
        ExprGuiM.unmemo . Sugar.holeInferResults (hiHole holeInfo) .
        Data.pureExpression . Data.ExpressionLeaf . Data.GetVariable $
        Data.DefinitionRef newDefI
      -- TODO: Can we use pickResult's animIdMapping?
      eventResult <- holePickResult defRef
      maybe (return ()) DataOps.savePreJumpPosition $
        Widget._eCursor eventResult
      return Widget.EventResult {
        Widget._eCursor = Just $ WidgetIds.fromIRef newDefI,
        Widget._eAnimIdMapping =
          holeResultAnimMappingNoParens holeInfo searchTermId
        }
    searchTermId = WidgetIds.searchTermId $ hiHoleId holeInfo

disallowedHoleChars :: String
disallowedHoleChars = "`[]\\\n "

makeSearchTermWidget
  :: MonadA m
  => HoleInfo m -> Widget.Id
  -> Maybe (Sugar.HoleResult (Tag m))
  -> ExprGuiM m (ExpressionGui m)
makeSearchTermWidget holeInfo searchTermId mResultToPick =
  ExprGuiM.widgetEnv .
  fmap
  (flip ExpressionGui (0.5/Config.holeSearchTermScaleFactor) .
   Widget.scale Config.holeSearchTermScaleFactor .
   Widget.strongerEvents pickResultEventMap .
   (Lens.over Widget.wEventMap . E.filterChars) (`notElem` disallowedHoleChars)) $
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
  :: MonadA m
  => HoleInfo m
  -> [ResultsList (Tag m)] -> Bool
  -> ExprGuiM m (Maybe (Sugar.HoleResult (Tag m)), WidgetT m)
makeResultsWidget holeInfo firstResults moreResults = do
  firstResultsAndWidgets <-
    traverse (resultsToWidgets holeInfo) firstResults
  (mResult, firstResultsWidget) <-
    case firstResultsAndWidgets of
    [] -> fmap ((,) Nothing) . makeNoResults $ Widget.toAnimId myId
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
    ExprGuiM.widgetEnv $
    if moreResults
    then fmap (: []) . BWidgets.makeLabel "..." $ Widget.toAnimId myId
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
      (E.Doc ["Navigation", "Move", "down (blocked)"]) (return ())

adHocTextEditEventMap :: MonadA m => Property m String -> Widget.EventHandlers m
adHocTextEditEventMap textProp =
  mconcat . concat $
  [ [ E.filterChars (`notElem` disallowedHoleChars) .
      E.simpleChars "Character" (E.Doc ["Edit", "Search Term", "Append character"]) $
      changeText . flip (++) . (: [])
    ]
  , [ E.keyPresses (map (E.ModKey E.noMods) [E.KeyBackspace, E.KeyDel]) (E.Doc ["Edit", "Search Term", "Delete backwards"]) $
      changeText init
    | (not . null . Property.value) textProp
    ]
  ]
  where
    changeText f = do
      Property.pureModify textProp f
      return Widget.emptyEventResult

collectResults ::
  (Applicative (List.ItemM l), List l) =>
  l (ResultType, a) -> List.ItemM l ([a], Bool)
collectResults =
  conclude <=<
  List.splitWhenM (return . (>= Config.holeResultCount) . length . fst) .
  List.scanl step ([], [])
  where
    conclude (notEnoughResults, enoughResultsM) =
      fmap
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

makeBackground :: Widget.Id -> Int -> Draw.Color -> Widget f -> Widget f
makeBackground myId level =
  Widget.backgroundColor level $
  mappend (Widget.toAnimId myId) ["hole background"]

operatorHandler ::
  Functor f => E.Doc -> (Char -> f Widget.Id) -> Widget.EventHandlers f
operatorHandler doc handler =
  (fmap . fmap) Widget.eventResultFromCursor .
  E.charGroup "Operator" doc
  Config.operatorChars . flip $ const handler

alphaNumericHandler ::
  Functor f => E.Doc -> (Char -> f Widget.Id) -> Widget.EventHandlers f
alphaNumericHandler doc handler =
  (fmap . fmap) Widget.eventResultFromCursor .
  E.charGroup "Letter/digit" doc
  Config.alphaNumericChars . flip $ const handler

pickEventMap ::
  MonadA m => HoleInfo m -> String -> Maybe (Sugar.HoleResult (Tag m)) ->
  Widget.EventHandlers (Transaction m)
pickEventMap holeInfo searchTerm (Just result)
  | nonEmptyAll (`notElem` Config.operatorChars) searchTerm =
    operatorHandler (E.Doc ["Edit", "Result", "Pick and apply operator"]) $ \x -> do
      (_, actions) <- Sugar.holePickResult (hiHoleActions holeInfo) result
      fmap (searchTermWidgetId . WidgetIds.fromGuid) $
        Sugar.giveAsArgToOperator actions [x]
  | nonEmptyAll (`elem` Config.operatorChars) searchTerm =
    alphaNumericHandler (E.Doc ["Edit", "Result", "Pick and resume"]) $ \x -> do
      (g, _) <- Sugar.holePickResult (hiHoleActions holeInfo) result
      let
        mTarget
          | g /= hiGuid holeInfo = Just g
          | otherwise = fmap (Lens.view Sugar.rGuid) $ hiMNextHole holeInfo
      fmap WidgetIds.fromGuid $ case mTarget of
        Just target -> do
          (`Property.set` [x]) =<< Anchors.assocSearchTermRef target
          return target
        Nothing -> return g
pickEventMap _ _ _ = mempty

markTypeMatchesAsUsed :: MonadA m => HoleInfo m -> ExprGuiM m ()
markTypeMatchesAsUsed holeInfo =
  ExprGuiM.markVariablesAsUsed =<<
  (filterM
   (checkInfer . Data.pureExpression . Data.makeParameterRef) .
   Sugar.holeScope . hiHole)
    holeInfo
  where
    checkInfer =
      fmap (isJust . listToMaybe) . ExprGuiM.fmapemoT .
      Sugar.holeInferResults (hiHole holeInfo)

makeActiveHoleEdit :: ViewM ~ m => HoleInfo m -> ExprGuiM m (ExpressionGui m)
makeActiveHoleEdit holeInfo = do
  markTypeMatchesAsUsed holeInfo
  allResults <- makeAllResults holeInfo

  (firstResults, hasMoreResults) <- ExprGuiM.fmapemoT $ collectResults allResults

  cursor <- ExprGuiM.widgetEnv WE.readCursor
  let
    sub = isJust . flip Widget.subId cursor
    shouldBeOnResult = sub $ resultsPrefixId holeInfo
    isOnResult = any sub $ [rlFirstId, rlMoreResultsPrefixId] <*> firstResults
    assignSource
      | shouldBeOnResult && not isOnResult = cursor
      | otherwise = hiHoleId holeInfo
    destId = head (map rlFirstId firstResults ++ [searchTermId])
  ExprGuiM.assignCursor assignSource destId $ do
    (mSelectedResult, resultsWidget) <-
      makeResultsWidget holeInfo firstResults hasMoreResults
    let
      mResult =
        mplus mSelectedResult .
        fmap rlFirst $ listToMaybe firstResults
      adHocEditor = adHocTextEditEventMap $ hiSearchTerm holeInfo
      eventMap = mconcat
        [ addNewDefinitionEventMap holeInfo
        , pickEventMap holeInfo searchTerm mResult
        ]
    searchTermWidget <- makeSearchTermWidget holeInfo searchTermId mResult
    return .
      Lens.over ExpressionGui.egWidget
      (Widget.strongerEvents eventMap .
       makeBackground (hiHoleId holeInfo)
       Layers.activeHoleBG Config.holeBackgroundColor) $
      ExpressionGui.addBelow
      [ (0.5, Widget.strongerEvents adHocEditor resultsWidget)
      ]
      searchTermWidget
  where
    searchTermId = WidgetIds.searchTermId $ hiHoleId holeInfo
    searchTerm = Property.value $ hiSearchTerm holeInfo

holeFDConfig :: FocusDelegator.Config
holeFDConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKey = E.ModKey E.noMods E.KeyEnter
  , FocusDelegator.startDelegatingDoc = E.Doc ["Navigation", "Hole", "Enter"]
  , FocusDelegator.stopDelegatingKey = E.ModKey E.noMods E.KeyEsc
  , FocusDelegator.stopDelegatingDoc = E.Doc ["Navigation", "Hole", "Leave"]
  }

make ::
  m ~ ViewM => Sugar.Hole m -> Maybe (Sugar.Expression m) -> Guid ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
make hole mNextHole guid =
  ExpressionGui.wrapDelegated holeFDConfig FocusDelegator.Delegating $
  makeUnwrapped hole mNextHole guid

makeInactiveHoleEdit ::
  MonadA m => Sugar.Hole m -> Widget.Id -> ExprGuiM m (ExpressionGui m)
makeInactiveHoleEdit hole myId =
  fmap
  (ExpressionGui.fromValueWidget .
   makeBackground myId Layers.inactiveHole unfocusedColor) .
  ExprGuiM.widgetEnv $ BWidgets.makeFocusableTextView "  " myId
  where
    unfocusedColor
      | canPickResult = Config.holeBackgroundColor
      | otherwise = Config.unfocusedReadOnlyHoleBackgroundColor
    canPickResult = isJust $ Sugar.holeMActions hole

makeUnwrapped ::
  m ~ ViewM => Sugar.Hole m -> Maybe (Sugar.Expression m) -> Guid ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
makeUnwrapped hole mNextHole guid myId = do
  cursor <- ExprGuiM.widgetEnv WE.readCursor
  searchTermProp <- ExprGuiM.transaction $ Anchors.assocSearchTermRef guid
  case (Sugar.holeMActions hole, Widget.subId myId cursor) of
    (Just holeActions, Just _) ->
      let
        holeInfo = HoleInfo
          { hiHoleId = myId
          , hiSearchTerm = searchTermProp
          , hiHole = hole
          , hiHoleActions = holeActions
          , hiGuid = guid
          , hiMNextHole = mNextHole
          }
      in makeActiveHoleEdit holeInfo
    _ -> makeInactiveHoleEdit hole myId

searchTermWidgetId :: Widget.Id -> Widget.Id
searchTermWidgetId = WidgetIds.searchTermId . FocusDelegator.delegatingId
