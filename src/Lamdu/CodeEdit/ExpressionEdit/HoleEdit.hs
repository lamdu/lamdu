{-# LANGUAGE OverloadedStrings, TypeFamilies, FlexibleContexts, TemplateHaskell #-}
module Lamdu.CodeEdit.ExpressionEdit.HoleEdit
  ( make, makeUnwrapped
  , searchTermWidgetId
  , HoleState(..), hsSearchTerm, hsArgument
  , setHoleStateAndJump
  ) where

import Control.Applicative (Applicative(..), (<$>), (<$))
import Control.Lens ((^.), (&), (%~), (.~))
import Control.Monad ((<=<), filterM, mplus, msum, void, guard, join)
import Control.Monad.ListT (ListT)
import Control.Monad.Trans.State (StateT)
import Control.MonadA (MonadA)
import Data.Binary (Binary(..))
import Data.Cache (Cache)
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Function (on)
import Data.List (isInfixOf, isPrefixOf)
import Data.List.Class (List)
import Data.List.Utils (sortOn, nonEmptyAll)
import Data.Maybe (isJust, listToMaybe, maybeToList, mapMaybe, fromMaybe)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.IRef (Tag)
import Data.Store.Property (Property(..))
import Data.Store.Transaction (Transaction, MkProperty)
import Data.Traversable (traverse, sequenceA)
import Graphics.UI.Bottle.Animation(AnimId)
import Graphics.UI.Bottle.Widget (Widget)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui(..))
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM, WidgetT)
import Lamdu.Data.Expression (Expression)
import Lamdu.Data.Expression.IRef (DefI)
import System.Random.Utils (randFunc)
import qualified Control.Lens as Lens
import qualified Control.Lens.TH as LensTH
import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
import qualified Data.List.Class as List
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.BottleWidgets as BWidgets
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Expression as Expression
import qualified Lamdu.Data.Expression.Infer as Infer
import qualified Lamdu.Data.Expression.IRef as DataIRef
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Layers as Layers
import qualified Lamdu.WidgetEnvT as WE
import qualified Lamdu.WidgetIds as WidgetIds

data HoleState t = HoleState
  { _hsSearchTerm :: String
  , _hsArgument :: Maybe (DataIRef.Expression t ())
  } deriving Eq
LensTH.makeLenses ''HoleState
derive makeBinary ''HoleState

moreSymbol :: String
moreSymbol = "▷"

moreSymbolSizeFactor :: Fractional a => a
moreSymbolSizeFactor = 0.5

data Group def = Group
  { groupNames :: [String]
  , groupBaseExpr :: Expression def ()
  }

type T = Transaction
type CT m = StateT Cache (T m)

data HoleInfo m = HoleInfo
  { hiHoleId :: Widget.Id
  , hiState :: Property (T m) (HoleState (Tag m))
  , hiHoleActions :: Sugar.HoleActions m
  , hiGuid :: Guid
  , hiMNextHole :: Maybe (Sugar.Expression m)
  }

pickResultAndCleanUp ::
  MonadA m =>
  HoleInfo m -> Sugar.HoleResult m -> T m Guid
pickResultAndCleanUp holeInfo holeResult = do
  Transaction.setP (assocStateRef (hiGuid holeInfo)) emptyState
  holeResult ^. Sugar.holeResultPick

resultPickEventMap ::
  MonadA m => HoleInfo m -> Sugar.HoleResult m ->
  Widget.EventHandlers (T m)
resultPickEventMap holeInfo holeResult =
  case hiMNextHole holeInfo of
  Just nextHole
    | not (Sugar.holeResultHasHoles holeResult) ->
      mappend (simplePickRes Config.pickResultKeys) .
      E.keyPresses Config.pickAndMoveToNextHoleKeys
      (E.Doc ["Edit", "Result", "Pick and move to next hole"]) $
        (Widget.eventResultFromCursor . WidgetIds.fromGuid)
        (nextHole ^. Sugar.rGuid) <$
        pickResultAndCleanUp holeInfo holeResult
  _ -> simplePickRes $ Config.pickResultKeys ++ Config.pickAndMoveToNextHoleKeys
  where
    simplePickRes keys =
      E.keyPresses keys (E.Doc ["Edit", "Result", "Pick"]) .
      fmap (Widget.eventResultFromCursor . WidgetIds.fromGuid) $
      pickResultAndCleanUp holeInfo holeResult

data ResultsList m = ResultsList
  { rlFirstId :: Widget.Id
  , rlMoreResultsPrefixId :: Widget.Id
  , rlFirst :: Sugar.HoleResult m
  , rlMore :: Maybe [Sugar.HoleResult m]
  }

resultsToWidgets
  :: MonadA m
  => HoleInfo m -> (ResultType, ResultsList m)
  -> ExprGuiM m
     ( WidgetT m
     , Maybe ((ResultType, Sugar.HoleResult m), Maybe (WidgetT m))
     )
resultsToWidgets holeInfo (resultType, results) = do
  cursorOnMain <- ExprGuiM.widgetEnv $ WE.isSubCursor myId
  extra <-
    if cursorOnMain
    then fmap (Just . (,) (resultType, rlFirst results) . fmap fst) makeExtra
    else do
      cursorOnExtra <- ExprGuiM.widgetEnv $ WE.isSubCursor moreResultsPrefixId
      if cursorOnExtra
        then do
          extra <- makeExtra
          return $ do
            (widget, mResult) <- extra
            result <- mResult
            return ((resultType, result), Just widget)
        else return Nothing
  fmap (flip (,) extra) .
    maybe return (const addMoreSymbol) (rlMore results) =<<
    toWidget myId (rlFirst results)
  where
    makeExtra = traverse makeMoreResults $ rlMore results
    makeMoreResults moreResults = do
      pairs <- traverse moreResult moreResults
      return
        ( Box.vboxAlign 0 $ map fst pairs
        , msum $ map snd pairs
        )
    moreResult holeResult = do
      mResult <-
        (fmap . fmap . const) holeResult . ExprGuiM.widgetEnv $ WE.subCursor resultId
      widget <- toWidget resultId holeResult
      return (widget, mResult)
      where
        resultId =
          mappend moreResultsPrefixId . widgetIdHash . void $
          holeResult ^. Sugar.holeResultInferred
    toWidget resultId holeResult =
      ExprGuiM.widgetEnv . BWidgets.makeFocusableView resultId .
      Widget.strongerEvents (resultPickEventMap holeInfo holeResult) .
      Lens.view ExpressionGui.egWidget =<<
      ExprGuiM.makeSubexpresion . Sugar.removeTypes =<<
      ExprGuiM.transaction (holeResult ^. Sugar.holeResultConvert)
    moreResultsPrefixId = rlMoreResultsPrefixId results
    addMoreSymbol w = do
      moreSymbolLabel <-
        fmap (Widget.scale moreSymbolSizeFactor) .
        ExprGuiM.widgetEnv .
        BWidgets.makeLabel moreSymbol $ Widget.toAnimId myId
      return $ BWidgets.hboxCenteredSpaced [w, moreSymbolLabel]
    myId = rlFirstId results

makeNoResults :: MonadA m => AnimId -> ExprGuiM m (WidgetT m)
makeNoResults myId =
  ExprGuiM.widgetEnv .
  BWidgets.makeTextView "(No results)" $ mappend myId ["no results"]

mkGroup :: [String] -> Expression.BodyExpr def () -> Group def
mkGroup names body = Group
  { groupNames = names
  , groupBaseExpr = ExprUtil.pureExpression body
  }

makeVariableGroup ::
  MonadA m => Expression.VariableRef (DefI (Tag m)) ->
  ExprGuiM m (Group (DefI (Tag m)))
makeVariableGroup varRef =
  ExprGuiM.withNameFromVarRef varRef $ \(_, varName) ->
  return . mkGroup [varName] . Expression.BodyLeaf $ Expression.GetVariable varRef

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
      , groupBaseExpr = ExprUtil.pureExpression $ Lens.review ExprUtil.bodyLiteralInteger integer
      }

resultsPrefixId :: HoleInfo m -> Widget.Id
resultsPrefixId holeInfo = mconcat [hiHoleId holeInfo, Widget.Id ["results"]]

widgetIdHash :: Show a => a -> Widget.Id
widgetIdHash = WidgetIds.fromGuid . randFunc . show

resultComplexityScore :: DataIRef.ExpressionM m (Infer.Inferred (DefI (Tag m))) -> Int
resultComplexityScore =
  sum . map (subtract 2 . length . Foldable.toList . Infer.iType) .
  Foldable.toList

toMResultsList ::
  MonadA m =>
  HoleInfo m -> Widget.Id -> [DataIRef.ExpressionM m ()] ->
  StateT Cache (T m) (Maybe (ResultsList m))
toMResultsList holeInfo baseId options = do
  results <-
    sortOn (resultComplexityScore . Lens.view Sugar.holeResultInferred) .
    concat . map maybeToList <$>
    traverse (hiHoleActions holeInfo ^. Sugar.holeResult) options
  return $
    case results of
    [] -> Nothing
    x : xs ->
      Just ResultsList
      { rlFirstId = mconcat [resultsPrefixId holeInfo, baseId]
      , rlMoreResultsPrefixId =
        mconcat [resultsPrefixId holeInfo, Widget.Id ["more results"], baseId]
      , rlFirst = x
      , rlMore =
        case xs of
        [] -> Nothing
        _ -> Just xs
      }

baseExprToResultsList ::
  MonadA m => HoleInfo m -> DataIRef.ExpressionM m () ->
  CT m (Maybe (ResultsList m))
baseExprToResultsList holeInfo baseExpr = do
  mBaseExprType <- hiHoleActions holeInfo ^. Sugar.holeInferExprType $ baseExpr
  case mBaseExprType of
    Nothing -> pure Nothing
    Just baseExprType ->
      toMResultsList holeInfo baseId $ ExprUtil.applyForms baseExprType baseExpr
  where
    baseId = widgetIdHash baseExpr

applyOperatorResultsList ::
  MonadA m =>
  DataIRef.ExpressionM m () ->
  HoleInfo m -> DataIRef.ExpressionM m () ->
  CT m (Maybe (ResultsList m))
applyOperatorResultsList argument holeInfo baseExpr = do
  mBaseExprType <- hiHoleActions holeInfo ^. Sugar.holeInferExprType $ baseExpr
  case mBaseExprType of
    Nothing -> pure Nothing
    Just baseExprType ->
      let
        base = ExprUtil.applyDependentPis baseExprType baseExpr
        applyBase =
          ExprUtil.pureExpression . ExprUtil.makeApply base
        fullyApplied x =
          ExprUtil.pureExpression . ExprUtil.makeApply (applyBase x)
      in
        toMResultsList holeInfo baseId
        [ fullyApplied argument ExprUtil.pureHole
        , fullyApplied ExprUtil.pureHole argument
        , applyBase argument
        ]
  where
    baseId = widgetIdHash baseExpr

data ResultType = GoodResult | BadResult

makeResultsList ::
  MonadA m => HoleInfo m -> Group (DefI (Tag m)) ->
  CT m (Maybe (ResultType, ResultsList m))
makeResultsList holeInfo group =
  case Property.value (hiState holeInfo) ^. hsArgument of
  Just arg ->
    fmap ((,) GoodResult) <$> applyOperatorResultsList arg holeInfo baseExpr
  Nothing -> do
    -- We always want the first, and we want to know if there's more, so
    -- take 2:
    mRes <- baseExprToResultsList holeInfo baseExpr
    case mRes of
      Just res -> return . Just $ (GoodResult, res)
      Nothing ->
        (fmap . fmap) ((,) BadResult) .
        baseExprToResultsList holeInfo $ holeApply baseExpr
  where
    baseExpr = groupBaseExpr group
    holeApply =
      ExprUtil.pureExpression .
      (ExprUtil.makeApply . ExprUtil.pureExpression . Expression.BodyLeaf) Expression.Hole

makeAllResults :: MonadA m => HoleInfo m -> ExprGuiM m (ListT (CT m) (ResultType, ResultsList m))
makeAllResults holeInfo = do
  paramResults <-
    traverse (makeVariableGroup . Expression.ParameterRef) $
    hiHoleActions holeInfo ^. Sugar.holeScope
  globalResults <-
    traverse (makeVariableGroup . Expression.DefinitionRef) =<<
    ExprGuiM.getCodeAnchor Anchors.globals
  let
    state = Property.value $ hiState holeInfo
    searchTerm = state ^. hsSearchTerm
    literalResults = makeLiteralGroup searchTerm
    nameMatch = any (insensitiveInfixOf searchTerm) . groupNames
    getVarResults = paramResults ++ globalResults
    relevantResults =
      case state ^. hsArgument of
      Just _ -> getVarResults
      Nothing -> getVarResults ++ literalResults ++ primitiveResults
  return .
    List.catMaybes .
    List.mapL (makeResultsList holeInfo) .
    List.fromList .
    sortOn (groupOrdering searchTerm) $
    filter nameMatch relevantResults
  where
    insensitiveInfixOf = isInfixOf `on` map Char.toLower
    primitiveResults =
      [ mkGroup ["Set", "Type"] $ Expression.BodyLeaf Expression.Set
      , mkGroup ["Integer", "ℤ", "Z"] $ Expression.BodyLeaf Expression.IntegerType
      , mkGroup ["->", "Pi", "→", "→", "Π", "π"] $ ExprUtil.makePi (Guid.augment "NewPi" (hiGuid holeInfo)) holeExpr holeExpr
      , mkGroup ["\\", "Lambda", "Λ", "λ"] $ ExprUtil.makeLambda (Guid.augment "NewLambda" (hiGuid holeInfo)) holeExpr holeExpr
      ]
    holeExpr = ExprUtil.pureExpression $ Expression.BodyLeaf Expression.Hole

addNewDefinitionEventMap ::
  MonadA m => Anchors.CodeProps m -> HoleInfo m -> Widget.EventHandlers (T m)
addNewDefinitionEventMap cp holeInfo =
  E.keyPresses Config.newDefinitionKeys
  (E.Doc ["Edit", "Result", "As new Definition"]) $ do
    newDefI <- DataOps.makeDefinition cp -- TODO: From Sugar
    let
      searchTerm = Property.value (hiState holeInfo) ^. hsSearchTerm
      newName = concat . words $ searchTerm
    Transaction.setP (Anchors.assocNameRef (IRef.guid newDefI)) newName
    DataOps.newPane cp newDefI
    defRef <-
      fmap (fromMaybe (error "GetDef should always type-check")) .
      ExprGuiM.unmemo . (hiHoleActions holeInfo ^. Sugar.holeResult) .
      ExprUtil.pureExpression $ Lens.review ExprUtil.bodyDefinitionRef newDefI
    targetGuid <- pickResultAndCleanUp holeInfo defRef
    DataOps.savePreJumpPosition cp $ WidgetIds.fromGuid targetGuid
    return Widget.EventResult {
      Widget._eCursor = Just $ WidgetIds.fromIRef newDefI,
      Widget._eAnimIdMapping =
        holeResultAnimMappingNoParens holeInfo searchTermId
      }
  where
    searchTermId = WidgetIds.searchTermId $ hiHoleId holeInfo

disallowedHoleChars :: [(Char, E.IsShifted)]
disallowedHoleChars =
  E.anyShiftedChars ",`\n() " ++
  [ ('0', E.Shifted)
  , ('9', E.Shifted)
  ]

disallowChars :: E.EventMap a -> E.EventMap a
disallowChars = E.filterSChars $ curry (`notElem` disallowedHoleChars)

searchTermProperty :: HoleInfo m -> Property (T m) String
searchTermProperty holeInfo =
  Property.composeLens hsSearchTerm $ hiState holeInfo

makeSearchTermWidget
  :: MonadA m
  => HoleInfo m -> Widget.Id
  -> Maybe (Sugar.HoleResult m)
  -> ExprGuiM m (ExpressionGui m)
makeSearchTermWidget holeInfo searchTermId mResultToPick =
  ExprGuiM.widgetEnv .
  fmap
  (flip ExpressionGui (0.5/Config.holeSearchTermScaleFactor) .
   Widget.scale Config.holeSearchTermScaleFactor .
   Widget.strongerEvents pickResultEventMap .
   Lens.over Widget.wEventMap disallowChars) $
  BWidgets.makeWordEdit (searchTermProperty holeInfo) searchTermId
  where
    pickResultEventMap =
      maybe mempty (resultPickEventMap holeInfo) mResultToPick

vboxMBiasedAlign ::
  Maybe Box.Cursor -> Box.Alignment -> [Widget f] -> Widget f
vboxMBiasedAlign mChildIndex align =
  maybe Box.toWidget Box.toWidgetBiased mChildIndex .
  Box.makeAlign align Box.vertical

data HaveMoreResults = HaveMoreResults | NoMoreResults

makeResultsWidget
  :: MonadA m
  => HoleInfo m
  -> [(ResultType, ResultsList m)] -> HaveMoreResults
  -> ExprGuiM m (Maybe (ResultType, Sugar.HoleResult m), WidgetT m)
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
          zipWith (Lens.over (Lens._2 . Lens.mapped) . (,)) [0..] xs
      return
        ( mResult
        , blockDownEvents . vboxMBiasedAlign (fmap fst mResult) 0 $
          map fst xs
        )
  let extraWidgets = maybeToList $ snd . snd =<< mResult
  moreResultsWidgets <-
    ExprGuiM.widgetEnv $
    case moreResults of
      HaveMoreResults ->
        (: []) <$> BWidgets.makeLabel "..." (Widget.toAnimId myId)
      NoMoreResults -> return []
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
      E.keyPresses
      [E.ModKey E.noMods E.KeyDown]
      (E.Doc ["Navigation", "Move", "down (blocked)"]) (return Widget.emptyEventResult)

adHocTextEditEventMap :: MonadA m => Property m String -> Widget.EventHandlers m
adHocTextEditEventMap textProp =
  mconcat . concat $
  [ [ disallowChars .
      E.simpleChars "Character"
      (E.Doc ["Edit", "Search Term", "Append character"]) $
      changeText . flip (++) . (: [])
    ]
  , [ E.keyPresses (map (E.ModKey E.noMods) [E.KeyBackspace])
      (E.Doc ["Edit", "Search Term", "Delete backwards"]) $
      changeText init
    | (not . null . Property.value) textProp
    ]
  ]
  where
    changeText f = Widget.emptyEventResult <$ Property.pureModify textProp f

collectResults ::
  (Applicative (List.ItemM l), List l) =>
  l (ResultType, a) -> List.ItemM l ([(ResultType, a)], HaveMoreResults)
collectResults =
  conclude <=<
  List.splitWhenM (return . (>= Config.holeResultCount) . length . fst) .
  List.scanl step ([], [])
  where
    haveMoreResults [] = NoMoreResults
    haveMoreResults _ = HaveMoreResults
    conclude (notEnoughResults, enoughResultsM) =
      ( (Lens._2 %~ haveMoreResults) . splitAt Config.holeResultCount
      . uncurry (on (++) reverse) . last . mappend notEnoughResults
      ) <$>
      List.toList (List.take 2 enoughResultsM)
    step results (tag, x) =
      results
      & case tag of
        GoodResult -> Lens._1
        BadResult -> Lens._2
        %~ ((tag, x) :)

makeBackground :: Widget.Id -> Int -> Draw.Color -> Widget f -> Widget f
makeBackground myId level =
  Widget.backgroundColor level $
  mappend (Widget.toAnimId myId) ["hole background"]

operatorHandler :: E.Doc -> (Char -> a) -> E.EventMap a
operatorHandler doc handler =
  E.charGroup "Operator" doc
  Config.operatorChars . flip $ const handler

alphaNumericHandler ::
  Functor f => E.Doc -> (Char -> f Widget.Id) -> Widget.EventHandlers f
alphaNumericHandler doc handler =
  (fmap . fmap) Widget.eventResultFromCursor .
  E.charGroup "Letter/digit" doc
  Config.alphaNumericChars . flip $ const handler

pickEventMap ::
  MonadA m =>
  HoleInfo m -> (ResultType, Sugar.HoleResult m) ->
  Widget.EventHandlers (T m)
pickEventMap holeInfo (resultType, result)
  | nonEmptyAll (`notElem` Config.operatorChars) searchTerm =
    case resultType of
    GoodResult ->
      operatorHandler (E.Doc ["Edit", "Result", "Apply operator"]) $
      \x ->
        Widget.emptyEventResult <$
        Transaction.setP (assocStateRef (hiGuid holeInfo))
        ( charToHoleState x
          & hsArgument .~ (Just . void) (result ^. Sugar.holeResultInferred)
        )
    BadResult ->
      operatorHandler (E.Doc ["Edit", "Result", "Pick and apply operator"]) $
      \x ->
        fmap Widget.eventResultFromCursor $
        setHoleStateAndJump (charToHoleState x) =<<
        pickResultAndCleanUp holeInfo result
  | nonEmptyAll (`elem` Config.operatorChars) searchTerm =
    alphaNumericHandler (E.Doc ["Edit", "Result", "Pick and resume"]) $ \x -> do
      g <- pickResultAndCleanUp holeInfo result
      let
        mTarget
          | g /= hiGuid holeInfo = Just g
          | otherwise = (^. Sugar.rGuid) <$> hiMNextHole holeInfo
      maybe
        ((pure . WidgetIds.fromGuid) g)
        (setHoleStateAndJump (charToHoleState x))
        mTarget
  | otherwise = mempty
  where
    searchTerm = Property.value (hiState holeInfo) ^. hsSearchTerm
    charToHoleState x =
      HoleState
      { _hsSearchTerm = [x]
      , _hsArgument = Nothing
      }

markTypeMatchesAsUsed :: MonadA m => HoleInfo m -> ExprGuiM m ()
markTypeMatchesAsUsed holeInfo =
  ExprGuiM.markVariablesAsUsed =<<
  filterM
  (checkInfer . ExprUtil.pureExpression . Lens.review ExprUtil.bodyParameterRef)
  (hiHoleActions holeInfo ^. Sugar.holeScope)
  where
    checkInfer =
      fmap isJust . ExprGuiM.liftMemoT .
      (hiHoleActions holeInfo ^. Sugar.holeResult)

mkEventMap ::
  MonadA m => HoleInfo m -> Maybe (ResultType, Sugar.HoleResult m) ->
  ExprGuiM m (Widget.EventHandlers (T m))
mkEventMap holeInfo mResult = do
  cp <- ExprGuiM.readCodeAnchors
  mDeleteOpResult <-
    ExprGuiM.liftMemoT . fmap join . sequenceA $ do
      guard . null $ drop 1 searchTerm
      arg <- Property.value (hiState holeInfo) ^. hsArgument
      Just $ hiHoleActions holeInfo ^. Sugar.holeResult $ arg
  pure $ mconcat
    [ addNewDefinitionEventMap cp holeInfo
    , maybe mempty (pickEventMap holeInfo) mResult
    , maybe mempty
      ( E.keyPresses
        (Config.delForwardKeys ++ Config.delBackwordKeys)
        (E.Doc ["Edit", "Back"])
      . fmap (Widget.eventResultFromCursor . WidgetIds.fromGuid)
      . pickResultAndCleanUp holeInfo
      ) mDeleteOpResult
    , maybe mempty
      ( E.keyPresses
        (Config.delForwardKeys ++ Config.delBackwordKeys)
        (E.Doc ["Edit", "Delete"])
      . fmap (Widget.eventResultFromCursor . WidgetIds.fromGuid)
      ) $ do
        guard $ null searchTerm
        actions ^. Sugar.holeMDelete
    ]
  where
    actions = hiHoleActions holeInfo
    searchTerm = Property.value (hiState holeInfo) ^. hsSearchTerm

makeActiveHoleEdit :: MonadA m => HoleInfo m -> ExprGuiM m (ExpressionGui m)
makeActiveHoleEdit holeInfo = do
  markTypeMatchesAsUsed holeInfo
  (firstResultsTagged, hasMoreResults) <-
    ExprGuiM.liftMemoT . collectResults =<< makeAllResults holeInfo
  cursor <- ExprGuiM.widgetEnv WE.readCursor
  let
    firstResults = snd <$> firstResultsTagged
    sub = isJust . flip Widget.subId cursor
    shouldBeOnResult = sub $ resultsPrefixId holeInfo
    isOnResult = any sub $ [rlFirstId, rlMoreResultsPrefixId] <*> firstResults
    assignSource
      | shouldBeOnResult && not isOnResult = cursor
      | otherwise = hiHoleId holeInfo
    destId = head (map rlFirstId firstResults ++ [searchTermId])
  ExprGuiM.assignCursor assignSource destId $ do
    (mSelectedResult, resultsWidget) <-
      makeResultsWidget holeInfo firstResultsTagged hasMoreResults
    let
      mResult =
        mplus mSelectedResult .
        (Lens.mapped . Lens._2 %~ rlFirst) $ listToMaybe firstResultsTagged
    searchTermWidget <-
      makeSearchTermWidget holeInfo searchTermId $ snd <$> mResult
    eventMap <- mkEventMap holeInfo mResult
    maybe (return ())
      (ExprGuiM.addResultPicker . (^. Lens._2 . Sugar.holeResultPickPrefix)) mResult
    let adHocEditor = adHocTextEditEventMap $ searchTermProperty holeInfo
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

holeFDConfig :: FocusDelegator.Config
holeFDConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKey = E.ModKey E.noMods E.KeyEnter
  , FocusDelegator.startDelegatingDoc = E.Doc ["Navigation", "Hole", "Enter"]
  , FocusDelegator.stopDelegatingKey = E.ModKey E.noMods E.KeyEsc
  , FocusDelegator.stopDelegatingDoc = E.Doc ["Navigation", "Hole", "Leave"]
  }

make ::
  MonadA m => Sugar.Hole m -> Maybe (Sugar.Expression m) -> Guid ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
make hole mNextHole guid =
  ExpressionGui.wrapDelegated holeFDConfig FocusDelegator.Delegating $
  makeUnwrapped hole mNextHole guid

makeInactiveHoleEdit ::
  MonadA m =>
  Sugar.Hole m -> Widget.Id -> ExprGuiM m (ExpressionGui m)
makeInactiveHoleEdit hole myId =
  fmap
  (ExpressionGui.fromValueWidget .
   makeBackground myId Layers.inactiveHole unfocusedColor) .
  ExprGuiM.widgetEnv $ BWidgets.makeFocusableTextView "  " myId
  where
    unfocusedColor
      | canPickResult = Config.holeBackgroundColor
      | otherwise = Config.unfocusedReadOnlyHoleBackgroundColor
    canPickResult = isJust $ hole ^. Sugar.holeMActions

makeUnwrapped ::
  MonadA m => Sugar.Hole m ->
  Maybe (Sugar.Expression m) -> Guid ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
makeUnwrapped hole mNextHole guid myId = do
  cursor <- ExprGuiM.widgetEnv WE.readCursor
  stateProp <- ExprGuiM.transaction $ assocStateRef guid ^. Transaction.mkProperty
  case (hole ^. Sugar.holeMActions, Widget.subId myId cursor) of
    (Just holeActions, Just _) ->
      let
        holeInfo = HoleInfo
          { hiHoleId = myId
          , hiState = stateProp
          , hiHoleActions = holeActions
          , hiGuid = guid
          , hiMNextHole = mNextHole
          }
      in makeActiveHoleEdit holeInfo
    _ -> makeInactiveHoleEdit hole myId

searchTermWidgetId :: Widget.Id -> Widget.Id
searchTermWidgetId = WidgetIds.searchTermId . FocusDelegator.delegatingId

setHoleStateAndJump :: MonadA m => HoleState (Tag m) -> Guid -> T m Widget.Id
setHoleStateAndJump newHoleState newHoleGuid = do
  Transaction.setP (assocStateRef newHoleGuid) newHoleState
  pure . searchTermWidgetId $ WidgetIds.fromGuid newHoleGuid

emptyState :: HoleState m
emptyState =
  HoleState
  { _hsSearchTerm = ""
  , _hsArgument = Nothing
  }

assocStateRef :: MonadA m => Guid -> MkProperty m (HoleState (Tag m))
assocStateRef = Transaction.assocDataRefDef emptyState "searchTerm"
