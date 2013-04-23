{-# LANGUAGE OverloadedStrings, TypeFamilies, FlexibleContexts, TemplateHaskell #-}
module Lamdu.CodeEdit.ExpressionEdit.HoleEdit
  ( make, makeUnwrapped
  , searchTermWidgetId
  , HoleState(..), hsSearchTerm, hsArgument
  , setHoleStateAndJump
  ) where

import Control.Applicative (Applicative(..), (<$>), (<$))
import Control.Lens ((^.), (&), (%~), (.~), (^?))
import Control.Monad ((<=<), filterM, mplus, msum, void, guard, join)
import Control.Monad.ListT (ListT)
import Control.Monad.Trans.State (StateT)
import Control.MonadA (MonadA)
import Data.Binary (Binary(..))
import Data.Cache (Cache)
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Function (on)
import Data.List.Class (List)
import Data.List.Utils (sortOn, nonEmptyAll)
import Data.Maybe (isJust, listToMaybe, maybeToList, mapMaybe, fromMaybe, catMaybes)
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
import Lamdu.Data.Expression (Expression(..))
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
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.BottleWidgets as BWidgets
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.ExpressionEdit.HoleCommon as HoleCommon
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Expression as Expression
import qualified Lamdu.Data.Expression.IRef as DataIRef
import qualified Lamdu.Data.Expression.Infer as Infer
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Layers as Layers
import qualified Lamdu.WidgetEnvT as WE
import qualified Lamdu.WidgetIds as WidgetIds

data HoleState m = HoleState
  { _hsSearchTerm :: String
  , _hsArgument :: Maybe (DataIRef.ExpressionM m (Maybe (Sugar.StorePoint (Tag m))))
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
  , hiState :: Property (T m) (HoleState m)
  , hiHoleActions :: Sugar.HoleActions m
  , hiMNextHole :: Maybe (Sugar.Expression m)
  }

pickResultAndCleanUp ::
  MonadA m =>
  HoleInfo m -> Sugar.HoleResult m -> T m Guid
pickResultAndCleanUp holeInfo holeResult = do
  Property.set (hiState holeInfo) emptyState
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
  , rlMore :: [Sugar.HoleResult m]
  }

resultsToWidgets
  :: MonadA m
  => HoleInfo m -> ResultsList m
  -> ExprGuiM m
     ( WidgetT m
     , Maybe (Sugar.HoleResult m, Maybe (WidgetT m))
     )
resultsToWidgets holeInfo results = do
  cursorOnMain <- ExprGuiM.widgetEnv $ WE.isSubCursor myId
  extra <-
    if cursorOnMain
    then fmap (Just . (,) (rlFirst results) . fmap fst) makeExtra
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
    (if null (rlMore results) then return else addMoreSymbol) =<<
    toWidget myId (rlFirst results)
  where
    makeExtra
      | null (rlMore results) = return Nothing
      | otherwise = Just <$> makeMoreResults (rlMore results)
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
      -- TODO: No need for this if we just add a pick result event map
      -- to the whole hole
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
resultsPrefixId = HoleCommon.resultsPrefixId . hiHoleId

widgetIdHash :: Show a => a -> Widget.Id
widgetIdHash = WidgetIds.fromGuid . randFunc . show

resultComplexityScore :: DataIRef.ExpressionM m (Infer.Inferred (DefI (Tag m))) -> Int
resultComplexityScore =
  sum . map (subtract 2 . length . Foldable.toList . Infer.iType) .
  Foldable.toList

toMResultsList ::
  MonadA m =>
  HoleInfo m -> Widget.Id ->
  [DataIRef.ExpressionM m (Maybe (Sugar.StorePoint (Tag m)))] ->
  CT m (Maybe (ResultsList m))
toMResultsList holeInfo baseId options = do
  results <-
    sortOn (resultComplexityScore . Lens.view Sugar.holeResultInferred) .
    catMaybes <$>
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
      , rlMore = xs
      }

baseExprToResultsList ::
  MonadA m => HoleInfo m -> DataIRef.ExpressionM m () ->
  CT m (Maybe (ResultsList m))
baseExprToResultsList holeInfo baseExpr =
  fmap join . traverse conclude =<<
  (hiHoleActions holeInfo ^. Sugar.holeInferExprType) baseExpr
  where
    conclude baseExprType =
      toMResultsList holeInfo baseId . map (Nothing <$) $
      ExprUtil.applyForms baseExprType baseExpr
    baseId = widgetIdHash baseExpr

applyOperatorResultsList ::
  MonadA m =>
  DataIRef.ExpressionM m (Maybe (Sugar.StorePoint (Tag m))) ->
  HoleInfo m -> DataIRef.ExpressionM m () ->
  CT m (Maybe (ResultsList m))
applyOperatorResultsList argument holeInfo baseExpr =
  toMResultsList holeInfo baseId =<<
  case (Nothing <$ baseExpr) ^. Expression.eBody of
  Expression.BodyLam (Expression.Lambda k paramGuid paramType result) ->
    pure $ map genExpr
    [ Expression.BodyLam (Expression.Lambda k paramGuid unwrappedArg result)
    , Expression.BodyLam (Expression.Lambda k paramGuid paramType unwrappedArg)
    ]
  _ -> do
    mBaseExprType <- hiHoleActions holeInfo ^. Sugar.holeInferExprType $ baseExpr
    pure $
      case mBaseExprType of
      Nothing -> []
      Just baseExprType ->
        let
          base = Nothing <$ ExprUtil.applyDependentPis baseExprType baseExpr
          applyBase = genExpr . ExprUtil.makeApply base
          fullyApplied x = genExpr . ExprUtil.makeApply (applyBase x)
        in
          [ fullyApplied unwrappedArg hole
          , fullyApplied hole unwrappedArg
          , applyBase unwrappedArg
          ]
  where
    genExpr = (`Expression` Nothing)
    hole = genExpr $ Expression.BodyLeaf Expression.Hole
    unwrappedArg = fromMaybe argument $ removeHoleWrap argument
    baseId = widgetIdHash baseExpr

removeHoleWrap :: Expression def a -> Maybe (Expression def a)
removeHoleWrap expr = do
  apply <- expr ^? Expression.eBody . Expression._BodyApply
  void $
    apply ^?
    Expression.applyFunc . Expression.eBody .
    Expression._BodyLeaf . Expression._Hole
  pure $ apply ^. Expression.applyArg

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
    getVarResults = paramResults ++ globalResults
    relevantResults = primitiveResults ++ literalResults ++ getVarResults
  return .
    List.catMaybes .
    List.mapL (makeResultsList holeInfo) .
    List.fromList $
    HoleCommon.holeMatches groupNames searchTerm relevantResults
  where
    primitiveResults =
      [ mkGroup ["Set", "Type"] $ Expression.BodyLeaf Expression.Set
      , mkGroup ["Integer", "ℤ", "Z"] $ Expression.BodyLeaf Expression.IntegerType
      , mkGroup ["->", "Pi", "→", "→", "Π", "π"] $
        ExprUtil.makePi (Guid.fromString "NewPi") holeExpr holeExpr
      , mkGroup ["\\", "Lambda", "Λ", "λ"] $
        ExprUtil.makeLambda (Guid.fromString "NewLambda") holeExpr holeExpr
      , mkGroup ["Record Type", "{"] . Expression.BodyRecord $
        Expression.Record Expression.Type mempty
      , mkGroup ["Record Value", "{"] . Expression.BodyRecord $
        Expression.Record Expression.Val mempty
      , mkGroup [".", "Get Field"] . Expression.BodyGetField $
        Expression.GetField Expression.FieldTagHole ExprUtil.pureHole
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
      ExprGuiM.unmemo . (hiHoleActions holeInfo ^. Sugar.holeResult) $
      Nothing <$
      ExprUtil.pureExpression (Lens.review ExprUtil.bodyDefinitionRef newDefI)
    targetGuid <- pickResultAndCleanUp holeInfo defRef
    DataOps.savePreJumpPosition cp $ WidgetIds.fromGuid targetGuid
    return Widget.EventResult {
      Widget._eCursor = Just $ WidgetIds.fromIRef newDefI,
      Widget._eAnimIdMapping =
        holeResultAnimMappingNoParens holeInfo searchTermId
      }
  where
    searchTermId = WidgetIds.searchTermId $ hiHoleId holeInfo

searchTermProperty :: HoleInfo m -> Property (T m) String
searchTermProperty holeInfo =
  Property.composeLens hsSearchTerm $ hiState holeInfo

vboxMBiasedAlign ::
  Maybe Box.Cursor -> Box.Alignment -> [Widget f] -> Widget f
vboxMBiasedAlign mChildIndex align =
  maybe Box.toWidget Box.toWidgetBiased mChildIndex .
  Box.makeAlign align Box.vertical

data HaveMoreResults = HaveMoreResults | NoMoreResults

makeResultsWidget ::
  MonadA m => HoleInfo m ->
  [ResultsList m] -> HaveMoreResults ->
  ExprGuiM m (Maybe (Sugar.HoleResult m), WidgetT m)
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
    case moreResults of
      HaveMoreResults ->
        ExprGuiM.widgetEnv . fmap (: []) .
        BWidgets.makeLabel "..." $ Widget.toAnimId myId
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

collectResults ::
  (Applicative (List.ItemM l), List l) =>
  l (ResultType, a) -> List.ItemM l ([a], HaveMoreResults)
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
        %~ (x :)

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

opPickEventMap ::
  MonadA m =>
  HoleInfo m -> Sugar.HoleResult m ->
  Widget.EventHandlers (T m)
opPickEventMap holeInfo result
  | nonEmptyAll (`notElem` Config.operatorChars) searchTerm =
    operatorHandler (E.Doc ["Edit", "Result", "Apply operator"]) $ \x ->
    Widget.emptyEventResult <$
    Property.set (hiState holeInfo)
    ( charToHoleState x
      & hsArgument .~ (Just . (Nothing <$)) (result ^. Sugar.holeResultInferred)
    )
  | nonEmptyAll (`elem` Config.operatorChars) searchTerm =
    alphaNumericHandler (E.Doc ["Edit", "Result", "Pick and resume"]) $ \x -> do
      g <- pickResultAndCleanUp holeInfo result
      let
        mTarget
          | Sugar.holeResultHasHoles result = Just g
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
      (hiHoleActions holeInfo ^. Sugar.holeResult) .
      (Nothing <$)

mkEventMap ::
  MonadA m => HoleInfo m -> Maybe (Sugar.HoleResult m) ->
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
    , maybe mempty (opPickEventMap holeInfo) mResult
    , maybe mempty
      ( E.keyPresses Config.delKeys
        (E.Doc ["Edit", "Back"])
      . fmap (Widget.eventResultFromCursor . WidgetIds.fromGuid)
      . pickResultAndCleanUp holeInfo
      ) mDeleteOpResult
    , maybe mempty
      ( E.keyPresses Config.delKeys
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
  (firstResults, hasMoreResults) <-
    ExprGuiM.liftMemoT . collectResults =<< makeAllResults holeInfo
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
        mplus mSelectedResult . (Lens.mapped %~ rlFirst) $
        listToMaybe firstResults
      searchTermEventMap = maybe mempty (resultPickEventMap holeInfo) mResult
    searchTermWidget <-
      HoleCommon.makeSearchTermWidget (searchTermProperty holeInfo) searchTermId
      -- TODO: Move the result picking events into pickEventMap
      -- instead of here on the searchTerm and on each result
      & Lens.mapped . ExpressionGui.egWidget %~ Widget.strongerEvents searchTermEventMap
    holeEventMap <- mkEventMap holeInfo mResult
    maybe (return ()) (ExprGuiM.addResultPicker . (^. Sugar.holeResultPickPrefix))
      mResult
    let adHocEditor = HoleCommon.adHocTextEditEventMap $ searchTermProperty holeInfo
    return .
      Lens.over ExpressionGui.egWidget
      (Widget.strongerEvents holeEventMap .
       HoleCommon.makeBackground (hiHoleId holeInfo)
       Layers.activeHoleBG Config.holeBackgroundColor) $
      ExpressionGui.addBelow
      [ (0.5, Widget.strongerEvents adHocEditor resultsWidget)
      ]
      searchTermWidget
  where
    searchTermId = WidgetIds.searchTermId $ hiHoleId holeInfo

make ::
  MonadA m => Sugar.Hole m -> Maybe (Sugar.Expression m) -> Guid ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
make hole mNextHole guid =
  ExpressionGui.wrapDelegated HoleCommon.holeFDConfig FocusDelegator.Delegating $
  makeUnwrapped hole mNextHole guid

makeUnwrapped ::
  MonadA m => Sugar.Hole m ->
  Maybe (Sugar.Expression m) -> Guid ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
makeUnwrapped hole mNextHole guid myId = do
  cursor <- ExprGuiM.widgetEnv WE.readCursor
  case (hole ^. Sugar.holeMActions, Widget.subId myId cursor) of
    (Just holeActions, Just _) -> do
      stateProp <- ExprGuiM.transaction $ assocStateRef guid ^. Transaction.mkProperty
      makeActiveHoleEdit HoleInfo
        { hiHoleId = myId
        , hiState = stateProp
        , hiHoleActions = holeActions
        , hiMNextHole = mNextHole
        }
    (x, _) -> HoleCommon.makeInactive (isJust x) myId

searchTermWidgetId :: Widget.Id -> Widget.Id
searchTermWidgetId = WidgetIds.searchTermId . FocusDelegator.delegatingId

setHoleStateAndJump :: MonadA m => HoleState m -> Guid -> T m Widget.Id
setHoleStateAndJump newHoleState newHoleGuid = do
  Transaction.setP (assocStateRef newHoleGuid) newHoleState
  pure . searchTermWidgetId $ WidgetIds.fromGuid newHoleGuid

emptyState :: HoleState m
emptyState =
  HoleState
  { _hsSearchTerm = ""
  , _hsArgument = Nothing
  }

assocStateRef :: MonadA m => Guid -> MkProperty m (HoleState m)
assocStateRef = Transaction.assocDataRefDef emptyState "searchTerm"
