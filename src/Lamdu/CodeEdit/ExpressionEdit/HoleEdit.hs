{-# LANGUAGE OverloadedStrings, TypeFamilies, FlexibleContexts, TemplateHaskell #-}
module Lamdu.CodeEdit.ExpressionEdit.HoleEdit
  ( make, makeUnwrapped
  , searchTermWidgetId
  , HoleState(..), hsSearchTerm, hsArgument
  , setHoleStateAndJump
  ) where

import Control.Applicative (Applicative(..), (<$>), (<$), (<|>))
import Control.Lens.Operators
import Control.Monad ((<=<), filterM, msum, void, guard, join)
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
import Data.Maybe (isJust, listToMaybe, maybeToList, fromMaybe, catMaybes)
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
import qualified Data.Cache as Cache
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

extraSymbol :: String
extraSymbol = "▷"

extraSymbolSizeFactor :: Fractional a => a
extraSymbolSizeFactor = 0.5

data Group def = Group
  { groupNames :: [String]
  , groupBaseExpr :: Expression def ()
  }

type T = Transaction
type CT m = StateT Cache (T m)

data HoleInfo m = HoleInfo
  { hiGuid :: Guid
  , hiHoleId :: Widget.Id
  , hiState :: Property (T m) (HoleState m)
  , hiHoleActions :: Sugar.HoleActions m
  , hiMNextHole :: Maybe (Sugar.Expression m)
  }

pickResultAndCleanUp ::
  MonadA m =>
  HoleInfo m -> Sugar.HoleResult m -> T m (Maybe Guid)
pickResultAndCleanUp holeInfo holeResult = do
  Property.set (hiState holeInfo) emptyState
  holeResult ^. Sugar.holeResultPick

handlePickResultTargetGuid ::
  MonadA m => HoleInfo m -> Maybe Guid -> Widget.EventResult
handlePickResultTargetGuid holeInfo =
  Widget.eventResultFromCursor . WidgetIds.fromGuid .
  fromMaybe (hiGuid holeInfo)

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
      fmap (handlePickResultTargetGuid holeInfo) $
      pickResultAndCleanUp holeInfo holeResult

data ResultsList m = ResultsList
  { rlMainId :: Widget.Id
  , rlExtraResultsPrefixId :: Widget.Id
  , rlMain :: Sugar.HoleResult m
  , rlExtra :: [Sugar.HoleResult m]
  }

resultsToWidgets
  :: MonadA m
  => HoleInfo m -> ResultsList m
  -> ExprGuiM m
     ( WidgetT m
       -- On any result?
     , Maybe
       -- Which result:
       ( Sugar.HoleResult m
         -- Extra results widget?
       , Maybe (WidgetT m)
       )
     )
resultsToWidgets holeInfo results = do
  mainResultWidget <-
    maybeAddExtraSymbol ((not . null . rlExtra) results) (rlMainId results) =<<
    makeHoleResultWidget holeInfo (rlMainId results) (rlMain results)
  mExtraResWidget <-
    if mainResultWidget ^. Widget.wIsFocused
    then do
      mWidget <- fmap snd <$> makeExtra
      return $ Just (rlMain results, mWidget)
    else do
      cursorOnExtra <-
        ExprGuiM.widgetEnv . WE.isSubCursor $ rlExtraResultsPrefixId results
      if cursorOnExtra
        then do
          mExtra <- makeExtra
          return $ do
            (mResult, widget) <- mExtra
            result <- mResult
            Just (result, Just widget)
        else return Nothing
  return (mainResultWidget, mExtraResWidget)
  where
    makeExtra =
      makeExtraResultsWidget holeInfo
      (rlExtraResultsPrefixId results) (rlExtra results)

makeExtraResultsWidget ::
  MonadA m => HoleInfo m -> Widget.Id -> [Sugar.HoleResult m] ->
  ExprGuiM m (Maybe (Maybe (Sugar.HoleResult m), WidgetT m))
makeExtraResultsWidget holeInfo extraPrefixId extraResults
  | (not . null) extraResults = Just <$> do
    (mResults, widgets) <-
      unzip <$> traverse extraResult extraResults
    return (msum mResults, Box.vboxAlign 0 widgets)
  | otherwise = return Nothing
  where
    extraResult holeResult = do
      widget <-
        makeHoleResultWidget holeInfo resultId holeResult
      mResult <-
        (fmap . fmap . const) holeResult . ExprGuiM.widgetEnv $ WE.subCursor resultId
      return (mResult, widget)
      where
        resultId =
          mappend extraPrefixId . widgetIdHash . void $
          holeResult ^. Sugar.holeResultInferred

makeHoleResultWidget ::
  MonadA m => HoleInfo m ->
  Widget.Id -> Sugar.HoleResult m -> ExprGuiM m (WidgetT m)
makeHoleResultWidget holeInfo resultId holeResult =
  ExprGuiM.widgetEnv . BWidgets.makeFocusableView resultId .
  -- TODO: No need for this if we just add a pick result event map
  -- to the whole hole
  Widget.scale Config.holeResultScaleFactor .
  Widget.strongerEvents (resultPickEventMap holeInfo holeResult) .
  Lens.view ExpressionGui.egWidget =<<
  ExprGuiM.makeSubexpresion . Sugar.removeTypes =<<
  ExprGuiM.transaction (holeResult ^. Sugar.holeResultConvert)

maybeAddExtraSymbol :: MonadA m => Bool -> Widget.Id -> Widget f -> ExprGuiM m (Widget f)
maybeAddExtraSymbol haveExtraResults myId w
  | haveExtraResults = do
    extraSymbolLabel <-
      fmap (Widget.scale extraSymbolSizeFactor) .
      ExprGuiM.widgetEnv .
      BWidgets.makeLabel extraSymbol $ Widget.toAnimId myId
    return $ BWidgets.hboxCenteredSpaced [w, extraSymbolLabel]
  | otherwise = return w

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

makeTagGroup ::
  MonadA m => Guid ->
  ExprGuiM m (Group (DefI (Tag m)))
makeTagGroup tag = do
  (_, name) <- ExprGuiM.transaction $ ExprGuiM.getGuidName tag
  return . mkGroup [name] . Expression.BodyLeaf $ Expression.Tag tag

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

widgetIdHash :: Show a => a -> Widget.Id
widgetIdHash = WidgetIds.fromGuid . randFunc . show

resultComplexityScore :: DataIRef.ExpressionM m (Infer.Inferred (DefI (Tag m))) -> Int
resultComplexityScore =
  sum . map (subtract 2 . length . Foldable.toList . Infer.iType) .
  Foldable.toList

toMResultsList ::
  MonadA m =>
  HoleInfo m -> Widget.Id ->
  [T m (DataIRef.ExpressionM m (Maybe (Sugar.StorePoint (Tag m))))] ->
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
      { rlMainId = mconcat [resultsPrefixId holeInfo, baseId]
      , rlExtraResultsPrefixId =
        mconcat [resultsPrefixId holeInfo, Widget.Id ["extra results"], baseId]
      , rlMain = x
      , rlExtra = xs
      }

baseExprToResultsList ::
  MonadA m => HoleInfo m -> DataIRef.ExpressionM m () ->
  CT m (Maybe (ResultsList m))
baseExprToResultsList holeInfo baseExpr =
  fmap join . traverse conclude =<<
  (hiHoleActions holeInfo ^. Sugar.holeInferExprType) baseExpr
  where
    conclude baseExprType =
      toMResultsList holeInfo baseId . map (return . (Nothing <$)) $
      ExprUtil.applyForms baseExprType baseExpr
    baseId = widgetIdHash baseExpr

applyOperatorResultsList ::
  MonadA m =>
  DataIRef.ExpressionM m (Maybe (Sugar.StorePoint (Tag m))) ->
  HoleInfo m -> DataIRef.ExpressionM m () ->
  CT m (Maybe (ResultsList m))
applyOperatorResultsList argument holeInfo baseExpr =
  toMResultsList holeInfo baseId . map return =<<
  case (Nothing <$ baseExpr) ^. Expression.eBody of
  Expression.BodyLam (Expression.Lambda k paramGuid paramType result) ->
    pure $ map genExpr
    [ Expression.BodyLam (Expression.Lambda k paramGuid unwrappedArg result)
    , Expression.BodyLam (Expression.Lambda k paramGuid paramType unwrappedArg)
    ]
  Expression.BodyGetField (Expression.GetField recExpr fieldTag) ->
    pure $ map genExpr
    [ Expression.BodyGetField $ Expression.GetField unwrappedArg fieldTag
    , Expression.BodyGetField $ Expression.GetField recExpr unwrappedArg
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
    -- We always want the first (main), and we want to know if there's
    -- more (extra), so take 2:
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
makeAllResults holeInfo =
  List.catMaybes .
  List.mapL (makeResultsList holeInfo) .
  List.fromList <$>
  makeAllGroups holeInfo

makeAllGroups :: MonadA m => HoleInfo m -> ExprGuiM m [Group (DefI (Tag m))]
makeAllGroups holeInfo = do
  paramGroups <-
    traverse (makeVariableGroup . Expression.ParameterRef) $
    hiHoleActions holeInfo ^. Sugar.holeScope
  globalGroups <-
    traverse (makeVariableGroup . Expression.DefinitionRef) =<<
    ExprGuiM.getCodeAnchor Anchors.globals
  tagGroups <- traverse makeTagGroup =<< ExprGuiM.getCodeAnchor Anchors.fields
  let
    state = Property.value $ hiState holeInfo
    searchTerm = state ^. hsSearchTerm
    literalGroups = makeLiteralGroup searchTerm
    getVarGroups = paramGroups ++ globalGroups
    relevantGroups = primitiveGroups ++ literalGroups ++ getVarGroups ++ tagGroups
  return $ holeMatches groupNames searchTerm relevantGroups
  where
    primitiveGroups =
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
        Expression.GetField ExprUtil.pureHole ExprUtil.pureHole
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
      Cache.unmemoS . (hiHoleActions holeInfo ^. Sugar.holeResult) . return $
      Nothing <$
      ExprUtil.pureExpression (Lens.review ExprUtil.bodyDefinitionRef newDefI)
    mTargetGuid <- pickResultAndCleanUp holeInfo defRef
    case mTargetGuid of
      Nothing -> return ()
      Just targetGuid ->
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

data HaveHiddenResults = HaveHiddenResults | NoHiddenResults

makeHiddenResultsMWidget :: MonadA m => HaveHiddenResults -> Widget.Id -> ExprGuiM m (Maybe (Widget f))
makeHiddenResultsMWidget HaveHiddenResults myId =
  fmap Just . ExprGuiM.widgetEnv . BWidgets.makeLabel "..." $
  Widget.toAnimId myId
makeHiddenResultsMWidget NoHiddenResults _ = return Nothing

unzipF :: Functor f => f (a, b) -> (f a, f b)
unzipF x = (fst <$> x, snd <$> x)

blockDownEvents :: Monad f => Widget f -> Widget f
blockDownEvents =
  Widget.weakerEvents $
  E.keyPresses
  [E.ModKey E.noMods E.KeyDown]
  (E.Doc ["Navigation", "Move", "down (blocked)"]) $
  return Widget.emptyEventResult

makeResultsWidget ::
  MonadA m => HoleInfo m ->
  [ResultsList m] -> HaveHiddenResults ->
  ExprGuiM m (Maybe (Sugar.HoleResult m), WidgetT m)
makeResultsWidget holeInfo shownResults hiddenResults = do
  (mainResultWidgets, mActiveResults) <-
    unzip <$> traverse (resultsToWidgets holeInfo) shownResults
  let
    (mIndex, mResult) = unzipF $ mActiveResults ^@? Lens.itraversed <. Lens._Just
    (mHoleResult, mExtraResultsWidget) = unzipF mResult & Lens._2 %~ join
  shownResultsWidget <-
    case mainResultWidgets of
    [] -> makeNoResults $ Widget.toAnimId myId
    _ ->
      return . blockDownEvents $
      vboxMBiasedAlign mIndex 0 mainResultWidgets
  hiddenResultsWidgets <- maybeToList <$> makeHiddenResultsMWidget hiddenResults myId
  return
    ( mHoleResult
    , BWidgets.hboxCenteredSpaced $
      Box.vboxCentered (shownResultsWidget : hiddenResultsWidgets) :
      maybeToList mExtraResultsWidget
    )
  where
    myId = hiHoleId holeInfo

collectResults ::
  (Applicative (List.ItemM l), List l) =>
  l (ResultType, a) -> List.ItemM l ([a], HaveHiddenResults)
collectResults =
  conclude <=<
  List.splitWhenM (return . (>= Config.holeResultCount) . length . fst) .
  List.scanl step ([], [])
  where
    haveHiddenResults [] = NoHiddenResults
    haveHiddenResults _ = HaveHiddenResults
    conclude (notEnoughResults, enoughResultsM) =
      ( (Lens._2 %~ haveHiddenResults) . splitAt Config.holeResultCount
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
      mTarget <- pickResultAndCleanUp holeInfo result
      case mTarget of
        Nothing -> pure . WidgetIds.fromGuid $ hiGuid holeInfo
        Just targetGuid -> setHoleStateAndJump (charToHoleState x) targetGuid
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
      (hiHoleActions holeInfo ^. Sugar.holeResult) . return .
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
      Just $ hiHoleActions holeInfo ^. Sugar.holeResult $ return arg
  pure $ mconcat
    [ addNewDefinitionEventMap cp holeInfo
    , maybe mempty (opPickEventMap holeInfo) mResult
    , maybe mempty
      ( E.keyPresses Config.delKeys
        (E.Doc ["Edit", "Back"])
      . fmap (handlePickResultTargetGuid holeInfo)
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

assignHoleEditCursor ::
  MonadA m =>
  HoleInfo m -> [Widget.Id] -> [Widget.Id] -> Widget.Id ->
  ExprGuiM m a ->
  ExprGuiM m a
assignHoleEditCursor holeInfo shownResultsIds allResultIds searchTermId action = do
  cursor <- ExprGuiM.widgetEnv WE.readCursor
  let
    sub = isJust . flip Widget.subId cursor
    shouldBeOnResult = sub $ resultsPrefixId holeInfo
    isOnResult = any sub allResultIds
    assignSource
      | shouldBeOnResult && not isOnResult = cursor
      | otherwise = hiHoleId holeInfo
    destId = head (shownResultsIds ++ [searchTermId])
  ExprGuiM.assignCursor assignSource destId action

makeActiveHoleEdit :: MonadA m => HoleInfo m -> ExprGuiM m (ExpressionGui m)
makeActiveHoleEdit holeInfo = do
  markTypeMatchesAsUsed holeInfo
  (shownResults, hasHiddenResults) <-
    ExprGuiM.liftMemoT . collectResults =<< makeAllResults holeInfo
  let
    shownResultsIds = rlMainId <$> shownResults
    allResultIds = [rlMainId, rlExtraResultsPrefixId] <*> shownResults
  assignHoleEditCursor
    holeInfo shownResultsIds allResultIds searchTermId $ do
      (mSelectedResult, resultsWidget) <-
        makeResultsWidget holeInfo shownResults hasHiddenResults
      let
        mResult =
          mSelectedResult <|> rlMain <$> listToMaybe shownResults
        searchTermEventMap = maybe mempty (resultPickEventMap holeInfo) mResult
      searchTermWidget <-
        makeSearchTermWidget (searchTermProperty holeInfo) searchTermId
        -- TODO: Move the result picking events into pickEventMap
        -- instead of here on the searchTerm and on each result
        & Lens.mapped . ExpressionGui.egWidget %~ Widget.strongerEvents searchTermEventMap
      holeEventMap <- mkEventMap holeInfo mResult
      maybe (return ()) (ExprGuiM.addResultPicker . (^. Sugar.holeResultPickPrefix))
        mResult
      let adHocEditor = adHocTextEditEventMap $ searchTermProperty holeInfo
      return .
        Lens.over ExpressionGui.egWidget
        (Widget.strongerEvents holeEventMap .
         makeBackground (hiHoleId holeInfo)
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
  ExpressionGui.wrapDelegated holeFDConfig FocusDelegator.Delegating $
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
        { hiGuid = guid
        , hiHoleId = myId
        , hiState = stateProp
        , hiHoleActions = holeActions
        , hiMNextHole = mNextHole
        }
    (x, _) -> makeInactive (isJust x) myId

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

disallowedHoleChars :: [(Char, E.IsShifted)]
disallowedHoleChars =
  E.anyShiftedChars ",`\n() " ++
  [ ('0', E.Shifted)
  , ('9', E.Shifted)
  ]

disallowChars :: E.EventMap a -> E.EventMap a
disallowChars = E.filterSChars $ curry (`notElem` disallowedHoleChars)

makeSearchTermWidget ::
  MonadA m =>
  Property (T m) String -> Widget.Id ->
  ExprGuiM m (ExpressionGui m)
makeSearchTermWidget searchTermProp searchTermId =
  ExprGuiM.widgetEnv .
  fmap
  (flip ExpressionGui (0.5 / Config.holeSearchTermScaleFactor) .
   Widget.scale Config.holeSearchTermScaleFactor .
   Lens.over Widget.wEventMap disallowChars) $
  BWidgets.makeWordEdit searchTermProp searchTermId

holeFDConfig :: FocusDelegator.Config
holeFDConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKey = E.ModKey E.noMods E.KeyEnter
  , FocusDelegator.startDelegatingDoc = E.Doc ["Navigation", "Hole", "Enter"]
  , FocusDelegator.stopDelegatingKey = E.ModKey E.noMods E.KeyEsc
  , FocusDelegator.stopDelegatingDoc = E.Doc ["Navigation", "Hole", "Leave"]
  }

makeBackground :: Widget.Id -> Int -> Draw.Color -> Widget f -> Widget f
makeBackground myId level =
  Widget.backgroundColor level $
  mappend (Widget.toAnimId myId) ["hole background"]

makeInactive ::
  MonadA m => Bool -> Widget.Id -> ExprGuiM m (ExpressionGui m)
makeInactive isReadOnly myId =
  fmap
  (ExpressionGui.fromValueWidget .
   makeBackground myId Layers.inactiveHole unfocusedColor) .
  ExprGuiM.widgetEnv $ BWidgets.makeFocusableTextView "  " myId
  where
    unfocusedColor
      | isReadOnly = Config.holeBackgroundColor
      | otherwise = Config.readOnlyHoleBackgroundColor

resultsPrefixId :: HoleInfo m -> Widget.Id
resultsPrefixId holeInfo = mconcat [hiHoleId holeInfo, Widget.Id ["results"]]

groupOrdering :: String -> [String] -> [Bool]
groupOrdering searchTerm names =
  map not
  [ match (==)
  , match isPrefixOf
  , match insensitivePrefixOf
  , match isInfixOf
  ]
  where
    insensitivePrefixOf = isPrefixOf `on` map Char.toLower
    match f = any (f searchTerm) names

holeMatches :: (a -> [String]) -> String -> [a] -> [a]
holeMatches getNames searchTerm =
  sortOn (groupOrdering searchTerm . getNames) .
  filter nameMatch
  where
    nameMatch = any (insensitiveInfixOf searchTerm) . getNames
    insensitiveInfixOf = isInfixOf `on` map Char.toLower
