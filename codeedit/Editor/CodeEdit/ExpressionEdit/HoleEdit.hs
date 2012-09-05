{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Editor.CodeEdit.ExpressionEdit.HoleEdit(make, makeUnwrapped, ResultPicker) where

import Control.Arrow (first, second, (&&&))
import Control.Monad (liftM, mplus, msum, void, filterM)
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
import Editor.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
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

type ResultPicker m = ITransaction ViewTag m Widget.EventResult

-- TODO: Rename this (to "Choice"?). Results are the post-apply-form inferred
-- expansions of this
data Result = Result
  { resultNames :: [String]
  , resultExpr :: Data.PureExpression
  }
AtFieldTH.make ''Result

type T = Transaction ViewTag

data HoleInfo m = HoleInfo
  { hiHoleId :: Widget.Id
  , hiSearchTerm :: Property (T m) String
  , hiHole :: Sugar.Hole m
  , hiPickResult :: Sugar.HoleResult -> T m Guid
  , hiGuid :: Guid
  }

pickExpr
  :: Monad m => HoleInfo m -> Sugar.HoleResult -> ResultPicker m
pickExpr holeInfo expr = do
  guid <- IT.transaction $ hiPickResult holeInfo expr
  return Widget.EventResult
    { Widget.eCursor = Just $ WidgetIds.fromGuid guid
    , Widget.eAnimIdMapping = id -- TODO: Need to fix the parens id
    }

resultPickEventMap
  :: Monad m
  => HoleInfo m -> Sugar.HoleResult -> Widget.EventHandlers (ITransaction ViewTag m)
resultPickEventMap holeInfo =
  E.keyPresses Config.pickResultKeys "Pick this search result" .
  pickExpr holeInfo

data ResultsList m = ResultsList
  { afMain :: Sugar.HoleResult
  , afMore :: Maybe (Sugar.HoleResult, ListT m Sugar.HoleResult)
  }

canonizeResultExpr
  :: HoleInfo m
  -> Data.Expression a -> Data.Expression a
canonizeResultExpr holeInfo expr =
  flip Data.randomizeGuids expr . Random.mkStdGen $
  hash (show (void expr), Guid.bs (hiGuid holeInfo))

resultToWidget
  :: MonadF m
  => ExpressionGui.Maker m -> HoleInfo m -> ResultsList (T m)
  -> VarAccess m
     ( WidgetT m
     , Maybe (Sugar.HoleResult, Maybe (WidgetT m))
     )
resultToWidget makeExpressionEdit holeInfo applyForms = do
  cursorOnMain <- VarAccess.otransaction $ OT.isSubCursor myId
  extra <-
    if cursorOnMain
    then liftM (Just . (,) canonizedExpr . fmap fst) makeExtra
    else do
      cursorOnExtra <- VarAccess.otransaction $ OT.isSubCursor moreResultsPrefix
      if cursorOnExtra
        then do
          extra <- makeExtra
          return $ do
            (widget, mResult) <- extra
            result <- mResult
            return (result, Just widget)
        else return Nothing
  liftM (flip (,) extra) .
    maybe return (const addMoreSymbol) (afMore applyForms) =<<
    toWidget myId canonizedExpr
  where
    makeExtra = maybe (return Nothing) (liftM Just . makeMoreResults) $ afMore applyForms
    makeMoreResults (firstResult, moreResults) = do
      pairs <-
        mapM moreResult . (firstResult :) =<<
        VarAccess.transaction (List.toList moreResults)
      return
        ( BWidgets.vboxAlign 0 $ map fst pairs
        , msum $ map snd pairs
        )
    moreResult expr = do
      mResult <- (liftM . fmap . const) cExpr . VarAccess.otransaction $ OT.subCursor resultId
      widget <- toWidget resultId cExpr
      return (widget, mResult)
      where
        resultId = mappend moreResultsPrefix $ pureGuidId cExpr
        cExpr = canonizeResultExpr holeInfo expr
    toWidget resultId expr = do
      isResultSelected <- VarAccess.otransaction $ OT.isSubCursor resultId
      VarAccess.otransaction . BWidgets.makeFocusableView resultId .
        Widget.strongerEvents (resultPickEventMap holeInfo expr) .
        ExpressionGui.egWidget =<<
        makeExpressionEdit . (if isResultSelected then id else Sugar.removeTypes) =<<
        (VarAccess.transaction . Sugar.convertHoleResult) expr
    moreResultsPrefix = mconcat [hiHoleId holeInfo, Widget.Id ["more results"], canonizedExprId]
    addMoreSymbol w = do
      moreSymbolLabel <-
        liftM (Widget.scale moreSymbolSizeFactor) .
        VarAccess.otransaction .
        BWidgets.makeLabel moreSymbol $ Widget.toAnimId myId
      return $ BWidgets.hboxCenteredSpaced [w, moreSymbolLabel]
    canonizedExpr = canonizeResultExpr holeInfo $ afMain applyForms
    canonizedExprId = pureGuidId canonizedExpr
    myId = mappend (hiHoleId holeInfo) canonizedExprId
    pureGuidId = WidgetIds.fromGuid . Data.eGuid

makeNoResults :: MonadF m => AnimId -> VarAccess m (WidgetT m)
makeNoResults myId =
  VarAccess.otransaction .
  BWidgets.makeTextView "(No results)" $ mappend myId ["no results"]

makeResultVariable ::
  MonadF m => (Guid, Data.VariableRef) -> VarAccess m Result
makeResultVariable (guid, varRef) = VarAccess.withName guid $ \(_, varName) ->
  return Result
    { resultNames = [varName]
    , resultExpr = toPureExpr . Data.ExpressionLeaf $ Data.GetVariable varRef
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
      , resultExpr = toPureExpr . Data.ExpressionLeaf $ Data.LiteralInteger integer
      }

makeResultsList ::
  Monad m => ListT m Sugar.HoleResult -> m (Maybe (ResultsList m))
makeResultsList applyForms = do
  -- We always want the first, and we want to know if there's more, so
  -- take 2:
  (firstTwo, rest) <- List.splitAtM 2 applyForms
  return $ case firstTwo of
    [] -> Nothing
    (x:xs) -> Just ResultsList
      { afMain = x
      , afMore = case xs of
        [] -> Nothing
        [y] -> Just (y, rest)
        _ -> error "We took 2, got more!"
      }

makeAllResults
  :: MonadF m
  => HoleInfo m
  -> VarAccess m (ListT (T m) (ResultsList (T m)))
makeAllResults holeInfo = do
  paramResults <-
    mapM makeResultVariable $
    Sugar.holeScope hole
  globalResults <-
    mapM (makeResultVariable . (Data.variableRefGuid &&& id)) =<<
    VarAccess.getP Anchors.globals
  let
    searchTerm = Property.value $ hiSearchTerm holeInfo
    literalResults = makeLiteralResults searchTerm
    nameMatch = any (insensitiveInfixOf searchTerm) . resultNames
  return .
    List.catMaybes .
    List.mapL
      (makeResultsList . Sugar.holeInferResults hole . resultExpr) .
    List.fromList .
    sortOn (resultOrdering searchTerm) .
    filter nameMatch $
    literalResults ++
    paramResults ++
    primitiveResults ++
    globalResults
  where
    insensitiveInfixOf = isInfixOf `on` map Char.toLower
    hole = hiHole holeInfo
    primitiveResults =
      [ Result
        { resultNames = ["Set", "Type"]
        , resultExpr = toPureExpr $ Data.ExpressionLeaf Data.Set
        }
      , Result
        { resultNames = ["Integer", "ℤ", "Z"]
        , resultExpr = toPureExpr $ Data.ExpressionLeaf Data.IntegerType
        }
      , Result
        { resultNames = ["->", "Pi", "→", "→", "Π", "π"]
        , resultExpr =
          Data.canonizeGuids . toPureExpr . Data.ExpressionPi $
          Data.Lambda holeExpr holeExpr
        }
      , Result
        { resultNames = ["\\", "Lambda", "Λ", "λ"]
        , resultExpr =
          Data.canonizeGuids . toPureExpr . Data.ExpressionLambda $
          Data.Lambda holeExpr holeExpr
        }
      ]
    holeExpr = toPureExpr $ Data.ExpressionLeaf Data.Hole


makeSearchTermWidget
  :: MonadF m
  => HoleInfo m -> Widget.Id
  -> Maybe Sugar.HoleResult
  -> VarAccess m (WidgetT m)
makeSearchTermWidget holeInfo searchTermId mFirstResult =
  VarAccess.otransaction .
  liftM
  (Widget.strongerEvents searchTermEventMap .
   (Widget.atWEventMap . E.filterChars) (`notElem` "`[]\\")) $
  BWidgets.makeWordEdit (hiSearchTerm holeInfo) searchTermId
  where
    pickFirstResultEventMap =
      maybe mempty (resultPickEventMap holeInfo) mFirstResult

    searchTermEventMap =
      pickFirstResultEventMap `mappend`
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
      defRef <-
        liftM (fromMaybe (error "GetDef should always type-check") . listToMaybe) .
        IT.transaction . List.toList .
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

vboxMBiasedAlign ::
  Maybe Box.Cursor -> Box.Alignment -> [Widget f] -> Widget f
vboxMBiasedAlign mChildIndex align =
  maybe Box.toWidget Box.toWidgetBiased mChildIndex .
  Box.makeAlign align Box.vertical

makeResultsWidget
  :: MonadF m
  => ExpressionGui.Maker m -> HoleInfo m
  -> [ResultsList (T m)] -> Bool
  -> VarAccess m
     (Maybe Sugar.HoleResult, WidgetT m)
makeResultsWidget makeExpressionEdit holeInfo firstResults moreResults = do
  firstResultsAndWidgets <-
    mapM (resultToWidget makeExpressionEdit holeInfo) firstResults
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
      BWidgets.vboxCentered (firstResultsWidget : moreResultsWidgets) :
      extraWidgets
    )
  where
    myId = hiHoleId holeInfo
    blockDownEvents =
      Widget.weakerEvents $
      Widget.keysEventMap
      [E.ModKey E.noMods E.KeyDown]
      "Nothing (at bottom)" (return ())

genericNull :: List l => l a -> List.ItemM l Bool
genericNull = liftM null . List.toList . List.take 1

makeActiveHoleEdit
  :: MonadF m
  => ExpressionGui.Maker m -> HoleInfo m
  -> VarAccess m
     (Maybe Sugar.HoleResult, WidgetT m)
makeActiveHoleEdit makeExpressionEdit holeInfo =
  VarAccess.assignCursor (hiHoleId holeInfo) searchTermId $ do
    VarAccess.markVariablesAsUsed . map fst =<<
      (filterM
       (VarAccess.transaction . checkInfer . toPureExpr .
        Data.ExpressionLeaf . Data.GetVariable . snd) .
       Sugar.holeScope . hiHole)
      holeInfo

    allResults <- makeAllResults holeInfo

    (firstResults, moreResults) <-
      VarAccess.transaction $ List.splitAtM Config.holeResultCount allResults

    let defaultResult = fmap afMain $ listToMaybe firstResults
    searchTermWidget <-
      makeSearchTermWidget holeInfo searchTermId defaultResult

    hasMoreResults <- VarAccess.transaction $ genericNull moreResults

    (mResult, resultsWidget) <-
      makeResultsWidget makeExpressionEdit holeInfo firstResults $
      not hasMoreResults
    return
      ( mplus mResult defaultResult
      , BWidgets.vboxCentered [searchTermWidget, resultsWidget]
      )
  where
    checkInfer = liftM not . genericNull . Sugar.holeInferResults (hiHole holeInfo)
    searchTermId = WidgetIds.searchTermId $ hiHoleId holeInfo

holeFDConfig :: FocusDelegator.Config
holeFDConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKey = E.ModKey E.noMods E.KeyEnter
  , FocusDelegator.startDelegatingDoc = "Enter hole"
  , FocusDelegator.stopDelegatingKey = E.ModKey E.noMods E.KeyEsc
  , FocusDelegator.stopDelegatingDoc = "Leave hole"
  }

make
  :: MonadF m
  => ExpressionGui.Maker m
  -> Sugar.Hole m -> Guid -> Widget.Id
  -> VarAccess m
     (Maybe (ResultPicker m), ExpressionGui m)
make makeExpressionEdit hole guid =
  BWidgets.wrapDelegatedVA holeFDConfig FocusDelegator.Delegating (second . ExpressionGui.atEgWidget) $
  makeUnwrapped makeExpressionEdit hole guid

makeUnwrapped ::
  MonadF m =>
  ExpressionGui.Maker m ->
  Sugar.Hole m -> Guid -> Widget.Id ->
  VarAccess m (Maybe (ResultPicker m), ExpressionGui m)
makeUnwrapped makeExpressionEdit hole guid myId = do
  cursor <- VarAccess.otransaction OT.readCursor
  searchTermProp <- VarAccess.transaction $ Anchors.assocSearchTermRef guid
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
        ((first . fmap) (pickExpr holeInfo) .
         second (makeBackground Layers.activeHoleBG Config.holeBackgroundColor)) $
        makeActiveHoleEdit makeExpressionEdit holeInfo
    _ ->
      liftM
      ((,) Nothing . makeBackground Layers.inactiveHole unfocusedColor) .
      VarAccess.otransaction .
      BWidgets.makeFocusableTextView snippet $
      WidgetIds.searchTermId myId
  where
    unfocusedColor
      | canPickResult = Config.holeBackgroundColor
      | otherwise = Config.unfocusedReadOnlyHoleBackgroundColor
    canPickResult = isJust $ Sugar.holePickResult hole
    makeBackground level =
      Widget.backgroundColor level $
      mappend (Widget.toAnimId myId) ["hole background"]
