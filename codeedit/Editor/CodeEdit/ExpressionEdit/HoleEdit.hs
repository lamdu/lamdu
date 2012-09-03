{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Editor.CodeEdit.ExpressionEdit.HoleEdit(make, ResultPicker) where

import Control.Arrow (first, second, (&&&))
import Control.Monad (liftM, mplus, msum, void)
import Control.Monad.ListT (ListT)
import Data.Function (on)
import Data.Hashable(hash)
import Data.List (isInfixOf, isPrefixOf)
import Data.List.Utils (sortOn)
import Data.Maybe (isJust, listToMaybe, maybeToList, mapMaybe, fromMaybe)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.Property (Property(..))
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import Editor.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Editor.ITransaction (ITransaction)
import Editor.MonadF (MonadF)
import Editor.OTransaction (OTransaction, WidgetT)
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
import qualified Editor.Config as Config
import qualified Editor.Data as Data
import qualified Editor.ITransaction as IT
import qualified Editor.OTransaction as OT
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
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

data ApplyForms m = ApplyForms
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
  => ExpressionGui.Maker m -> HoleInfo m -> ApplyForms (T m)
  -> OTransaction ViewTag m
     ( WidgetT ViewTag m
     , Maybe (Sugar.HoleResult, Maybe (WidgetT ViewTag m))
     )
resultToWidget makeExpressionEdit holeInfo applyForms = do
  cursorOnMain <- liftM isJust $ OT.subCursor myId
  extra <-
    if cursorOnMain
    then liftM (Just . (,) canonizedExpr . fmap fst) makeExtra
    else do
      cursorOnExtra <- liftM isJust $ OT.subCursor moreResultsPrefix
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
        OT.transaction (List.toList moreResults)
      return
        ( BWidgets.vboxAlign 0 $ map fst pairs
        , msum $ map snd pairs
        )
    moreResult expr = do
      mResult <- (liftM . fmap . const) cExpr $ OT.subCursor ident
      widget <- toWidget ident cExpr
      return (widget, mResult)
      where
        ident = mappend moreResultsPrefix $ pureGuidId cExpr
        cExpr = canonizeResultExpr holeInfo expr
    toWidget ident expr =
      BWidgets.makeFocusableView ident .
      Widget.strongerEvents (resultPickEventMap holeInfo expr) .
      ExpressionGui.egWidget =<<
      makeExpressionEdit . Sugar.removeTypes =<<
      (OT.transaction . Sugar.convertHoleResult) expr
    moreResultsPrefix = mconcat [hiHoleId holeInfo, Widget.Id ["more results"], canonizedExprId]
    addMoreSymbol w = do
      moreSymbolLabel <-
        liftM (Widget.scale moreSymbolSizeFactor) .
        BWidgets.makeLabel moreSymbol $ Widget.toAnimId myId
      return $ BWidgets.hboxCenteredSpaced [w, moreSymbolLabel]
    canonizedExpr = canonizeResultExpr holeInfo $ afMain applyForms
    canonizedExprId = pureGuidId canonizedExpr
    myId = mappend (hiHoleId holeInfo) canonizedExprId
    pureGuidId = WidgetIds.fromGuid . Data.eGuid

makeNoResults :: MonadF m => AnimId -> OTransaction t m (WidgetT t m)
makeNoResults myId =
  BWidgets.makeTextView "(No results)" $ mappend myId ["no results"]

makeResultVariable ::
  MonadF m => (Guid, Data.VariableRef) -> OTransaction ViewTag m Result
makeResultVariable (guid, varRef) = do
  varName <- liftM snd $ OT.getName guid
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

makeApplyForms ::
  Monad m => ListT m Sugar.HoleResult -> m (Maybe (ApplyForms m))
makeApplyForms applyForms = do
  -- We always want the first, and we want to know if there's more, so
  -- take 2:
  (firstTwo, rest) <- List.splitAtM 2 applyForms
  return $ case firstTwo of
    [] -> Nothing
    (x:xs) -> Just ApplyForms
      { afMain = x
      , afMore = case xs of
        [] -> Nothing
        [y] -> Just (y, rest)
        _ -> error "We took 2, got more!"
      }

makeAllResults
  :: MonadF m
  => HoleInfo m
  -> OTransaction ViewTag m (ListT (T m) (ApplyForms (T m)))
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
  return .
    List.catMaybes .
    List.mapL
      (makeApplyForms . Sugar.holeInferResults hole . resultExpr) .
    List.fromList .
    sortOn (resultOrdering searchTerm) .
    filter nameMatch $
    literalResults ++ primitiveResults ++ varResults
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
  -> OTransaction ViewTag m (WidgetT ViewTag m)
makeSearchTermWidget holeInfo searchTermId mFirstResult =
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
  -> [ApplyForms (T m)] -> Bool
  -> OTransaction ViewTag m
     (Maybe Sugar.HoleResult, WidgetT ViewTag m)
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

makeActiveHoleEdit
  :: MonadF m
  => ExpressionGui.Maker m -> HoleInfo m
  -> OTransaction ViewTag m
     (Maybe Sugar.HoleResult, WidgetT ViewTag m)
makeActiveHoleEdit makeExpressionEdit holeInfo =
  OT.assignCursor (hiHoleId holeInfo) searchTermId $ do
    OT.markVariablesAsUsed . map fst . Sugar.holeScope $ hiHole holeInfo

    allResults <- makeAllResults holeInfo

    (firstResults, moreResults) <-
      OT.transaction $ List.splitAtM Config.holeResultCount allResults

    let defaultResult = fmap afMain $ listToMaybe firstResults
    searchTermWidget <-
      makeSearchTermWidget holeInfo searchTermId defaultResult

    hasMoreResults <-
      liftM null . OT.transaction . List.toList $ List.take 1 moreResults

    (mResult, resultsWidget) <-
      makeResultsWidget makeExpressionEdit holeInfo firstResults $
      not hasMoreResults
    return
      ( mplus mResult defaultResult
      , BWidgets.vboxCentered [searchTermWidget, resultsWidget]
      )
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
  searchTermProp <- OT.transaction $ Anchors.assocSearchTermRef guid
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
         second (makeBackground 8 Config.holeBackgroundColor)) $
        makeActiveHoleEdit makeExpressionEdit holeInfo
    _ ->
      liftM
      ((,) Nothing . makeBackground 9 unfocusedColor) .
      BWidgets.makeFocusableTextView snippet $
      WidgetIds.searchTermId myId
  where
    -- TODO: If just one possible result due to type, ignore the
    -- search string
    maybeRemovePicker "" = const Nothing
    maybeRemovePicker _ = id
    unfocusedColor
      | canPickResult = Config.holeBackgroundColor
      | otherwise = Config.unfocusedReadOnlyHoleBackgroundColor
    canPickResult = isJust $ Sugar.holePickResult hole
    makeBackground level =
      Widget.backgroundColor level $
      mappend (Widget.toAnimId myId) ["hole background"]
