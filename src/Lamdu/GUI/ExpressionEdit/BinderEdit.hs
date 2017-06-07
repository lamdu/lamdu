{-# LANGUAGE NamedFieldPuns, RecordWildCards, NoImplicitPrelude, OverloadedStrings, LambdaCase #-}
module Lamdu.GUI.ExpressionEdit.BinderEdit
    ( make
    , makeBinderBodyEdit
    , Parts(..), makeParts
    , nonOperatorName
    ) where

import           Control.Applicative ((<|>), liftA2)
import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Transaction as Transaction
import           Data.CurAndPrev (CurAndPrev, current)
import           Data.List.Utils (nonEmptyAll)
import qualified Data.Map as Map
import           Data.Store.Transaction (Transaction)
import qualified Data.Text as Text
import           Data.Text.Encoding (encodeUtf8)
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.MetaKey (MetaKey(..), noMods, toModKey)
import qualified Graphics.UI.Bottle.View as View
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import           Graphics.UI.Bottle.Widget.Aligned (AlignedWidget)
import qualified Graphics.UI.Bottle.Widget.Aligned as AlignedWidget
import qualified Graphics.UI.Bottle.Widget.TreeLayout as TreeLayout
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.Choice as Choice
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import qualified Graphics.UI.GLFW as GLFW
import           Lamdu.CharClassification (operatorChars)
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.GUI.CodeEdit.Settings as CESettings
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.ParamEdit as ParamEdit
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.Names.Types (Name(..), NameSource(..))
import           Lamdu.Sugar.NearestHoles (NearestHoles)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction

nonOperatorName :: Name m -> Bool
nonOperatorName (Name NameSourceStored _ _ x) =
    nonEmptyAll (`notElem` operatorChars) (Text.unpack x)
nonOperatorName _ = False

makeBinderNameEdit ::
    Monad m =>
    Sugar.BinderActions m ->
    Widget.EventMap (T m Widget.EventResult) ->
    Name m -> Draw.Color -> Widget.Id ->
    ExprGuiM m (ExpressionGui m)
makeBinderNameEdit binderActions rhsJumperEquals name color myId =
    do
        config <- ExprGuiM.readConfig
        ExpressionGui.makeNameOriginEdit name color myId
            <&> jumpToRHSViaEquals name
            <&> E.weakerEvents
                (ParamEdit.eventMapAddFirstParam config
                 (binderActions ^. Sugar.baAddFirstParam))
            <&> TreeLayout.fromCenteredWidget
    where
        jumpToRHSViaEquals n
            | nonOperatorName n = E.strongerEvents rhsJumperEquals
            | otherwise = id

presentationModeChoiceConfig :: Choice.Config
presentationModeChoiceConfig = Choice.Config
    { Choice.cwcFDConfig =
        FocusDelegator.Config
        { FocusDelegator.focusChildKeys = [MetaKey noMods GLFW.Key'Enter]
        , FocusDelegator.focusChildDoc = E.Doc ["Presentation Mode", "Select"]
        , FocusDelegator.focusParentKeys = [MetaKey noMods GLFW.Key'Enter]
        , FocusDelegator.focusParentDoc = E.Doc ["Presentation Mode", "Choose selected"]
        }
    , Choice.cwcOrientation = Box.Vertical
    , Choice.cwcExpandMode = Choice.ExplicitEntry
    }

mkPresentationModeEdit ::
    Monad m => Widget.Id ->
    Transaction.MkProperty m Sugar.PresentationMode ->
    ExprGuiM m (Widget (T m Widget.EventResult))
mkPresentationModeEdit myId prop = do
    cur <- Transaction.getP prop
    theme <- ExprGuiM.readTheme
    let mkPair presentationMode = do
            widget <-
                TextView.makeFocusable ?? text ?? labelId
                & Reader.local
                  (TextView.style . TextView.styleColor .~ Theme.presentationChoiceColor theme)
            return (presentationMode, widget)
            where
                labelId = Widget.joinId myId [encodeUtf8 text]
                text = show presentationMode & Text.pack
    pairs <- traverse mkPair [Sugar.OO, Sugar.Verbose, Sugar.Infix]
    Choice.make ?? Transaction.setP prop ?? pairs ?? cur
        ?? presentationModeChoiceConfig ?? myId
        <&> Widget.scale (realToFrac <$> Theme.presentationChoiceScaleFactor theme)

data Parts m = Parts
    { pMParamsEdit :: Maybe (ExpressionGui m)
    , pMScopesEdit :: Maybe (AlignedWidget (T m Widget.EventResult))
    , pBodyEdit :: ExpressionGui m
    , pEventMap :: Widget.EventMap (T m Widget.EventResult)
    }

data ScopeCursor = ScopeCursor
    { sBinderScope :: Sugar.BinderParamScopeId
    , sMPrevParamScope :: Maybe Sugar.BinderParamScopeId
    , sMNextParamScope :: Maybe Sugar.BinderParamScopeId
    }

trivialScopeCursor :: Sugar.BinderParamScopeId -> ScopeCursor
trivialScopeCursor x = ScopeCursor x Nothing Nothing

scopeCursor :: Maybe Sugar.BinderParamScopeId -> [Sugar.BinderParamScopeId] -> Maybe ScopeCursor
scopeCursor mChosenScope scopes =
    do
        chosenScope <- mChosenScope
        (prevs, it:nexts) <- break (== chosenScope) scopes & Just
        Just ScopeCursor
            { sBinderScope = it
            , sMPrevParamScope = reverse prevs ^? Lens.traversed
            , sMNextParamScope = nexts ^? Lens.traversed
            }
    <|> (scopes ^? Lens.traversed <&> def)
    where
        def binderScope =
            ScopeCursor
            { sBinderScope = binderScope
            , sMPrevParamScope = Nothing
            , sMNextParamScope = scopes ^? Lens.ix 1
            }

lookupMKey :: Ord k => Maybe k -> Map k a -> Maybe a
lookupMKey k m = k >>= (`Map.lookup` m)

readBinderChosenScope ::
    Monad m =>
    Sugar.Binder name m expr -> ExprGuiM m (Maybe Sugar.BinderParamScopeId)
readBinderChosenScope binder =
    binder ^. Sugar.bChosenScopeProp & Transaction.getP

mkChosenScopeCursor ::
    Monad m =>
    Sugar.Binder (Name m) m (ExprGuiT.SugarExpr m) ->
    ExprGuiM m (CurAndPrev (Maybe ScopeCursor))
mkChosenScopeCursor binder =
    do
        mOuterScopeId <- ExprGuiM.readMScopeId
        case binder ^. Sugar.bBodyScopes of
            Sugar.SameAsParentScope ->
                mOuterScopeId <&> fmap (trivialScopeCursor . Sugar.BinderParamScopeId) & return
            Sugar.BinderBodyScope binderBodyScope ->
                do
                    mChosenScope <- readBinderChosenScope binder
                    liftA2 lookupMKey mOuterScopeId binderBodyScope
                        <&> (>>= scopeCursor mChosenScope) & return

makeScopeEventMap ::
    Monad m =>
    [MetaKey] -> [MetaKey] -> ScopeCursor -> (Sugar.BinderParamScopeId -> m ()) ->
    Widget.EventMap (m Widget.EventResult)
makeScopeEventMap prevKey nextKey cursor setter =
    do
        (key, doc, scope) <-
            (sMPrevParamScope cursor ^.. Lens._Just <&> (,,) prevKey prevDoc) ++
            (sMNextParamScope cursor ^.. Lens._Just <&> (,,) nextKey nextDoc)
        [setter scope & Widget.keysEventMap key doc]
    & mconcat
    where
        prevDoc = E.Doc ["Evaluation", "Scope", "Previous"]
        nextDoc = E.Doc ["Evaluation", "Scope", "Next"]

blockEventMap :: Monad m => Widget.EventMap (m Widget.EventResult)
blockEventMap =
    return mempty
    & E.keyPresses (dirKeys <&> toModKey)
    (E.Doc ["Navigation", "Move", "(blocked)"])
    where
        dirKeys = [GLFW.Key'Left, GLFW.Key'Right] <&> MetaKey noMods

makeScopeNavEdit ::
    Monad m =>
    Sugar.Binder name m expr -> Widget.Id -> ScopeCursor ->
    ExprGuiM m
    ( Widget.EventMap (T m Widget.EventResult)
    , Maybe (AlignedWidget (T m Widget.EventResult))
    )
makeScopeNavEdit binder myId curCursor =
    do
        theme <- ExprGuiM.readTheme
        let mkArrow (txt, mScopeId) =
                ExpressionGui.makeLabel txt
                & Reader.local
                ( TextView.color .~
                    case mScopeId of
                    Nothing -> Theme.disabledColor theme
                    Just _ -> Theme.grammarColor theme
                )
        Config.Eval{..} <- ExprGuiM.readConfig <&> Config.eval
        settings <- ExprGuiM.readSettings
        case settings ^. CESettings.sInfoMode of
            CESettings.Evaluation ->
                (Widget.makeFocusableView ?? myId <&> (AlignedWidget.widget %~))
                <*> (mapM mkArrow scopes <&> AlignedWidget.hbox 0.5)
                <&> E.weakerEvents (mkScopeEventMap leftKeys rightKeys `mappend` blockEventMap)
                <&> Just
                <&> (,) (mkScopeEventMap prevScopeKeys nextScopeKeys)
            _ -> return (mempty, Nothing)
    where
        mkScopeEventMap l r = makeScopeEventMap l r curCursor setScope
        leftKeys = [MetaKey noMods GLFW.Key'Left]
        rightKeys = [MetaKey noMods GLFW.Key'Right]
        scopes :: [(Text, Maybe Sugar.BinderParamScopeId)]
        scopes =
            [ ("◀", sMPrevParamScope curCursor)
            , (" ", Nothing)
            , ("▶", sMNextParamScope curCursor)
            ]
        setScope = Transaction.setP (binder ^. Sugar.bChosenScopeProp) . Just

data IsScopeNavFocused = ScopeNavIsFocused | ScopeNavNotFocused
    deriving (Eq, Ord)

makeMParamsEdit ::
    Monad m =>
    CurAndPrev (Maybe ScopeCursor) -> IsScopeNavFocused ->
    Widget.Id -> Widget.Id ->
    NearestHoles -> Widget.Id -> Sugar.BinderParams (Name m) m ->
    ExprGuiM m (Maybe (ExpressionGui m))
makeMParamsEdit mScopeCursor isScopeNavFocused delVarBackwardsId myId nearestHoles bodyId params =
    params
    & makeParamsEdit annotationMode nearestHoles
      delVarBackwardsId myId bodyId
    & ExprGuiM.withLocalMScopeId
      ( mScopeCursor
      <&> Lens.traversed %~ (^. Sugar.bParamScopeId) . sBinderScope
      )
    >>= \case
    [] -> return Nothing
    paramEdits ->
        frame
        <*>
        ( ExpressionGui.combineSpaced ??
            (paramEdits <&> TreeLayout.alignment . _1 .~ 0.5)
        )
        <&> Just
    where
        frame =
            case params of
            Sugar.FieldParams{} -> ExpressionGui.addValFrame
            _ -> return id
        mCurCursor =
            do
                ScopeNavIsFocused == isScopeNavFocused & guard
                mScopeCursor ^. current
        annotationMode =
            ExpressionGui.NeighborVals
            (mCurCursor >>= sMPrevParamScope)
            (mCurCursor >>= sMNextParamScope)
            & ExpressionGui.WithNeighbouringEvalAnnotations

binderContentNearestHoles :: Sugar.BinderContent name m (ExprGuiT.SugarExpr m) -> NearestHoles
binderContentNearestHoles body =
    body ^? Lens.traverse
    & fromMaybe (error "We have at least a body expression inside the binder")
    & ExprGuiT.nextHolesBefore

makeParts ::
    Monad m =>
    ExprGuiT.FuncApplyLimit ->
    Sugar.Binder (Name m) m (ExprGuiT.SugarExpr m) ->
    Widget.Id -> Widget.Id ->
    ExprGuiM m (Parts m)
makeParts funcApplyLimit binder delVarBackwardsId myId =
    do
        mScopeCursor <- mkChosenScopeCursor binder
        let binderScopeId = mScopeCursor <&> Lens.mapped %~ (^. Sugar.bParamScopeId) . sBinderScope
        (scopeEventMap, mScopeNavEdit) <-
            do
                guard (funcApplyLimit == ExprGuiT.UnlimitedFuncApply)
                currentScope <- mScopeCursor ^. current
                guard $
                    Lens.nullOf (Sugar.bParams . Sugar._NullParam) binder ||
                    Lens.has (Lens.traversed . Lens._Just) [sMPrevParamScope currentScope, sMNextParamScope currentScope]
                Just currentScope
            & maybe (return (mempty, Nothing)) (makeScopeNavEdit binder scopesNavId)
        let isScopeNavFocused =
                case mScopeNavEdit of
                Just layout
                    | Widget.isFocused (layout ^. AlignedWidget.widget) -> ScopeNavIsFocused
                _ -> ScopeNavNotFocused
        do
            mParamsEdit <-
                makeMParamsEdit mScopeCursor isScopeNavFocused delVarBackwardsId myId
                (binderContentNearestHoles bodyContent) bodyId params
            rhs <- makeBinderBodyEdit body
            Parts mParamsEdit mScopeNavEdit rhs scopeEventMap & return
            & case mScopeNavEdit of
              Nothing -> Widget.assignCursorPrefix scopesNavId (const destId)
              Just _ -> id
            & ExprGuiM.withLocalMScopeId binderScopeId
    where
        destId =
            params ^? SugarLens.binderNamedParams . Sugar.fpId
            <&> WidgetIds.fromEntityId
            & fromMaybe bodyId
        params = binder ^. Sugar.bParams
        body = binder ^. Sugar.bBody
        bodyContent = body ^. Sugar.bbContent
        bodyId = bodyContent ^. SugarLens.binderContentEntityId & WidgetIds.fromEntityId
        scopesNavId = Widget.joinId myId ["scopesNav"]

make ::
    Monad m =>
    Name m -> Draw.Color ->
    Sugar.Binder (Name m) m (ExprGuiT.SugarExpr m) ->
    Widget.Id ->
    ExprGuiM m (ExpressionGui m)
make name color binder myId =
    do
        Parts mParamsEdit mScopeEdit bodyEdit eventMap <-
            makeParts ExprGuiT.UnlimitedFuncApply binder myId myId
        rhsJumperEquals <- jumpToRHS (body ^. SugarLens.binderContentEntityId)
        presentationEdits <-
            binder ^.. Sugar.bMPresentationModeProp . Lens._Just
            & traverse (mkPresentationModeEdit presentationChoiceId)
        jumpHolesEventMap <-
            ExprEventMap.jumpHolesEventMap (binderContentNearestHoles body)
        defNameEdit <-
            makeBinderNameEdit (binder ^. Sugar.bActions) rhsJumperEquals
            name color myId
            <&> TreeLayout.alignment . _1 .~ 0
            <&> TreeLayout.alignedWidget %~
                AlignedWidget.addAfter AlignedWidget.Vertical
                (presentationEdits
                <&> AlignedWidget.fromCenteredWidget
                <&> AlignedWidget.alignment . _1 .~ 0)
            <&> E.weakerEvents jumpHolesEventMap
        mLhsEdit <-
            case mParamsEdit of
            Nothing -> return Nothing
            Just paramsEdit ->
                ExpressionGui.vboxTopFocalSpaced
                ?? (paramsEdit : fmap TreeLayout.fromAlignedWidget mScopeEdit ^.. Lens._Just
                    <&> TreeLayout.alignment . _1 .~ 0.5)
                <&> E.strongerEvents rhsJumperEquals
                <&> Just
        equals <- ExpressionGui.makeLabel "="
        ExpressionGui.combineSpaced
            <&>
            (\hbox ->
            hbox
            [ hbox (defNameEdit : (mLhsEdit ^.. Lens._Just) ++ [TreeLayout.fromAlignedWidget equals])
            , bodyEdit
            ] )
            <&> E.weakerEvents eventMap
    & Reader.local (View.animIdPrefix .~ Widget.toAnimId myId)
    where
        presentationChoiceId = Widget.joinId myId ["presentation"]
        body = binder ^. Sugar.bBody . Sugar.bbContent

makeLetEdit ::
    Monad m =>
    Sugar.Let (Name m) m (ExprGuiT.SugarExpr m) ->
    ExprGuiM m (ExpressionGui m)
makeLetEdit item =
    do
        config <- ExprGuiM.readConfig
        theme <- ExprGuiM.readTheme
        let letColor = Theme.letColor (Theme.name theme)
        let actionsEventMap =
                mconcat
                [ Widget.keysEventMapMovesCursor (Config.delKeys config)
                  (E.Doc ["Edit", "Let clause", "Delete"]) $
                  bodyId <$ item ^. Sugar.lActions . Sugar.laSetToInner
                , Widget.keysEventMapMovesCursor (Config.extractKeys config)
                  (E.Doc ["Edit", "Let clause", "Extract to outer scope"]) $
                  WidgetIds.fromEntityId . Sugar.lfrNewEntity <$>
                  item ^. Sugar.lActions . Sugar.laFloat
                ]
        let usageEventMap =
                mconcat
                [ Widget.keysEventMapMovesCursor (Config.inlineKeys config)
                  (E.Doc ["Navigation", "Jump to first use"])
                  (return (WidgetIds.fromEntityId usage))
                | usage <- take 1 (item ^. Sugar.lUsages)
                ]
        let eventMap = mappend actionsEventMap usageEventMap
        ExpressionGui.tagItem
            <*> ExpressionGui.grammarLabel "let"
            <*> (make (item ^. Sugar.lName) letColor binder myId
                <&> E.weakerEvents eventMap
                <&> TreeLayout.pad
                    (Theme.letItemPadding theme <&> realToFrac)
                )
    & Reader.local (View.animIdPrefix .~ Widget.toAnimId myId)
    where
        bodyId =
            item ^. Sugar.lBody . Sugar.bbContent . SugarLens.binderContentEntityId
            & WidgetIds.fromEntityId
        myId = item ^. Sugar.lEntityId & WidgetIds.fromEntityId
        binder = item ^. Sugar.lValue

jumpToRHS ::
    Monad f =>
    Sugar.EntityId ->
    ExprGuiM f (Widget.EventMap (T f Widget.EventResult))
jumpToRHS rhsId =
    ExprGuiM.mkPrejumpPosSaver
    <&> Lens.mapped .~ WidgetIds.fromEntityId rhsId
    <&> Widget.keysEventMapMovesCursor [MetaKey noMods GLFW.Key'Equal]
        (E.Doc ["Navigation", "Jump to Def Body"])

makeBinderBodyEdit ::
    Monad m =>
    Sugar.BinderBody (Name m) m (ExprGuiT.SugarExpr m) ->
    ExprGuiM m (ExpressionGui m)
makeBinderBodyEdit (Sugar.BinderBody addOuterLet content) =
    do
        config <- ExprGuiM.readConfig
        savePos <- ExprGuiM.mkPrejumpPosSaver
        let newLetEventMap =
                savePos >> addOuterLet
                <&> WidgetIds.fromEntityId <&> WidgetIds.nameEditOf
                & Widget.keysEventMapMovesCursor (Config.letAddItemKeys config)
                  (E.Doc ["Edit", "Let clause", "Add"])
        makeBinderContentEdit content <&> E.weakerEvents newLetEventMap

makeBinderContentEdit ::
    Monad m =>
    Sugar.BinderContent (Name m) m (ExprGuiT.SugarExpr m) ->
    ExprGuiM m (ExpressionGui m)
makeBinderContentEdit (Sugar.BinderExpr binderBody) =
    ExprGuiM.makeSubexpression binderBody
makeBinderContentEdit (Sugar.BinderLet l) =
    do
        config <- ExprGuiM.readConfig
        let delEventMap =
                l ^. Sugar.lActions . Sugar.laSetToHole
                <&> WidgetIds.fromEntityId
                & Widget.keysEventMapMovesCursor (Config.delKeys config)
                  (E.Doc ["Edit", "Delete let expression"])
        let moveToInnerEventMap =
                body
                ^? Sugar.bbContent . Sugar._BinderLet
                . Sugar.lActions . Sugar.laFloat
                & maybe mempty
                (Widget.keysEventMap (Config.moveLetInwardKeys config)
                (E.Doc ["Edit", "Let clause", "Move inwards"]) . void)
        mOuterScopeId <- ExprGuiM.readMScopeId
        let letBodyScope = liftA2 lookupMKey mOuterScopeId (l ^. Sugar.lBodyScope)
        ExpressionGui.parentDelegator letEntityId
            <*> ( ExpressionGui.vboxTopFocalSpaced
                  <*>
                  ( sequence
                    [ makeLetEdit l <&> E.weakerEvents moveToInnerEventMap
                    , makeBinderBodyEdit body
                      & ExprGuiM.withLocalMScopeId letBodyScope
                    ] <&> map (TreeLayout.alignment . _1 .~ 0)
                  )
                )
            <&> E.weakerEvents delEventMap
    where
        letEntityId = l ^. Sugar.lEntityId & WidgetIds.fromEntityId
        body = l ^. Sugar.lBody

namedParamEditInfo :: Monad m => Draw.Color -> Sugar.NamedParamInfo (Name m) m -> ParamEdit.Info m
namedParamEditInfo color paramInfo =
    ParamEdit.Info
    { ParamEdit.iMakeNameEdit =
      ExpressionGui.makeNameOriginEdit (paramInfo ^. Sugar.npiName) color
      <&> Lens.mapped %~ TreeLayout.fromCenteredWidget
    , ParamEdit.iMAddNext = paramInfo ^. Sugar.npiActions . Sugar.fpAddNext & Just
    , ParamEdit.iMOrderBefore = paramInfo ^. Sugar.npiActions . Sugar.fpMOrderBefore
    , ParamEdit.iMOrderAfter = paramInfo ^. Sugar.npiActions . Sugar.fpMOrderAfter
    , ParamEdit.iDel = paramInfo ^. Sugar.npiActions . Sugar.fpDelete
    }

nullParamEditInfo :: Monad m => Sugar.NullParamActions m -> ParamEdit.Info m
nullParamEditInfo mActions =
    ParamEdit.Info
    { ParamEdit.iMakeNameEdit =
      \myId ->
      ExpressionGui.makeFocusableView myId
      <*> ExpressionGui.grammarLabel "◗"
      <&> TreeLayout.fromAlignedWidget
    , ParamEdit.iMAddNext = Nothing
    , ParamEdit.iMOrderBefore = Nothing
    , ParamEdit.iMOrderAfter = Nothing
    , ParamEdit.iDel = Sugar.ParamDelResultDelVar <$ mActions ^. Sugar.npDeleteLambda
    }

makeParamsEdit ::
    Monad m =>
    ExpressionGui.EvalAnnotationOptions -> NearestHoles ->
    Widget.Id -> Widget.Id -> Widget.Id ->
    Sugar.BinderParams (Name m) m ->
    ExprGuiM m [ExpressionGui m]
makeParamsEdit annotationOpts nearestHoles delVarBackwardsId lhsId rhsId params =
    do
        paramColor <- ExprGuiM.readTheme <&> Theme.name <&> Theme.parameterColor
        case params of
            Sugar.BinderWithoutParams -> return []
            Sugar.NullParam p ->
                fromParamList ExprGuiT.showAnnotationWhenVerbose delVarBackwardsId rhsId
                [p & Sugar.fpInfo %~ nullParamEditInfo]
            Sugar.VarParam p ->
                fromParamList ExprGuiT.alwaysShowAnnotations delVarBackwardsId rhsId
                [p & Sugar.fpInfo %~ namedParamEditInfo paramColor]
            Sugar.FieldParams ps ->
                ps ^.. Lens.traversed . _2
                & traverse . Sugar.fpInfo %~ namedParamEditInfo paramColor
                & fromParamList ExprGuiT.alwaysShowAnnotations lhsId rhsId
    where
        fromParamList showParamAnnotation delDestFirst delDestLast paramList =
            do
                jumpHolesEventMap <- ExprEventMap.jumpHolesEventMap nearestHoles
                let mkParam (prevId, nextId, param) =
                        ParamEdit.make annotationOpts showParamAnnotation prevId nextId param
                        <&> E.weakerEvents jumpHolesEventMap
                ExpressionGui.listWithDelDests delDestFirst delDestLast
                    (WidgetIds.fromEntityId . (^. Sugar.fpId)) paramList
                    & traverse mkParam
