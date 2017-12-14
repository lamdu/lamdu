{-# LANGUAGE NamedFieldPuns, NoImplicitPrelude, OverloadedStrings, LambdaCase, FlexibleContexts #-}
module Lamdu.GUI.ExpressionEdit.BinderEdit
    ( make
    , makeBinderBodyEdit
    , addLetEventMap
    , Parts(..), makeParts
    , nonOperatorName
    ) where

import           Control.Applicative ((<|>), liftA2)
import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           Control.Monad.Transaction (transaction)
import qualified Control.Monad.Transaction as Transaction
import           Data.CurAndPrev (CurAndPrev, current, fallbackToPrev)
import           Data.List.Utils (nonEmptyAll, withPrevNext)
import qualified Data.Map as Map
import           Data.Store.Property (Property)
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction, MkProperty(..))
import qualified Data.Text as Text
import           GUI.Momentu.Align (WithTextPos)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/-/), (/|/))
import qualified GUI.Momentu.Glue as Glue
import           GUI.Momentu.MetaKey (MetaKey(..), noMods, toModKey)
import qualified GUI.Momentu.MetaKey as MetaKey
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Options as Options
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.CharClassification as Chars
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Data.Meta as Meta
import qualified Lamdu.GUI.CodeEdit.Settings as CESettings
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import qualified Lamdu.GUI.ExpressionEdit.TagEdit as TagEdit
import qualified Lamdu.GUI.ExpressionGui.Annotation as Annotation
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExprGui
import           Lamdu.GUI.ExpressionGui.Wrap (parentDelegator)
import qualified Lamdu.GUI.NameEdit as NameEdit
import qualified Lamdu.GUI.ParamEdit as ParamEdit
import qualified Lamdu.GUI.PresentationModeEdit as PresentationModeEdit
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name(..))
import qualified Lamdu.Name as Name
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.NearestHoles (NearestHoles)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction

nonOperatorName :: Name.Form -> Bool
nonOperatorName (Name.Stored x _) =
    nonEmptyAll (`notElem` Chars.operator) (Text.unpack x)
nonOperatorName _ = False

makeBinderNameEdit ::
    Monad m =>
    Sugar.BinderActions (T m) ->
    Widget.EventMap (T m GuiState.Update) ->
    Name (T m) -> Draw.Color -> Widget.Id ->
    ExprGuiM m (WithTextPos (Widget (T m GuiState.Update)))
makeBinderNameEdit binderActions rhsJumperEquals name color myId =
    do
        config <- Lens.view Config.config
        NameEdit.makeAtBinder name color myId
            <&> jumpToRHSViaEquals
            <&> Align.tValue %~ E.weakerEvents
                (ParamEdit.eventMapAddFirstParam config
                 (binderActions ^. Sugar.baAddFirstParam))
    where
        jumpToRHSViaEquals
            | nonOperatorName (name ^. Name.form) =
                Align.tValue %~ E.strongerEvents rhsJumperEquals
            | otherwise = id

data Parts m = Parts
    { pMParamsEdit :: Maybe (ExpressionGui m)
    , pMScopesEdit :: Maybe (Widget (T m GuiState.Update))
    , pBodyEdit :: ExpressionGui m
    , pEventMap :: Widget.EventMap (T m GuiState.Update)
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
    Sugar.Binder name m expr -> m (Maybe Sugar.BinderParamScopeId)
readBinderChosenScope binder =
    binder ^. Sugar.bChosenScopeProp <&> Property.value

mkChosenScopeCursor ::
    Monad m =>
    Sugar.Binder (Name (T m)) (T m) (ExprGui.SugarExpr m) ->
    ExprGuiM m (CurAndPrev (Maybe ScopeCursor))
mkChosenScopeCursor binder =
    do
        mOuterScopeId <- ExprGuiM.readMScopeId
        case binder ^. Sugar.bBodyScopes of
            Sugar.SameAsParentScope ->
                mOuterScopeId <&> fmap (trivialScopeCursor . Sugar.BinderParamScopeId) & return
            Sugar.BinderBodyScope binderBodyScope ->
                do
                    mChosenScope <- readBinderChosenScope binder & transaction
                    liftA2 lookupMKey mOuterScopeId binderBodyScope
                        <&> (>>= scopeCursor mChosenScope) & return

makeScopeEventMap ::
    Monad m =>
    [MetaKey] -> [MetaKey] -> ScopeCursor -> (Sugar.BinderParamScopeId -> m ()) ->
    Widget.EventMap (m GuiState.Update)
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

blockEventMap :: Monad m => Widget.EventMap (m GuiState.Update)
blockEventMap =
    return mempty
    & E.keyPresses (dirKeys <&> toModKey)
    (E.Doc ["Navigation", "Move", "(blocked)"])
    where
        dirKeys = [MetaKey.Key'Left, MetaKey.Key'Right] <&> MetaKey noMods

makeScopeNavEdit ::
    Monad m =>
    Sugar.Binder name (T m) expr -> Widget.Id -> ScopeCursor ->
    ExprGuiM m
    ( Widget.EventMap (T m GuiState.Update)
    , Maybe (Widget (T m GuiState.Update))
    )
makeScopeNavEdit binder myId curCursor =
    do
        theme <- Lens.view Theme.theme
        let mkArrow (txt, mScopeId) =
                TextView.makeLabel txt
                <&> (^. Align.tValue)
                & Reader.local
                ( TextView.color .~
                    case mScopeId of
                    Nothing -> Theme.disabledColor theme
                    Just _ -> Theme.grammarColor (Theme.codeForegroundColors theme)
                )
        evalConfig <- Lens.view Config.config <&> Config.eval
        Lens.view (CESettings.settings . CESettings.sInfoMode)
            >>= \case
            CESettings.Evaluation ->
                (Widget.makeFocusableView ?? myId)
                <*> (mapM mkArrow scopes <&> Glue.hbox)
                <&> E.weakerEvents (mkScopeEventMap leftKeys rightKeys `mappend` blockEventMap)
                <&> Just
                <&> (,) (mkScopeEventMap
                         (Config.prevScopeKeys evalConfig)
                         (Config.nextScopeKeys evalConfig))
            _ -> return (mempty, Nothing)
    where
        mkScopeEventMap l r = makeScopeEventMap l r curCursor setScope
        leftKeys = [MetaKey noMods MetaKey.Key'Left]
        rightKeys = [MetaKey noMods MetaKey.Key'Right]
        scopes :: [(Text, Maybe Sugar.BinderParamScopeId)]
        scopes =
            [ ("◀", sMPrevParamScope curCursor)
            , (" ", Nothing)
            , ("▶", sMNextParamScope curCursor)
            ]
        setScope = Transaction.setP (MkProperty (binder ^. Sugar.bChosenScopeProp)) . Just

data IsScopeNavFocused = ScopeNavIsFocused | ScopeNavNotFocused
    deriving (Eq, Ord)

makeMParamsEdit ::
    Monad m =>
    CurAndPrev (Maybe ScopeCursor) -> IsScopeNavFocused ->
    Widget.Id -> Widget.Id ->
    NearestHoles -> Widget.Id -> Sugar.BinderParams (Name (T m)) (T m) ->
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
        <*> (Options.boxSpaced ?? Options.disambiguationNone ?? paramEdits)
        <&> Just
    where
        frame =
            case params of
            Sugar.FieldParams{} -> Styled.addValFrame
            _ -> return id
        mCurCursor =
            do
                ScopeNavIsFocused == isScopeNavFocused & guard
                mScopeCursor ^. current
        annotationMode =
            Annotation.NeighborVals
            (mCurCursor >>= sMPrevParamScope)
            (mCurCursor >>= sMNextParamScope)
            & Annotation.WithNeighbouringEvalAnnotations

binderContentNearestHoles :: Sugar.BinderContent name (T m) (ExprGui.SugarExpr m) -> NearestHoles
binderContentNearestHoles body =
    body ^? Lens.traverse
    & fromMaybe (error "We have at least a body expression inside the binder")
    & ExprGui.nextHolesBefore

makeParts ::
    Monad m =>
    ExprGui.FuncApplyLimit ->
    Sugar.Binder (Name (T m)) (T m) (ExprGui.SugarExpr m) ->
    Widget.Id -> Widget.Id ->
    ExprGuiM m (Parts m)
makeParts funcApplyLimit binder delVarBackwardsId myId =
    do
        mScopeCursor <- mkChosenScopeCursor binder
        let binderScopeId = mScopeCursor <&> Lens.mapped %~ (^. Sugar.bParamScopeId) . sBinderScope
        (scopeEventMap, mScopeNavEdit) <-
            do
                guard (funcApplyLimit == ExprGui.UnlimitedFuncApply)
                scope <- fallbackToPrev mScopeCursor
                guard $
                    Lens.nullOf (Sugar.bParams . Sugar._NullParam) binder ||
                    Lens.has (Lens.traversed . Lens._Just) [sMPrevParamScope scope, sMNextParamScope scope]
                Just scope
            & maybe (return (mempty, Nothing)) (makeScopeNavEdit binder scopesNavId)
        let isScopeNavFocused =
                case mScopeNavEdit of
                Just edit | Widget.isFocused edit -> ScopeNavIsFocused
                _ -> ScopeNavNotFocused
        do
            mParamsEdit <-
                makeMParamsEdit mScopeCursor isScopeNavFocused delVarBackwardsId myId
                (binderContentNearestHoles bodyContent) bodyId params
            rhs <- makeBinderBodyEdit body
            Parts mParamsEdit mScopeNavEdit rhs scopeEventMap & return
            & case mScopeNavEdit of
              Nothing -> GuiState.assignCursorPrefix scopesNavId (const destId)
              Just _ -> id
            & ExprGuiM.withLocalMScopeId binderScopeId
    where
        destId =
            case params of
            Sugar.BinderWithoutParams -> bodyId
            Sugar.NullParam{} -> bodyId
            Sugar.VarParam v -> v ^. Sugar.fpInfo . Sugar.vpiId & WidgetIds.fromEntityId
            Sugar.FieldParams ps ->
                ps ^?! traverse . Sugar.fpInfo . Sugar.fpiTag . Sugar.tagInfo . Sugar.tagInstance & WidgetIds.fromEntityId
        params = binder ^. Sugar.bParams
        body = binder ^. Sugar.bBody
        bodyContent = body ^. Sugar.bbContent
        bodyId = bodyContent ^. SugarLens.binderContentEntityId & WidgetIds.fromEntityId
        scopesNavId = Widget.joinId myId ["scopesNav"]

make ::
    Monad m =>
    Maybe (T m (Property (T m) Meta.PresentationMode)) ->
    Widget.EventMap (T m GuiState.Update) ->
    Name (T m) -> Draw.Color ->
    Sugar.Binder (Name (T m)) (T m) (ExprGui.SugarExpr m) ->
    Widget.Id ->
    ExprGuiM m (ExpressionGui m)
make pMode lhsEventMap name color binder myId =
    do
        Parts mParamsEdit mScopeEdit bodyEdit eventMap <-
            makeParts ExprGui.UnlimitedFuncApply binder myId myId
        rhsJumperEquals <- jumpToRHS bodyId
        mPresentationEdit <-
            pMode & sequenceA & transaction
            >>= traverse
                (PresentationModeEdit.make presentationChoiceId (binder ^. Sugar.bParams))
        jumpHolesEventMap <-
            ExprEventMap.jumpHolesEventMap (binderContentNearestHoles body)
        defNameEdit <-
            makeBinderNameEdit (binder ^. Sugar.bActions) rhsJumperEquals
            name color myId
            <&> (/-/ fromMaybe Element.empty mPresentationEdit)
            <&> Responsive.fromWithTextPos
            <&> E.weakerEvents jumpHolesEventMap
        mParamEdit <-
            case mParamsEdit of
            Nothing -> return Nothing
            Just paramsEdit ->
                Responsive.vboxSpaced
                ?? (paramsEdit : fmap Responsive.fromWidget mScopeEdit ^.. Lens._Just)
                <&> E.strongerEvents rhsJumperEquals
                <&> Just
        equals <- TextView.makeLabel "="
        Options.boxSpaced ?? Options.disambiguationNone
            <&>
            (\hbox ->
            hbox
            [ hbox (defNameEdit : (mParamEdit ^.. Lens._Just) ++ [Responsive.fromTextView equals])
                & E.weakerEvents lhsEventMap
            , bodyEdit
            ] )
            <&> E.weakerEvents eventMap
    & Reader.local (Element.animIdPrefix .~ Widget.toAnimId myId)
    & case binder ^. Sugar.bLamId of
        Nothing -> id
        Just lamId ->
            GuiState.assignCursorPrefix (WidgetIds.fromEntityId lamId) (const bodyId)
    where
        presentationChoiceId = Widget.joinId myId ["presentation"]
        body = binder ^. Sugar.bBody . Sugar.bbContent
        bodyId = body ^. SugarLens.binderContentEntityId & WidgetIds.fromEntityId

makeLetEdit ::
    Monad m =>
    Sugar.Let (Name (T m)) (T m) (ExprGui.SugarExpr m) ->
    ExprGuiM m (ExpressionGui m)
makeLetEdit item =
    do
        config <- Lens.view Config.config
        theme <- Lens.view Theme.theme
        let letColor = Theme.letColor (Theme.name theme)
        let actionsEventMap =
                mconcat
                [ bodyId <$ item ^. Sugar.lActions . Sugar.laSetToInner
                    & Widget.keysEventMapMovesCursor (Config.delKeys config)
                    (E.Doc ["Edit", "Let clause", "Delete"])
                , item ^. Sugar.lActions . Sugar.laFloat
                    <&> Sugar.lfrNewEntity
                    <&> ExprEventMap.extractCursor
                    & Widget.keysEventMapMovesCursor (Config.extractKeys config)
                    (E.Doc ["Edit", "Let clause", "Extract to outer scope"])
                ]
        let usageEventMap =
                mconcat
                [ Widget.keysEventMapMovesCursor (Config.inlineKeys config)
                  (E.Doc ["Navigation", "Jump to first use"])
                  (return (WidgetIds.fromEntityId usage))
                | usage <- take 1 (item ^. Sugar.lUsages)
                ]
        let eventMap = mappend actionsEventMap usageEventMap
        letLabel <- Styled.grammarLabel "let"
        space <- Spacer.stdHSpace
        letEquation <-
            make Nothing mempty (item ^. Sugar.lName) letColor binder letId
            <&> E.weakerEvents eventMap
            <&> Element.pad (Theme.letItemPadding theme <&> realToFrac)
        letLabel /|/ space /|/ letEquation & return
    & Reader.local (Element.animIdPrefix .~ Widget.toAnimId letId)
    where
        bodyId =
            item ^. Sugar.lBody . Sugar.bbContent . SugarLens.binderContentEntityId
            & WidgetIds.fromEntityId
        letId =
            item ^. Sugar.lEntityId & WidgetIds.fromEntityId
            & WidgetIds.letBinderId
        binder = item ^. Sugar.lValue

jumpToRHS ::
    Monad f => Widget.Id -> ExprGuiM f (Widget.EventMap (T f GuiState.Update))
jumpToRHS rhsId =
    ExprGuiM.mkPrejumpPosSaver
    <&> Lens.mapped .~ rhsId
    <&> Widget.keysEventMapMovesCursor [MetaKey noMods MetaKey.Key'Equal]
        (E.Doc ["Navigation", "Jump to Def Body"])

addLetEventMap :: Monad m => T m Sugar.EntityId -> ExprGuiM m (Widget.EventMap (T m GuiState.Update))
addLetEventMap addLet =
    do
        config <- Lens.view Config.config
        savePos <- ExprGuiM.mkPrejumpPosSaver
        savePos >> addLet
            <&> WidgetIds.fromEntityId <&> WidgetIds.letBinderId
            & Widget.keysEventMapMovesCursor (Config.letAddItemKeys config)
                (E.Doc ["Edit", "Let clause", "Add"])
            & pure

makeBinderBodyEdit ::
    Monad m =>
    Sugar.BinderBody (Name (T m)) (T m) (ExprGui.SugarExpr m) ->
    ExprGuiM m (ExpressionGui m)
makeBinderBodyEdit (Sugar.BinderBody addOuterLet content) =
    do
        newLetEventMap <- addLetEventMap addOuterLet
        makeBinderContentEdit content <&> E.weakerEvents newLetEventMap

makeBinderContentEdit ::
    Monad m =>
    Sugar.BinderContent (Name (T m)) (T m) (ExprGui.SugarExpr m) ->
    ExprGuiM m (ExpressionGui m)
makeBinderContentEdit (Sugar.BinderExpr binderBody) =
    ExprGuiM.makeSubexpression binderBody
makeBinderContentEdit (Sugar.BinderLet l) =
    do
        config <- Lens.view Config.config
        let eventMap =
                mconcat
                [ l ^. Sugar.lActions . Sugar.laSetToHole
                    <&> WidgetIds.fromEntityId
                    & Widget.keysEventMapMovesCursor (Config.delKeys config)
                    (E.Doc ["Edit", "Delete let expression"])
                , ExprEventMap.wrapEventMap (l ^. Sugar.lActions . Sugar.laWrap <&> snd) config
                ]
        let moveToInnerEventMap =
                body
                ^? Sugar.bbContent . Sugar._BinderLet
                . Sugar.lActions . Sugar.laFloat
                & maybe mempty
                (Widget.keysEventMap (Config.moveLetInwardKeys config)
                (E.Doc ["Edit", "Let clause", "Move inwards"]) . void)
        mOuterScopeId <- ExprGuiM.readMScopeId
        let letBodyScope = liftA2 lookupMKey mOuterScopeId (l ^. Sugar.lBodyScope)
        parentDelegator letEntityId
            <*> ( Responsive.vboxSpaced
                  <*>
                  sequence
                  [ makeLetEdit l <&> E.weakerEvents moveToInnerEventMap
                  , makeBinderBodyEdit body
                    & ExprGuiM.withLocalMScopeId letBodyScope
                  ]
                )
            <&> E.weakerEvents eventMap
    where
        letEntityId = l ^. Sugar.lEntityId & WidgetIds.fromEntityId
        body = l ^. Sugar.lBody

namedParamEditInfo ::
    Monad m =>
    Widget.Id -> Sugar.FuncParamActions (T m) ->
    WithTextPos (Widget (T m GuiState.Update)) ->
    ParamEdit.Info m
namedParamEditInfo widgetId actions nameEdit =
    ParamEdit.Info
    { ParamEdit.iNameEdit = nameEdit
    , ParamEdit.iMAddNext = actions ^. Sugar.fpAddNext & Just
    , ParamEdit.iMOrderBefore = actions ^. Sugar.fpMOrderBefore
    , ParamEdit.iMOrderAfter = actions ^. Sugar.fpMOrderAfter
    , ParamEdit.iDel = actions ^. Sugar.fpDelete
    , ParamEdit.iId = widgetId
    }

nullParamEditInfo :: Monad m => Widget.Id -> WithTextPos (Widget (T m GuiState.Update)) -> Sugar.NullParamActions (T m) -> ParamEdit.Info m
nullParamEditInfo widgetId nameEdit mActions =
    ParamEdit.Info
    { ParamEdit.iNameEdit = nameEdit
    , ParamEdit.iMAddNext = Nothing
    , ParamEdit.iMOrderBefore = Nothing
    , ParamEdit.iMOrderAfter = Nothing
    , ParamEdit.iDel = Sugar.ParamDelResultDelVar <$ mActions ^. Sugar.npDeleteLambda
    , ParamEdit.iId = widgetId
    }

makeParamsEdit ::
    Monad m =>
    Annotation.EvalAnnotationOptions -> NearestHoles ->
    Widget.Id -> Widget.Id -> Widget.Id ->
    Sugar.BinderParams (Name (T m)) (T m) ->
    ExprGuiM m [ExpressionGui m]
makeParamsEdit annotationOpts nearestHoles delVarBackwardsId lhsId rhsId params =
    do
        paramColor <- Lens.view Theme.theme <&> Theme.name <&> Theme.parameterColor
        case params of
            Sugar.BinderWithoutParams -> return []
            Sugar.NullParam p ->
                do
                    nullParamGui <-
                        (Widget.makeFocusableView ?? nullParamId <&> (Align.tValue %~))
                        <*> Styled.grammarLabel "|"
                    fromParamList delVarBackwardsId rhsId
                        [p & Sugar.fpInfo %~ nullParamEditInfo lhsId nullParamGui]
                where
                    nullParamId = Widget.joinId lhsId ["param"]
            Sugar.VarParam p ->
                p & Sugar.fpInfo %%~ onFpInfo
                <&> (:[])
                >>= fromParamList delVarBackwardsId rhsId
                where
                    onFpInfo x =
                        NameEdit.makeAtBinder (x ^. Sugar.vpiName) paramColor widgetId
                        <&> namedParamEditInfo widgetId (x ^. Sugar.vpiActions)
                        where
                            widgetId = x ^. Sugar.vpiId & WidgetIds.fromEntityId
            Sugar.FieldParams ps ->
                ps
                & traverse . Sugar.fpInfo %%~ onFpInfo
                >>= fromParamList lhsId rhsId
                where
                    onFpInfo x =
                        TagEdit.makeParamTag (x ^. Sugar.fpiTag)
                        <&> namedParamEditInfo widgetId (x ^. Sugar.fpiActions)
                        where
                            widgetId =
                                x ^. Sugar.fpiTag . Sugar.tagInfo . Sugar.tagInstance & WidgetIds.fromEntityId
    where
        fromParamList delDestFirst delDestLast paramList =
            do
                jumpHolesEventMap <- ExprEventMap.jumpHolesEventMap nearestHoles
                let mkParam (prevId, nextId, param) =
                        ParamEdit.make annotationOpts prevId nextId param
                        <&> E.weakerEvents jumpHolesEventMap
                withPrevNext delDestFirst delDestLast
                    (ParamEdit.iId . (^. Sugar.fpInfo)) paramList
                    & traverse mkParam
