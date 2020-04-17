{-# LANGUAGE ConstraintKinds #-}
module Lamdu.GUI.Expr.TagEdit
    ( makeRecordTag, makeVariantTag
    , makeParamTag, addParamId
    , makeArgTag
    , makeTagHoleEdit
    , makeBinderTagEdit
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import qualified Data.Char as Char
import           Data.MRUMemo (memo)
import qualified Data.Property as Property
import qualified Data.Text as Text
import           GUI.Momentu.Align (WithTextPos, TextWidget)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.I18N as MomentuTexts
import qualified GUI.Momentu.MetaKey as MetaKey
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Config.Theme.TextColors (TextColors)
import qualified Lamdu.Config.Theme.TextColors as TextColors
import           Lamdu.Fuzzy (Fuzzy)
import qualified Lamdu.Fuzzy as Fuzzy
import           Lamdu.GUI.ExpressionGui.Monad (GuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as GuiM
import qualified Lamdu.GUI.NameView as NameView
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.TagView as TagView
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.Name as Texts
import qualified Lamdu.I18N.Navigation as Texts
import           Lamdu.Name (Name(..))
import qualified Lamdu.Name as Name
import qualified Lamdu.Style as Style
import           Lamdu.Sugar.EntityId (EntityId)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

makePickEventMap ::
    ( Functor f, Has Config env
    , Has (MomentuTexts.Texts Text) env
    , Has (Texts.CodeUI Text) env
    , MonadReader env m
    ) =>
    f Menu.PickResult ->
    m (EventMap (f GuiState.Update))
makePickEventMap action =
    Lens.view id <&>
    \env ->
    let config = env ^. has . Config.menu
        pickKeys = config ^. Menu.keysPickOption
        jumpNextKeys = config ^. Menu.keysPickOptionAndGotoNext
        mkDoc lens =
            E.toDoc env [has . MomentuTexts.edit, has . Texts.tag, has . lens]
    in
    E.keysEventMapMovesCursor pickKeys (mkDoc Texts.new)
    (action <&> (^. Menu.pickDest))
    -- TODO: DRY with search-menu?
    <> E.keyPresses (jumpNextKeys <&> MetaKey.toModKey)
        (mkDoc Texts.newAndJumpToNextEntry)
        (action <&> \result ->
            case result ^. Menu.pickMNextEntry of
            Just nextEntry -> GuiState.updateCursor nextEntry
            Nothing ->
                GuiState.updateCursor (result ^. Menu.pickDest)
                & GuiState.uPreferStroll .~ (True ^. Lens._Unwrapped)
        )

makeNewTag ::
    (Monad i, Monad o) =>
    Sugar.TagReplace Name i o a ->
    GuiM env i o (Text -> (EntityId -> a -> b) -> o b)
makeNewTag tagRefReplace =
    (,) <$> GuiM.im (tagRefReplace ^. Sugar.tsNewTag) <*> GuiM.assocTagName
    <&> \(tagOpt, assocTagName) searchTerm mkPickResult ->
    do
        Property.setP (assocTagName (tagOpt ^. Sugar.toInfo . Sugar.tagVal)) searchTerm
        tagOpt ^. Sugar.toPick <&> mkPickResult (tagOpt ^. Sugar.toInfo . Sugar.tagInstance)

makeNewTagPreEvent ::
    (Has (Texts.CodeUI Text) env, Monad i, Monad o) =>
    Sugar.TagReplace Name i o a ->
    GuiM env i o (Text -> (EntityId -> a -> r) -> Maybe (Widget.PreEvent (o r)))
makeNewTagPreEvent tagRefReplace =
    (,) <$> Lens.view (has . Texts.newName) <*> makeNewTag tagRefReplace
    <&>
    \(newNameText, newTag) searchTerm mkPickResult ->
    if Text.null searchTerm
    then Nothing
    else
        Just Widget.PreEvent
        { Widget._pDesc = newNameText
        , Widget._pAction = newTag searchTerm mkPickResult
        , Widget._pTextRemainder = ""
        }

makeAddNewTag ::
    ( Monad i, Monad o, MonadReader env f
    , GuiState.HasCursor env, Has Theme env
    , Has TextView.Style env, Element.HasAnimIdPrefix env, Has Dir.Layout env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.CodeUI Text) menv
    ) =>
    Sugar.TagReplace Name i o a ->
    GuiM menv i o
    ( (EntityId -> a -> Menu.PickResult) ->
        SearchMenu.ResultsContext -> Maybe (Menu.Option f o)
    )
makeAddNewTag tagRefReplace =
    makeNewTagPreEvent tagRefReplace <&>
    \newTagPreEvent mkPickResult ctx ->
    let optionId =
            (ctx ^. SearchMenu.rResultIdPrefix) `Widget.joinId` ["Create new"]
        searchTerm = ctx ^. SearchMenu.rSearchTerm
    in  newTagPreEvent searchTerm mkPickResult
        <&> \preEvent ->
        Menu.Option
        { Menu._oId = optionId
        , Menu._oSubmenuWidgets = Menu.SubmenuEmpty
        , Menu._oRender =
            (Widget.makeFocusableView ?? optionId <&> fmap)
            <*> Styled.label Texts.createNew
            <&> (`Menu.RenderedOption` preEvent)
            & Styled.withColor TextColors.actionTextColor
        }

nameText :: Lens.Traversal' (Sugar.TagOption Name m a) Text
nameText = Sugar.toInfo . Sugar.tagName . Name._NameTag . Name.tnDisplayText . Name.ttText

{-# NOINLINE fuzzyMaker #-}
fuzzyMaker :: [(Text, Int)] -> Fuzzy (Set Int)
fuzzyMaker = memo Fuzzy.make

makeOptions ::
    ( Monad i, Monad o, MonadReader menv m
    , GuiState.HasCursor menv, Has Theme menv, Has TextView.Style menv
    , Element.HasAnimIdPrefix menv, Glue.HasTexts menv
    , Has (Texts.Name Text) menv
    , Has (Texts.CodeUI Text) menv
    , Has (Texts.CodeUI Text) env
    , Has (MomentuTexts.Texts Text) env
    ) =>
    Sugar.TagReplace Name i o a ->
    (EntityId -> a -> Menu.PickResult) ->
    SearchMenu.ResultsContext ->
    GuiM env i o (Menu.OptionList (Menu.Option m o))
makeOptions tagRefReplace mkPickResult ctx
    | Text.null searchTerm = pure Menu.TooMany
    | otherwise =
        do
            resultCount <-
                Lens.view
                (has . Config.completion . Config.completionResultCount)
            results <-
                tagRefReplace ^. Sugar.tsOptions
                <&> concatMap withText
                <&> (Fuzzy.memoableMake fuzzyMaker ?? searchTerm)
                & GuiM.im
            let nonFuzzyResults =
                    results ^? Lens.ix 0 . _1 . Fuzzy.isFuzzy
                    & any not
            addNewTag <- makeAddNewTag tagRefReplace
            let maybeAddNewTagOption
                    | nonFuzzyResults || not (Name.isValidText searchTerm) = id
                    | otherwise =
                        maybe id (:) (addNewTag mkPickResult ctx)
            chooseText <- Lens.view (has . MomentuTexts.choose)
            let makeOption opt =
                    Menu.Option
                    { Menu._oId = optionWId
                    , Menu._oRender =
                        (Widget.makeFocusableView ?? optionWId <&> fmap)
                        <*> NameView.make (opt ^. Sugar.toInfo . Sugar.tagName)
                        & Reader.local (Element.animIdPrefix .~ Widget.toAnimId instanceId)
                        <&>
                        \widget ->
                        Menu.RenderedOption
                        { Menu._rWidget = widget
                        , Menu._rPick = Widget.PreEvent
                            { Widget._pDesc = chooseText
                            , Widget._pAction =
                                opt ^. Sugar.toPick
                                <&> mkPickResult
                                (opt ^. Sugar.toInfo . Sugar.tagInstance)
                            , Widget._pTextRemainder = ""
                            }
                        }
                    , Menu._oSubmenuWidgets = Menu.SubmenuEmpty
                    }
                    where
                        instanceId =
                            opt ^. Sugar.toInfo . Sugar.tagInstance
                            & WidgetIds.fromEntityId
                        optionWId =
                            ctx ^. SearchMenu.rResultIdPrefix <> instanceId
            results <&> snd
                & splitAt resultCount
                & _2 %~ not . null
                & _1 %~ maybeAddNewTagOption . map makeOption
                & uncurry Menu.toOptionList
                & pure
    where
        withText tagOption = tagOption ^.. nameText <&> ((,) ?? tagOption)
        searchTerm = ctx ^. SearchMenu.rSearchTerm

allowedSearchTerm :: Text -> Bool
allowedSearchTerm = Name.isValidText

makeHoleSearchTerm ::
    ( Monad i, Monad o
    , TextEdit.HasTexts env
    , SearchMenu.HasTexts env
    , Has (Texts.CodeUI Text) env
    ) =>
    Sugar.TagReplace Name i o a ->
    (EntityId -> a -> Menu.PickResult) -> Widget.Id ->
    GuiM env i o (SearchMenu.Term o)
makeHoleSearchTerm tagRefReplace mkPickResult holeId =
    do
        searchTerm <- SearchMenu.readSearchTerm holeId
        let allowNewTag = Name.isValidText searchTerm
        newTag <- makeNewTag tagRefReplace
        newTagEventMap <-
            if allowNewTag
            then newTag searchTerm mkPickResult & makePickEventMap
            else pure mempty
        newTagPreEvent <- makeNewTagPreEvent tagRefReplace
        let newTagPreEvents =
                newTagPreEvent searchTerm mkPickResult
                ^.. Lens._Just
                <&> fmap (mempty <$)
        let addPreEvents =
                Widget.wState . Widget._StateFocused . Lens.mapped .
                Widget.fPreEvents %~ (Widget.PreEvents newTagPreEvents <>)
        term <-
            SearchMenu.addDelSearchTerm holeId
            <*> SearchMenu.basicSearchTermEdit holeId (pure . allowedSearchTerm)
                SearchMenu.defaultEmptyStrings
            <&> SearchMenu.termWidget . Align.tValue %~
                addPreEvents . Widget.weakerEvents newTagEventMap
        tooltip <- Lens.view (has . Theme.tooltip)
        if  allowNewTag &&
            Widget.isFocused (term ^. SearchMenu.termWidget . Align.tValue)
            then
                do
                    newText <- Lens.view (has . Texts.new)
                    newTagLabel <-
                        (TextView.make ?? ("(" <> newText <> ")"))
                            <*> (Element.subAnimId ?? ["label"])
                    space <- Spacer.stdHSpace
                    hover <- Hover.hover
                    Glue.Poly (|||) <- Glue.mkPoly ?? Glue.Horizontal
                    anchor <- Hover.anchor <&> fmap
                    let hNewTagLabel = hover newTagLabel & Hover.sequenceHover
                    let termWithHover termW =
                            let hoverOptions =
                                    [ anchor (termW ||| space) ||| hNewTagLabel
                                    , hNewTagLabel ||| anchor (space ||| termW)
                                    ] <&> (^. Align.tValue)
                            in  anchor termW
                                <&> Hover.hoverInPlaceOf hoverOptions
                    term & SearchMenu.termWidget %~ termWithHover & pure
                    & Reader.local (Hover.backgroundColor .~ tooltip ^. Theme.tooltipBgColor)
                    & Reader.local (TextView.color .~ tooltip ^. Theme.tooltipFgColor)
                    & Reader.local (Element.animIdPrefix <>~ ["label"])
            else pure term

makeTagHoleEdit ::
    ( Monad i, Monad o
    , Has (Texts.Name Text) env
    , Has (Texts.CodeUI Text) env
    , Glue.HasTexts env
    , TextEdit.HasTexts env
    , SearchMenu.HasTexts env
    ) =>
    Sugar.TagReplace Name i o a ->
    (EntityId -> a -> Menu.PickResult) ->
    Widget.Id ->
    GuiM env i o (TextWidget o)
makeTagHoleEdit tagRefReplace mkPickResult holeId =
    SearchMenu.make
    (const (makeHoleSearchTerm tagRefReplace mkPickResult holeId))
    (makeOptions tagRefReplace mkPickResult) Element.empty holeId
    ?? Menu.AnyPlace

makeTagRefEdit ::
    ( Monad i, Monad o
    , Glue.HasTexts env
    , TextEdit.HasTexts env, SearchMenu.HasTexts env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Navigation Text) env
    ) =>
    Sugar.TagRef Name i o ->
    GuiM env i o (TextWidget o)
makeTagRefEdit = makeTagRefEditWith id (const Nothing) <&> fmap snd

data TagRefEditType
    = TagHole
    | SimpleView
    deriving (Eq)

makeTagRefEditWith ::
    ( Monad i, Monad o, MonadReader nenv n
    , GuiState.HasCursor nenv, Has TextView.Style nenv, Has (Texts.Name Text) nenv
    , Element.HasAnimIdPrefix nenv, Has Theme nenv, Glue.HasTexts nenv
    , Glue.HasTexts env, TextEdit.HasTexts env, SearchMenu.HasTexts env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Navigation Text) env
    , Has (Texts.Name Text) env
    ) =>
    (n (TextWidget o) ->
     GuiM env i o (TextWidget o)) ->
    (Sugar.EntityId -> Maybe Widget.Id) ->
    Sugar.TagRef Name i o ->
    GuiM env i o (TagRefEditType, TextWidget o)
makeTagRefEditWith onView onPickNext tag =
    do
        isHole <- GuiState.isSubCursor ?? holeId
        env <- Lens.view id
        let jumpToTagEventMap jump =
                jump <&> WidgetIds.fromEntityId
                & E.keysEventMapMovesCursor
                (env ^. has . Config.jumpToDefinitionKeys)
                (E.toDoc env
                    [ has . MomentuTexts.edit
                    , has . Texts.tag
                    , has . Texts.jumpToTag
                    ])
        let chooseNewTagEventMap =
                E.keysEventMapMovesCursor
                (Config.delKeys env <> env ^. has . Config.jumpToDefinitionKeys)
                ( E.toDoc env
                    [ has . MomentuTexts.edit
                    , has . Texts.tag
                    , has . MomentuTexts.choose
                    ] ) chooseAction
        let eventMap =
                foldMap jumpToTagEventMap (tag ^. Sugar.tagRefJumpTo)
                <> chooseNewTagEventMap
        nameView <-
            (Widget.makeFocusableView ?? viewId <&> fmap) <*>
            TagView.make info
            <&> Lens.mapped %~ Widget.weakerEvents eventMap
            & onView
        let leaveHoleEventMap =
                E.keysEventMapMovesCursor
                (env ^. has . Config.completion . Config.completionCloseKeys)
                (E.toDoc env
                    [ has . MomentuTexts.navigation
                    , has . Texts.closeHole
                    ])
                (pure myId)
        if isHole
            then
                makeTagHoleEdit (tag ^. Sugar.tagRefReplace) mkPickResult holeId
                <&> Align.tValue %~ Widget.weakerEvents leaveHoleEventMap
                <&> (,) TagHole
            else pure (SimpleView, nameView)
        & GuiState.assignCursor myId viewId
    where
        info = tag ^. Sugar.tagRefTag
        myId = info ^. Sugar.tagInstance & WidgetIds.fromEntityId
        holeId = WidgetIds.tagHoleId myId
        viewId = Widget.joinId myId ["view"]
        mkPickResult tagInstance () =
            Menu.PickResult
            { Menu._pickDest = WidgetIds.fromEntityId tagInstance
            , Menu._pickMNextEntry = onPickNext tagInstance
            }
        chooseAction =
            case tag ^. Sugar.tagRefReplace . Sugar.tsAnon of
            Nothing -> pure myId
            Just setAnon -> setAnon <&> fst <&> WidgetIds.fromEntityId
            <&> WidgetIds.tagHoleId

makeRecordTag ::
    ( Monad i, Monad o
    , Glue.HasTexts env
    , TextEdit.HasTexts env, SearchMenu.HasTexts env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Navigation Text) env
    ) =>
    Sugar.TagRef Name i o ->
    GuiM env i o (TextWidget o)
makeRecordTag =
    makeTagRefEdit <&> Styled.withColor TextColors.recordTagColor

makeVariantTag ::
    ( Monad i, Monad o
    , Glue.HasTexts env
    , TextEdit.HasTexts env, SearchMenu.HasTexts env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Navigation Text) env
    ) =>
    Sugar.TagRef Name i o ->
    GuiM env i o (TextWidget o)
makeVariantTag tag =
    makeTagRefEdit tag
    & Styled.withColor TextColors.caseTagColor

addParamId :: Widget.Id -> Widget.Id
addParamId = (`Widget.joinId` ["add param"])

makeLHSTag ::
    ( Monad i, Monad o
    , Glue.HasTexts env, TextEdit.HasTexts env, SearchMenu.HasTexts env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Navigation Text) env
    ) =>
    (Sugar.EntityId -> Maybe Widget.Id) ->
    Lens.ALens' TextColors Draw.Color -> Sugar.TagRef Name i o ->
    GuiM env i o (TextWidget o)
makeLHSTag onPickNext color tag =
    do
        env <- Lens.view id
        (tagEditType, tagEdit) <-
            makeTagRefEditWith onView onPickNext tag
            & Styled.withColor color
            & Reader.local (has .~ env ^. has . Style.nameAtBinder)
        let chooseEventMap =
                E.charEventMap "Letter"
                (E.toDoc env
                    [has . MomentuTexts.edit, has . Texts.tag, has . MomentuTexts.choose])
                chooseWithChar

        let eventMap =
                case tagEditType of
                SimpleView -> chooseEventMap
                _ -> mempty
        tagEdit <&> Widget.weakerEvents eventMap & pure
    where
        chooseWithChar c =
            SearchMenu.enterWithSearchTerm (Text.singleton c)
            (WidgetIds.tagHoleId myId)
            <$ guard (Char.isAlpha c)
            <&> pure
        myId = tag ^. Sugar.tagRefTag . Sugar.tagInstance & WidgetIds.fromEntityId
        -- Apply the name style only when the tag is a view. If it is
        -- a tag hole, the name style (indicating auto-name) makes no sense
        onView =
            Styled.nameAtBinder (tag ^. Sugar.tagRefTag . Sugar.tagName) .
            Styled.withColor color

makeParamTag ::
    ( Monad i, Monad o
    , Glue.HasTexts env, TextEdit.HasTexts env, SearchMenu.HasTexts env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Navigation Text) env
    ) =>
    Sugar.TagRef Name i o ->
    GuiM env i o (TextWidget o)
makeParamTag =
    makeLHSTag onPickNext TextColors.parameterColor
    where
        onPickNext pos = WidgetIds.fromEntityId pos & addParamId & Just

-- | Unfocusable tag view (e.g: in apply args)
makeArgTag ::
    ( MonadReader env m, Has Theme env, Has TextView.Style env
    , Element.HasAnimIdPrefix env, Glue.HasTexts env, Has (Texts.Name Text) env
    ) =>
    Name -> Sugar.EntityId -> m (WithTextPos View)
makeArgTag name tagInstance =
    NameView.make name
    & Styled.withColor TextColors.argTagColor
    & Reader.local (Element.animIdPrefix .~ animId)
    where
        animId = WidgetIds.fromEntityId tagInstance & Widget.toAnimId

makeBinderTagEdit ::
    ( Monad i, Monad o
    , Glue.HasTexts env
    , TextEdit.HasTexts env
    , SearchMenu.HasTexts env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Navigation Text) env
    ) =>
    Lens.ALens' TextColors Draw.Color -> Sugar.TagRef Name i o ->
    GuiM env i o (TextWidget o)
makeBinderTagEdit color tag =
    makeLHSTag (const Nothing) color tag
    & Reader.local (has . Menu.configKeys . Menu.keysPickOptionAndGotoNext .~ [])
