module Lamdu.GUI.Expr.GetVarEdit
    ( make, makeSimpleView
    , makePunnedVars
    , Role(..)
    ) where

import qualified Control.Lens as Lens
import           GUI.Momentu (Responsive, EventMap, noMods, Update)
import qualified GUI.Momentu as M
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Font (Underline(..))
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.I18N as MomentuTexts
import qualified GUI.Momentu.ModKey as ModKey
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Options as Options
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Grid as Grid
import qualified GUI.Momentu.Widgets.Label as Label
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Config.Theme.TextColors (TextColors)
import qualified Lamdu.Config.Theme.TextColors as TextColors
import qualified Lamdu.GUI.LightLambda as LightLambda
import           Lamdu.GUI.Monad (GuiM)
import qualified Lamdu.GUI.Monad as GuiM
import qualified Lamdu.GUI.NameView as NameView
import           Lamdu.GUI.Styled (grammar, label)
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.TypeView as TypeView
import qualified Lamdu.GUI.Types as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.GUI.Wrap (stdWrap)
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.Definitions as Texts
import qualified Lamdu.I18N.Navigation as Texts
import           Lamdu.Name (Name(..))
import qualified Lamdu.Name as Name
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

makeSimpleView :: _ => Lens.ALens' TextColors M.Color -> Name -> Widget.Id -> m (M.TextWidget f)
makeSimpleView color name myId =
    (Widget.makeFocusableView ?? myId <&> (Align.tValue %~))
    <*> NameView.make name
    & Styled.withColor (Lens.cloneLens color)

data Role = Normal | Operator deriving Eq

navDoc :: _ => env -> Lens.ALens' (Texts.Navigation Text) Text -> E.Doc
navDoc env lens =
    E.toDoc env [has . MomentuTexts.navigation, has . lens]

makeNameRef ::
    _ =>
    Role ->
    Lens.ALens' TextColors M.Color -> Widget.Id ->
    Sugar.GetVar Name o ->
    GuiM env i o (M.TextWidget o)
makeNameRef role color myId var =
    do
        savePrecursor <- GuiM.mkPrejumpPosSaver
        env <- Lens.view id
        openPane <- GuiM.openPane
        let jumpToDefinitionEventMap =
                E.keysEventMapMovesCursor
                (env ^. has . Config.jumpToDefinitionKeys ++
                 env ^. has . Config.extractKeys)
                (navDoc env Texts.jumpToDef) $
                do
                    savePrecursor
                    case var ^. Sugar.vGotoParam of
                        Just x -> WidgetIds.fromEntityId x & pure
                        Nothing -> var ^. Sugar.vVar & Sugar.GoToDef & openPane <&> WidgetIds.fromEntityId
        let mAddMarker =
                case role of
                Operator
                    | Name.isOperator name -> id
                    | otherwise -> (Label.make "." M./|/)
                Normal
                    | Name.isOperator name -> \x -> Label.make "(" M./|/ x M./|/ Label.make ")"
                    | otherwise -> id
        makeSimpleView color name nameId
            & mAddMarker
            <&> Align.tValue %~ Widget.weakerEvents jumpToDefinitionEventMap
    & local (M.animIdPrefix .~ Widget.toAnimId nameId)
    & GuiState.assignCursor myId nameId
    where
        name = var ^. Sugar.vName
        nameId = Widget.joinId myId ["name"]

makeInlineEventMap :: _ => env -> Sugar.VarInline f -> EventMap (f Update)
makeInlineEventMap env (Sugar.InlineVar inline) =
    inline <&> WidgetIds.fromEntityId
    & E.keysEventMapMovesCursor (env ^. has . Config.inlineKeys)
      (E.toDoc env [has . MomentuTexts.edit, has . Texts.inline])
makeInlineEventMap env (Sugar.CannotInlineDueToUses (x:_)) =
    WidgetIds.fromEntityId x & pure
    & E.keysEventMapMovesCursor (env ^. has . Config.inlineKeys)
      (navDoc env Texts.jumpToNextUse)
makeInlineEventMap _ _ = mempty

definitionTypeChangeBox ::
    _ => Sugar.DefinitionOutdatedType Name f Sugar.EntityId -> Widget.Id -> m (M.TextWidget f)
definitionTypeChangeBox info getVarId =
    do
        env <- Lens.view id
        let updateDoc =
                E.toDoc env
                [has . MomentuTexts.edit, has . Texts.updateDefType]
        oldTypeRow <- Styled.info (label Texts.defUpdateWas)
        newTypeRow <-
            -- TODO: Only actionable when focused with hover?
            Styled.actionable myId Texts.defUpdateHeader
            updateDoc update
            M./|/ Spacer.stdHSpace
            M./|/ Styled.info (label Texts.defUpdateTo)

        oldTypeView <- mkTypeView "oldTypeView" (info ^. Sugar.defTypeWhenUsed)
        newTypeView <- mkTypeView "newTypeView" (info ^. Sugar.defTypeCurrent)

        Grid.make ??
            [ [ Align.fromWithTextPos 0 (oldTypeRow <&> Widget.fromView)
              , Align.fromWithTextPos 0 (oldTypeView <&> Widget.fromView) ]
            , [ Align.fromWithTextPos 0 newTypeRow
              , Align.fromWithTextPos 0 (newTypeView <&> Widget.fromView) ]
            ] <&> snd <&> Align.WithTextPos 0
    where
        update = info ^. Sugar.defTypeUseCurrent <&> WidgetIds.fromEntityId
        mkTypeView idSuffix scheme =
            TypeView.makeScheme scheme & local (M.animIdPrefix .~ animId ++ [idSuffix])
        myId = Widget.joinId getVarId ["type change"]
        animId = Widget.toAnimId myId

processDefinitionWidget ::
    _ => Sugar.DefinitionForm Name f -> Widget.Id -> m (M.TextWidget f) -> m (M.TextWidget f)
processDefinitionWidget Sugar.DefUpToDate _myId mkLayout = mkLayout
processDefinitionWidget Sugar.DefDeleted _myId mkLayout =
    Styled.deletedUse <*> mkLayout
processDefinitionWidget (Sugar.DefTypeChanged info) myId mkLayout =
    do
        env <- Lens.view id
        let showDialogEventMap =
                pure myId
                & E.keysEventMapMovesCursor [noMods ModKey.Key'Enter]
                (E.toDoc env
                    [ has . MomentuTexts.view
                    , has . Texts.typeUpdateDialog
                    , has . Texts.show
                    ])
        let hideDialogEventMap =
                pure hiddenId
                & E.keysEventMapMovesCursor [noMods ModKey.Key'Escape]
                (E.toDoc env
                    [ has . MomentuTexts.view
                    , has . Texts.typeUpdateDialog
                    , has . Texts.hide
                    ])
        let underline = Underline
                { _underlineColor = env ^. has . Theme.errorColor
                , _underlineWidth = env ^. has . Theme.wideUnderlineWidth
                }
        layout <-
            local (TextView.underline ?~ underline) mkLayout
            & GuiState.assignCursor hiddenId myId
        isSelected <- GuiState.isSubCursor ?? myId
        isHidden <- GuiState.isSubCursor ?? hiddenId
        case (isHidden, isSelected) of
            (True, _) ->
                layout
                <&> Widget.strongerEventsWithoutPreevents showDialogEventMap
                & pure
            (False, True) ->
                ( Hover.hoverBeside Align.tValue ?? layout )
                <*>
                ( definitionTypeChangeBox info myId <&> (^. Align.tValue) )
                <&> fmap (Widget.weakerEventsWithoutPreevents hideDialogEventMap)
            (False, False) -> pure layout
    where
        hiddenId = myId `Widget.joinId` ["hidden"]

make ::
    _ =>
    Role ->
    Annotated (ExprGui.Payload i o) # Const (Sugar.GetVar Name o) ->
    GuiM env i o (Responsive o)
make role (Ann (Const pl) (Const var)) =
    do
        env <- Lens.view id
        let (color, processDef) =
                case var ^. Sugar.vForm of
                Sugar.GetDefinition defForm ->
                    ( TextColors.definitionColor
                    , processDefinitionWidget defForm myId
                    )
                _ -> (TextColors.variableColor, id)
        mUnderline <-
            case var ^. Sugar.vForm of
            Sugar.GetLightParam -> Lens.view has <&> LightLambda.underline <&> (TextView.underline ?~)
            _ -> pure id
        makeNameRef role color myId var
            & local mUnderline
            <&> Align.tValue %~ Widget.weakerEvents
                (makeInlineEventMap env (var ^. Sugar.vInline))
            & processDef
    <&> Responsive.fromWithTextPos & stdWrap pl
    where
        myId = WidgetIds.fromExprPayload pl

makePunnedVar ::
    _ =>
    Sugar.PunnedVar Name o # Annotated (ExprGui.Payload i o) ->
    GuiM env i o (Responsive o)
makePunnedVar (Sugar.PunnedVar var tagId) =
    make Normal var
    & GuiState.assignCursor
        (WidgetIds.fromEntityId tagId)
        (WidgetIds.fromExprPayload (var ^. annotation))

makePunnedVars ::
    _ =>
    [Sugar.PunnedVar Name o # Annotated (ExprGui.Payload i o)] ->
    GuiM env i o (Responsive o)
makePunnedVars args =
    do
        argEdits <- traverse makePunnedVar args
        collapsed <- grammar (label Texts.punnedFields) <&> Responsive.fromTextView
        Options.boxSpaced ?? Options.disambiguationNone ?? collapsed : argEdits
