module Lamdu.GUI.ExpressionEdit.GetVarEdit
    ( make, makeGetBinder, makeNoActions, makeSimpleView
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import qualified Data.ByteString.Char8 as SBS8
import           GUI.Momentu.Align (WithTextPos)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Font (Underline(..))
import           GUI.Momentu.Glue ((/|/))
import qualified GUI.Momentu.Hover as Hover
import           GUI.Momentu.MetaKey (MetaKey(..), noMods)
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Options as Options
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Grid as Grid
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Lamdu.Config (Config, HasConfig)
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (HasTheme)
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Config.Theme.TextColors (TextColors)
import qualified Lamdu.Config.Theme.TextColors as TextColors
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExprGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import           Lamdu.GUI.ExpressionGui.Wrap (stdWrap)
import qualified Lamdu.GUI.LightLambda as LightLambda
import qualified Lamdu.GUI.NameView as NameView
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.TypeView as TypeView
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

makeSimpleView ::
    ( MonadReader env m, GuiState.HasCursor env, HasTheme env
    , Applicative f, Element.HasAnimIdPrefix env, TextView.HasStyle env
    ) =>
    Lens.ALens' TextColors Draw.Color -> Name x -> Widget.Id ->
    m (WithTextPos (Widget (f GuiState.Update)))
makeSimpleView color name myId =
    (Widget.makeFocusableView ?? myId <&> (Align.tValue %~))
    <*> NameView.make name
    & Styled.withColor (Lens.cloneLens color)

makeParamsRecord ::
    ( MonadReader env m, HasTheme env, GuiState.HasCursor env
    , Element.HasAnimIdPrefix env, Spacer.HasStdSpacing env
    , Applicative f
    ) =>
    Widget.Id -> Sugar.ParamsRecordVarRef (Name f) -> m (Responsive (f GuiState.Update))
makeParamsRecord myId paramsRecordVar =
    do
        respondToCursor <- Widget.respondToCursorPrefix ?? myId
        sequence
            [ TextView.makeLabel "Params {" <&> Responsive.fromTextView
            , (Options.boxSpaced ?? Options.disambiguationNone)
              <*>
              ( fieldNames
                & Lens.itraverse
                (\i fieldName ->
                    let paramId = ["params", SBS8.pack (show (i :: Int))]
                    in
                    Widget.joinId myId paramId
                    & makeSimpleView TextColors.parameterColor fieldName
                    <&> Responsive.fromWithTextPos
                    & Reader.local (Element.animIdPrefix %~ (<> paramId))
                )
              )
            , TextView.makeLabel "}" <&> Responsive.fromTextView
            ] <&> Options.box Options.disambiguationNone <&> respondToCursor
    where
        Sugar.ParamsRecordVarRef fieldNames = paramsRecordVar

makeNameRef ::
    (Monad i, Monad o) =>
    Lens.ALens' TextColors Draw.Color -> Widget.Id ->
    Sugar.NameRef (Name x) o ->
    ExprGuiM i o (WithTextPos (Widget (o GuiState.Update)))
makeNameRef color myId nameRef =
    do
        savePrecursor <- ExprGuiM.mkPrejumpPosSaver
        config <- Lens.view Config.config
        let jumpToDefinitionEventMap =
                E.keysEventMapMovesCursor
                (config ^. Config.jumpToDefinitionKeys ++
                 config ^. Config.extractKeys)
                (E.Doc ["Navigation", "Jump to definition"]) $
                do
                    savePrecursor
                    nameRef ^. Sugar.nrGotoDefinition <&> WidgetIds.fromEntityId
        makeSimpleView color (nameRef ^. Sugar.nrName) nameId
            <&> Align.tValue %~ Widget.weakerEvents jumpToDefinitionEventMap
    & Reader.local (Element.animIdPrefix .~ Widget.toAnimId nameId)
    & GuiState.assignCursor myId nameId
    where
        nameId = Widget.joinId myId ["name"]

makeInlineEventMap ::
    Applicative f =>
    Config -> Sugar.BinderVarInline f ->
    EventMap (f GuiState.Update)
makeInlineEventMap config (Sugar.InlineVar inline) =
    inline <&> WidgetIds.fromEntityId
    & E.keysEventMapMovesCursor (config ^. Config.inlineKeys)
      (E.Doc ["Edit", "Inline"])
makeInlineEventMap config (Sugar.CannotInlineDueToUses (x:_)) =
    WidgetIds.fromEntityId x & pure
    & E.keysEventMapMovesCursor (config ^. Config.inlineKeys)
      (E.Doc ["Navigation", "Jump to next use"])
makeInlineEventMap _ _ = mempty

definitionTypeChangeBox ::
    ( MonadReader env m
    , Element.HasAnimIdPrefix env
    , Spacer.HasStdSpacing env, HasTheme env, GuiState.HasCursor env
    , HasConfig env, Applicative f
    ) =>
    Sugar.DefinitionOutdatedType (Name x) (f Sugar.EntityId) -> Widget.Id ->
    m (WithTextPos (Widget (f GuiState.Update)))
definitionTypeChangeBox info getVarId =
    do
        updateLabel <- Styled.actionable myId "Update" updateDoc update
        toLabel <- Styled.infoLabel "to: "

        oldTypeRow <- Styled.infoLabel "Type was: "
        hspace <- Spacer.stdHSpace
        let newTypeRow = updateLabel /|/ hspace /|/ toLabel

        oldTypeView <- mkTypeView "oldTypeView" (info ^. Sugar.defTypeWhenUsed)
        newTypeView <- mkTypeView "newTypeView" (info ^. Sugar.defTypeCurrent)

        Grid.make
            [ [ Align.fromWithTextPos 0 (oldTypeRow <&> Widget.fromView)
              , Align.fromWithTextPos 0 (oldTypeView <&> Widget.fromView) ]
            , [ Align.fromWithTextPos 0 newTypeRow
              , Align.fromWithTextPos 0 (newTypeView <&> Widget.fromView) ]
            ] & snd & Align.WithTextPos 0 & pure
    where
        update = info ^. Sugar.defTypeUseCurrent <&> WidgetIds.fromEntityId
        updateDoc = E.Doc ["Edit", "Update definition type"]
        mkTypeView idSuffix scheme =
            TypeView.makeScheme scheme
            & Reader.local (Element.animIdPrefix .~ animId ++ [idSuffix])
        myId = Widget.joinId getVarId ["type change"]
        animId = Widget.toAnimId myId

processDefinitionWidget ::
    ( MonadReader env m, Spacer.HasStdSpacing env
    , HasTheme env, Element.HasAnimIdPrefix env, HasConfig env
    , GuiState.HasCursor env, Hover.HasStyle env
    , Applicative f
    ) =>
    Sugar.DefinitionForm (Name x) f -> Widget.Id ->
    m (WithTextPos (Widget (f GuiState.Update))) ->
    m (WithTextPos (Widget (f GuiState.Update)))
processDefinitionWidget Sugar.DefUpToDate _myId mkLayout = mkLayout
processDefinitionWidget Sugar.DefDeleted _myId mkLayout =
    Styled.deletedUse <*> mkLayout
processDefinitionWidget (Sugar.DefTypeChanged info) myId mkLayout =
    do
        theme <- Lens.view Theme.theme
        let underline = Underline
                { _underlineColor = theme ^. Theme.errorColor
                , _underlineWidth = theme ^. Theme.wideUnderlineWidth
                }
        layout <-
            Reader.local (TextView.underline ?~ underline) mkLayout
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
        showDialogEventMap =
            pure myId
            & E.keysEventMapMovesCursor [MetaKey noMods MetaKey.Key'Enter]
            (E.Doc ["View", "Type update dialog", "Show"])
        hideDialogEventMap =
            pure hiddenId
            & E.keysEventMapMovesCursor [MetaKey noMods MetaKey.Key'Escape]
            (E.Doc ["View", "Type update dialog", "Hide"])
        hiddenId = myId `Widget.joinId` ["hidden"]

makeGetBinder ::
    (Monad i, Monad o) =>
    Sugar.BinderVarRef (Name x) o -> Widget.Id ->
    ExprGuiM i o (WithTextPos (Widget (o GuiState.Update)))
makeGetBinder binderVar myId =
    do
        config <- Lens.view Config.config
        let (color, processDef) =
                case binderVar ^. Sugar.bvForm of
                Sugar.GetLet -> (TextColors.letColor, id)
                Sugar.GetDefinition defForm ->
                    ( TextColors.definitionColor
                    , processDefinitionWidget defForm myId
                    )
        makeNameRef color myId (binderVar ^. Sugar.bvNameRef)
            <&> Align.tValue %~ Widget.weakerEvents
                (makeInlineEventMap config (binderVar ^. Sugar.bvInline))
            & processDef

makeGetParam ::
    (Monad i, Monad o) =>
    Sugar.ParamRef (Name x) o -> Widget.Id ->
    ExprGuiM i o (WithTextPos (Widget (o GuiState.Update)))
makeGetParam param myId =
    do
        theme <- Lens.view Theme.theme
        let mk = makeNameRef TextColors.parameterColor myId (param ^. Sugar.pNameRef)
        case param ^. Sugar.pBinderMode of
            Sugar.LightLambda ->
                mk
                & Reader.local (TextView.underline ?~ LightLambda.underline theme)
                & Styled.nameAtBinder name
            Sugar.NormalBinder -> mk
    where
        name = param ^. Sugar.pNameRef . Sugar.nrName

makeNoActions ::
    (Monad i, Monad o) =>
    Sugar.GetVar (Name o) o ->
    Widget.Id ->
    ExprGuiM i o (ExpressionGui o)
makeNoActions getVar myId =
    case getVar of
    Sugar.GetBinder binderVar ->
        makeGetBinder binderVar myId <&> Responsive.fromWithTextPos
    Sugar.GetParamsRecord paramsRecordVar ->
        makeParamsRecord myId paramsRecordVar
    Sugar.GetParam param ->
        makeGetParam param myId <&> Responsive.fromWithTextPos

make ::
    (Monad i, Monad o) =>
    Sugar.GetVar (Name o) o ->
    Sugar.Payload (Name o) i o ExprGui.Payload ->
    ExprGuiM i o (ExpressionGui o)
make getVar pl =
    stdWrap pl <*> makeNoActions getVar (WidgetIds.fromExprPayload pl)
