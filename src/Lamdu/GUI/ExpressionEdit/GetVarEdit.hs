{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.GetVarEdit
    ( make, makeGetBinder, makeNoActions
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import qualified Data.ByteString.Char8 as SBS8
import           GUI.Momentu.Align (WithTextPos)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Font (Underline(..))
import           GUI.Momentu.Glue ((/|/))
import qualified GUI.Momentu.Hover as Hover
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
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

makeSimpleView ::
    ( MonadReader env m, GuiState.HasCursor env, HasTheme env
    , Applicative f, Element.HasAnimIdPrefix env, TextView.HasStyle env
    ) =>
    Name x -> Widget.Id ->
    m (WithTextPos (Widget (f GuiState.Update)))
makeSimpleView name myId =
    (Widget.makeFocusableView ?? myId <&> (Align.tValue %~))
    <*> NameView.make name

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
                    & makeSimpleView fieldName <&> Responsive.fromWithTextPos
                    & Styled.withColor TextColors.parameterColor
                    & Reader.local (Element.animIdPrefix %~ (<> paramId))
                )
              )
            , TextView.makeLabel "}" <&> Responsive.fromTextView
            ] <&> Options.box Options.disambiguationNone <&> respondToCursor
    where
        Sugar.ParamsRecordVarRef fieldNames = paramsRecordVar

makeNameRef ::
    Monad m =>
    Widget.Id -> Sugar.NameRef name (T m) ->
    (name -> Widget.Id -> ExprGuiM m (WithTextPos (Widget (T m GuiState.Update)))) ->
    ExprGuiM m (WithTextPos (Widget (T m GuiState.Update)))
makeNameRef myId nameRef maker =
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
        maker (nameRef ^. Sugar.nrName) nameId
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
    Sugar.DefinitionOutdatedType (Name g) (f Sugar.EntityId) -> Widget.Id ->
    m (WithTextPos (Widget (f GuiState.Update)))
definitionTypeChangeBox info getVarId =
    do
        infoColor <-
            Lens.view (Theme.theme . Theme.textColors . TextColors.infoTextColor)
        let infoLabel text =
                TextView.makeLabel text & Reader.local (TextView.color .~ infoColor)
        updateLabel <- Styled.actionable myId "Update" updateDoc update
        toLabel <- infoLabel "to: "

        oldTypeRow <- infoLabel "Type was: "
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
    Sugar.DefinitionForm (Name g) f -> Widget.Id ->
    m (WithTextPos (Widget (f GuiState.Update))) ->
    m (WithTextPos (Widget (f GuiState.Update)))
processDefinitionWidget Sugar.DefUpToDate _myId mkLayout = mkLayout
processDefinitionWidget Sugar.DefDeleted _myId mkLayout =
    (Styled.addDeletionDiagonal ?? 0.1)
    <*> mkLayout
processDefinitionWidget (Sugar.DefTypeChanged info) myId mkLayout =
    do
        theme <- Lens.view Theme.theme
        let underline = Underline
                { _underlineColor = theme ^. Theme.typeIndicatorErrorColor
                , _underlineWidth = theme ^. Theme.wideUnderlineWidth
                }
        layout <- Reader.local (TextView.underline ?~ underline) mkLayout
        isSelected <- GuiState.isSubCursor ?? myId
        if isSelected
            then
                ( Hover.hoverBeside Align.tValue ?? layout )
                <*>
                ( definitionTypeChangeBox info myId <&> (^. Align.tValue) )
            else pure layout

makeGetBinder ::
    Monad m =>
    Sugar.BinderVarRef (Name (T m)) (T m) -> Widget.Id ->
    ExprGuiM m (WithTextPos (Widget (T m GuiState.Update)))
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
        makeSimpleView
            <&> Lens.mapped %~ Styled.withColor (Lens.cloneLens color)
            & makeNameRef myId (binderVar ^. Sugar.bvNameRef)
            <&> Align.tValue %~ Widget.weakerEvents
                (makeInlineEventMap config (binderVar ^. Sugar.bvInline))
            & processDef

makeGetParam ::
    Monad m =>
    Sugar.ParamRef (Name (T m)) (T m) -> Widget.Id ->
    ExprGuiM m (WithTextPos (Widget (T m GuiState.Update)))
makeGetParam param myId =
    do
        theme <- Lens.view Theme.theme
        case param ^. Sugar.pBinderMode of
            Sugar.LightLambda ->
                makeSimpleView
                <&> Lens.mapped %~ Reader.local (TextView.underline ?~ LightLambda.underline theme)
                <&> Lens.mapped %~ Styled.nameAtBinder TextColors.parameterColor name
            _ ->
                makeSimpleView
            <&> Lens.mapped %~ Styled.withColor TextColors.parameterColor
            & makeNameRef myId (param ^. Sugar.pNameRef)
    where
        name = param ^. Sugar.pNameRef . Sugar.nrName

makeNoActions ::
    Monad m =>
    Sugar.GetVar (Name (T m)) (T m) ->
    Widget.Id ->
    ExprGuiM m (ExpressionGui (T m))
makeNoActions getVar myId =
    case getVar of
    Sugar.GetBinder binderVar ->
        makeGetBinder binderVar myId <&> Responsive.fromWithTextPos
    Sugar.GetParamsRecord paramsRecordVar ->
        makeParamsRecord myId paramsRecordVar
    Sugar.GetParam param ->
        makeGetParam param myId <&> Responsive.fromWithTextPos

make ::
    Monad m =>
    Sugar.GetVar (Name (T m)) (T m) ->
    Sugar.Payload (Name g) (T m) ExprGui.Payload ->
    ExprGuiM m (ExpressionGui (T m))
make getVar pl =
    stdWrap pl <*> makeNoActions getVar (WidgetIds.fromExprPayload pl)
