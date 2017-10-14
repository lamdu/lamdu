{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.GetVarEdit
    ( make, makeGetBinder, makeGetParam
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           Control.Monad.Transaction (MonadTransaction)
import qualified Data.ByteString.Char8 as SBS8
import           Data.Store.Transaction (Transaction)
import           GUI.Momentu.Align (WithTextPos)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Font (Underline(..))
import           GUI.Momentu.Glue ((/-/))
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Options as Options
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Lamdu.Calc.Type.Scheme (schemeType)
import           Lamdu.Config (Config, HasConfig)
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (HasTheme)
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Data.Ops as DataOps
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.LightLambda as LightLambda
import qualified Lamdu.GUI.NameEdit as NameEdit
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.TypeView as TypeView
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction

makeSimpleView ::
    ( MonadReader env m, Widget.HasCursor env, HasTheme env
    , Applicative f, Element.HasAnimIdPrefix env, TextView.HasStyle env
    ) =>
    Name x -> Widget.Id ->
    m (WithTextPos (Widget (f Widget.EventResult)))
makeSimpleView (Name name _) myId =
    (Widget.makeFocusableView ?? myId <&> (Align.tValue %~))
    <*> NameEdit.makeView name (Widget.toAnimId myId)

makeParamsRecord ::
    ( Monad m, MonadReader env f, HasTheme env, Widget.HasCursor env
    , TextView.HasStyle env, Element.HasAnimIdPrefix env
    , Spacer.HasStdSpacing env
    ) =>
    Widget.Id -> Sugar.ParamsRecordVarRef (Name (T m)) -> f (ExpressionGui m)
makeParamsRecord myId paramsRecordVar =
    do
        nameTheme <- Lens.view Theme.theme <&> Theme.name
        respondToCursor <- Widget.respondToCursorPrefix ?? myId
        sequence
            [ TextView.makeLabel "Params {" <&> Responsive.fromTextView
            , (Options.boxSpaced ?? Options.disambiguationNone)
              <*>
              ( fieldNames
                & Lens.itraverse
                (\i fieldName ->
                    Widget.joinId myId ["params", SBS8.pack (show (i::Int))]
                    & makeSimpleView fieldName <&> Responsive.fromWithTextPos
                    & Reader.local (TextView.color .~ Theme.parameterColor nameTheme)
                )
              )
            , TextView.makeLabel "}" <&> Responsive.fromTextView
            ] <&> Options.box Options.disambiguationNone <&> respondToCursor
    where
        Sugar.ParamsRecordVarRef fieldNames = paramsRecordVar

makeNameRef ::
    Monad m =>
    Widget.Id -> Sugar.NameRef name (T m) ->
    (name -> Widget.Id -> ExprGuiM m (WithTextPos (Widget (T m Widget.EventResult)))) ->
    ExprGuiM m (WithTextPos (Widget (T m Widget.EventResult)))
makeNameRef myId nameRef maker =
    do
        cp <- ExprGuiM.readCodeAnchors
        config <- Lens.view Config.config
        let jumpToDefinitionEventMap =
                Widget.keysEventMapMovesCursor
                (Config.jumpToDefinitionKeys config ++ Config.extractKeys config)
                (E.Doc ["Navigation", "Jump to definition"]) $
                do
                    DataOps.savePreJumpPosition cp myId
                    nameRef ^. Sugar.nrGotoDefinition <&> WidgetIds.fromEntityId
        maker (nameRef ^. Sugar.nrName) nameId
            <&> Align.tValue %~ E.weakerEvents jumpToDefinitionEventMap
    & Widget.assignCursor myId nameId
    where
        nameId = Widget.joinId myId ["name"]

makeInlineEventMap ::
    Monad m =>
    Config -> Sugar.BinderVarInline (T m) ->
    Widget.EventMap (T m Widget.EventResult)
makeInlineEventMap config (Sugar.InlineVar inline) =
    inline <&> WidgetIds.fromEntityId
    & Widget.keysEventMapMovesCursor (Config.inlineKeys config)
      (E.Doc ["Edit", "Inline"])
makeInlineEventMap config (Sugar.CannotInlineDueToUses (x:_)) =
    WidgetIds.fromEntityId x & return
    & Widget.keysEventMapMovesCursor (Config.inlineKeys config)
      (E.Doc ["Navigation", "Jump to next use"])
makeInlineEventMap _ _ = mempty

definitionTypeChangeBox ::
    ( Monad m, MonadReader env f, MonadTransaction n f
    , Element.HasAnimIdPrefix env, TextView.HasStyle env
    , Spacer.HasStdSpacing env, HasTheme env, Widget.HasCursor env
    , HasConfig env
    ) =>
    Sugar.DefinitionOutdatedType (T m) -> Widget.Id ->
    f (WithTextPos (Widget (T m Widget.EventResult)))
definitionTypeChangeBox info getVarId =
    do
        headerLabel <- TextView.makeLabel "Type was:"
        typeWhenUsed <-
            mkTypeView "typeWhenUsed" (info ^. Sugar.defTypeWhenUsed)
        spacing <- Spacer.stdVSpace
        sepLabel <-
            (Widget.makeFocusableView ?? myId <&> (Align.tValue %~))
            <*> TextView.makeLabel "Update to:"
        typeCurrent <- mkTypeView "typeCurrent" (info ^. Sugar.defTypeCurrent)
        config <- Lens.view Config.config
        -- TODO: unify config's button press keys
        let keys = Config.newDefinitionButtonPressKeys (Config.pane config)
        let update = (info ^. Sugar.defTypeUseCurrent) >> return getVarId
        headerLabel /-/ typeWhenUsed /-/ spacing /-/ sepLabel /-/ typeCurrent
            & Align.tValue %~ E.weakerEvents
            (Widget.keysEventMapMovesCursor keys
                (E.Doc ["Edit", "Update definition type"]) update)
            & pure
    where
        mkTypeView idSuffix scheme =
            TypeView.make (scheme ^. schemeType)
            & Reader.local (Element.animIdPrefix .~ animId ++ [idSuffix])
        myId = Widget.joinId getVarId ["type change"]
        animId = Widget.toAnimId myId

processDefinitionWidget ::
    ( Monad m, MonadReader env f, MonadTransaction n f, Spacer.HasStdSpacing env
    , HasTheme env, Element.HasAnimIdPrefix env, HasConfig env
    , TextView.HasStyle env, Widget.HasCursor env, Hover.HasStyle env
    ) =>
    Sugar.DefinitionForm (T m) -> Widget.Id ->
    f (WithTextPos (Widget (T m Widget.EventResult))) ->
    f (WithTextPos (Widget (T m Widget.EventResult)))
processDefinitionWidget Sugar.DefUpToDate _myId mkLayout = mkLayout
processDefinitionWidget Sugar.DefDeleted _myId mkLayout =
    (Styled.addDeletionDiagonal ?? 0.1)
    <*> mkLayout
processDefinitionWidget (Sugar.DefTypeChanged info) myId mkLayout =
    do
        theme <- Lens.view Theme.theme
        layout <-
            ExprGuiM.withLocalUnderline Underline
                { _underlineColor = Theme.typeIndicatorErrorColor theme
                , _underlineWidth = Theme.underlineWidth theme
                }
            mkLayout
        isSelected <- Widget.isSubCursor ?? myId
        if isSelected
            then
                ( Hover.hoverBeside Align.tValue ?? layout )
                <*>
                ( definitionTypeChangeBox info myId <&> (^. Align.tValue) )
            else return layout

makeGetBinder ::
    Monad m =>
    Sugar.BinderVarRef (Name (T m)) (T m) -> Widget.Id ->
    ExprGuiM m (WithTextPos (Widget (T m Widget.EventResult)))
makeGetBinder binderVar myId =
    do
        config <- Lens.view Config.config
        nameTheme <- Lens.view Theme.theme <&> Theme.name
        let (color, processDef) =
                case binderVar ^. Sugar.bvForm of
                Sugar.GetLet -> (Theme.letColor nameTheme, id)
                Sugar.GetDefinition defForm ->
                    ( Theme.definitionColor nameTheme
                    , processDefinitionWidget defForm myId
                    )
        makeSimpleView
            <&> Lens.mapped %~ Reader.local (TextView.color .~ color)
            & makeNameRef myId (binderVar ^. Sugar.bvNameRef)
            <&> Align.tValue %~ E.weakerEvents
                (makeInlineEventMap config (binderVar ^. Sugar.bvInline))
            & processDef

makeGetParam ::
    Monad m =>
    Sugar.ParamRef (Name (T m)) (T m) -> Widget.Id ->
    ExprGuiM m (WithTextPos (Widget (T m Widget.EventResult)))
makeGetParam param myId =
    do
        theme <- Lens.view Theme.theme
        let paramColor = Theme.name theme & Theme.parameterColor
        case param ^. Sugar.pBinderMode of
            Sugar.LightLambda ->
                makeSimpleView
                <&> Lens.mapped %~ LightLambda.withUnderline theme
                <&> Lens.mapped %~ NameEdit.styleNameAtBinder name paramColor
            _ ->
                makeSimpleView
                <&> Lens.mapped %~ Reader.local (TextView.color .~ paramColor)
            & makeNameRef myId (param ^. Sugar.pNameRef)
    where
        name = param ^. Sugar.pNameRef . Sugar.nrName

make ::
    Monad m =>
    Sugar.GetVar (Name (T m)) (T m) ->
    Sugar.Payload (T m) ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
make getVar pl =
    case getVar of
    Sugar.GetBinder binderVar ->
        makeGetBinder binderVar myId <&> Responsive.fromWithTextPos
    Sugar.GetParamsRecord paramsRecordVar ->
        makeParamsRecord myId paramsRecordVar
    Sugar.GetParam param ->
        makeGetParam param myId <&> Responsive.fromWithTextPos
    & ExpressionGui.stdWrap pl
    where
        myId = WidgetIds.fromExprPayload pl
