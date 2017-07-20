{-# LANGUAGE NoImplicitPrelude, RecordWildCards, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.GetVarEdit
    ( make, makeGetBinder, makeGetParam
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import qualified Data.ByteString.Char8 as SBS8
import           Data.Store.Transaction (Transaction)
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.Font (Underline(..))
import           Graphics.UI.Bottle.View ((/-/))
import qualified Graphics.UI.Bottle.View as View
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import           Graphics.UI.Bottle.Align (WithTextPos)
import qualified Graphics.UI.Bottle.Align as Align
import qualified Graphics.UI.Bottle.Widget.TreeLayout as TreeLayout
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import           Lamdu.Calc.Type.Scheme (schemeType)
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Data.Ops as DataOps
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.Hover as Hover
import qualified Lamdu.GUI.LightLambda as LightLambda
import qualified Lamdu.GUI.TypeView as TypeView
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.Names.Types (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction

makeSimpleView ::
    (Applicative f, Monad m) =>
    Name m -> Widget.Id ->
    ExprGuiM m (WithTextPos (Widget (f Widget.EventResult)))
makeSimpleView name myId =
    (Widget.makeFocusableView ?? myId <&> (Align.tValue %~))
    <*> ( ExpressionGui.makeNameView name (Widget.toAnimId myId)
          <&> Align.tValue %~ Widget.fromView
        )

makeParamsRecord ::
    Monad m => Widget.Id -> Sugar.ParamsRecordVar (Name m) ->
    ExprGuiM m (ExpressionGui m)
makeParamsRecord myId paramsRecordVar =
    do
        theme <- Lens.view Theme.theme
        let Theme.Name{..} = Theme.name theme
        sequence
            [ TextView.makeLabel "Params {" <&> TreeLayout.fromTextView
            , ExpressionGui.combineSpaced
              <*>
              ( fieldNames
                & Lens.itraverse
                (\i fieldName ->
                    Widget.joinId myId ["params", SBS8.pack (show (i::Int))]
                    & makeSimpleView fieldName <&> TreeLayout.fromWithTextPos
                    & Reader.local (TextView.color .~ parameterColor)
                )
              )
            , TextView.makeLabel "}" <&> TreeLayout.fromTextView
            ] <&> ExpressionGui.combine
    where
        Sugar.ParamsRecordVar fieldNames = paramsRecordVar

makeNameRef ::
    Monad m =>
    Widget.Id -> Sugar.NameRef name m ->
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
                    WidgetIds.fromEntityId <$> nameRef ^. Sugar.nrGotoDefinition
        maker (nameRef ^. Sugar.nrName) nameId
            <&> Align.tValue %~ E.weakerEvents jumpToDefinitionEventMap
    & Widget.assignCursor myId nameId
    where
        nameId = Widget.joinId myId ["name"]

makeInlineEventMap ::
    Monad m =>
    Config -> Sugar.BinderVarInline m ->
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
    Monad m =>
    Sugar.DefinitionOutdatedType m -> Widget.Id ->
    ExprGuiM m (WithTextPos (Widget (T m Widget.EventResult)))
definitionTypeChangeBox info getVarId =
    do
        headerLabel <- TextView.makeLabel "Type was:"
        typeWhenUsed <-
            mkTypeView "typeWhenUsed" (info ^. Sugar.defTypeWhenUsed)
        spacing <- Spacer.stdVSpace
        sepLabel <-
            (Widget.makeFocusableView ?? myId <&> (Align.tValue %~))
            <*> (TextView.makeLabel "Update to:" <&> Align.tValue %~ Widget.fromView)
        typeCurrent <- mkTypeView "typeCurrent" (info ^. Sugar.defTypeCurrent)
        config <- Lens.view Config.config
        theme <- Lens.view Theme.theme
        let padding = realToFrac <$> Theme.valFramePadding theme
        let box =
                headerLabel /-/ typeWhenUsed /-/ spacing /-/ sepLabel /-/ typeCurrent
                & View.pad padding
                & Hover.addBackground animId (Theme.hoverBGColor theme)
        -- TODO: unify config's button press keys
        let keys = Config.newDefinitionButtonPressKeys (Config.pane config)
        let update = (info ^. Sugar.defTypeUseCurrent) >> return getVarId
        Hover.addDarkBackground animId
            ?? box
            <&> Align.tValue %~ E.weakerEvents
                (Widget.keysEventMapMovesCursor keys
                 (E.Doc ["Edit", "Update definition type"]) update)
    where
        mkTypeView idSuffix scheme = TypeView.make (scheme ^. schemeType) (animId ++ [idSuffix])
        myId = Widget.joinId getVarId ["type change"]
        animId = Widget.toAnimId myId

processDefinitionWidget ::
    Monad m =>
    Sugar.DefinitionForm m -> Widget.Id ->
    ExprGuiM m (WithTextPos (Widget (T m Widget.EventResult))) ->
    ExprGuiM m (WithTextPos (Widget (T m Widget.EventResult)))
processDefinitionWidget Sugar.DefUpToDate _myId mkLayout = mkLayout
processDefinitionWidget Sugar.DefDeleted _myId mkLayout =
    (ExpressionGui.addDeletionDiagonal ?? 0.1)
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
            do
                box <-
                    definitionTypeChangeBox info myId
                    -- Remove the text alignment so it can be placed at different vertical positions
                    <&> (^. Align.tValue)
                let a = layout & Align.tValue %~ Align.anchor
                a & Align.tValue %~
                    Align.hoverInPlaceOf (Align.hoverBesideOptions box (a ^. Align.tValue))
                    & return
            else return layout

makeGetBinder ::
    Monad m =>
    Sugar.BinderVar (Name m) m -> Widget.Id ->
    ExprGuiM m (WithTextPos (Widget (T m Widget.EventResult)))
makeGetBinder binderVar myId =
    do
        config <- Lens.view Config.config
        theme <- Lens.view Theme.theme
        let Theme.Name{..} = Theme.name theme
        let (color, processDef) =
                case binderVar ^. Sugar.bvForm of
                Sugar.GetLet -> (letColor, id)
                Sugar.GetDefinition defForm ->
                    ( definitionColor
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
    Sugar.Param (Name m) m -> Widget.Id ->
    ExprGuiM m (WithTextPos (Widget (T m Widget.EventResult)))
makeGetParam param myId =
    do
        theme <- Lens.view Theme.theme
        let paramColor = Theme.name theme & Theme.parameterColor
        case param ^. Sugar.pBinderMode of
            Sugar.LightLambda ->
                makeSimpleView
                <&> Lens.mapped %~ LightLambda.withUnderline theme
                <&> Lens.mapped %~ ExpressionGui.styleNameOrigin name paramColor
            _ ->
                makeSimpleView
                <&> Lens.mapped %~ Reader.local (TextView.color .~ paramColor)
            & makeNameRef myId (param ^. Sugar.pNameRef)
    where
        name = param ^. Sugar.pNameRef . Sugar.nrName

make ::
    Monad m =>
    Sugar.GetVar (Name m) m ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
make getVar pl =
    case getVar of
    Sugar.GetBinder binderVar ->
        makeGetBinder binderVar myId <&> TreeLayout.fromWithTextPos
    Sugar.GetParamsRecord paramsRecordVar ->
        makeParamsRecord myId paramsRecordVar
    Sugar.GetParam param ->
        makeGetParam param myId <&> TreeLayout.fromWithTextPos
    & ExpressionGui.stdWrap pl
    where
        myId = WidgetIds.fromExprPayload pl
