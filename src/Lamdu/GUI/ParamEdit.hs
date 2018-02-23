{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ParamEdit
    ( Info(..), make
    , eventMapAddFirstParam, mkParamPickResult
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           GUI.Momentu.Align (WithTextPos)
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.MetaKey (MetaKey, toModKey)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Menu as Menu
import           Lamdu.Config (Config, HasConfig(..))
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.GUI.ExpressionEdit.TagEdit as TagEdit
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExprGui
import qualified Lamdu.GUI.ExpressionGui.Annotation as Annotation
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.NameEdit as NameEdit
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Types as Sugar
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

eventMapAddFirstParam ::
    (MonadReader env m, Applicative f, HasConfig env) =>
    Widget.Id ->
    Sugar.AddFirstParam name f ->
    m (EventMap (f GuiState.Update))
eventMapAddFirstParam binderId addFirst =
    Lens.view Config.config <&> Config.addNextParamKeys
    <&>
    \keys ->
    E.keysEventMapMovesCursor keys (E.Doc ["Edit", doc]) action
    where
        enterParam = WidgetIds.tagHoleId . WidgetIds.fromEntityId
        (action, doc) =
            case addFirst of
            Sugar.NeedToPickTag paramEntity -> (pure (enterParam paramEntity), "Name first parameter")
            Sugar.PrependParam{} -> (pure (TagEdit.addParamId binderId), "Add parameter")
            Sugar.AddInitialParam x -> (x <&> enterParam, "Add parameter")

eventMapAddNextParam ::
    Applicative f => Config -> Widget.Id -> EventMap (f GuiState.Update)
eventMapAddNextParam conf myId =
    TagEdit.addParamId myId & pure
    & E.keysEventMapMovesCursor (Config.addNextParamKeys conf)
        (E.Doc ["Edit", "Add next parameter"])

eventMapOrderParam ::
    Monad m =>
    [MetaKey] -> Text -> m () -> EventMap (m GuiState.Update)
eventMapOrderParam keys docSuffix =
    E.keysEventMap keys (E.Doc ["Edit", "Parameter", "Move " <> docSuffix])

eventParamDelEventMap ::
    Monad m => m () -> [MetaKey] -> Text -> Widget.Id -> EventMap (m GuiState.Update)
eventParamDelEventMap fpDel keys docSuffix dstPosId =
    GuiState.updateCursor dstPosId <$ fpDel
    & E.keyPresses (keys <&> toModKey)
        (E.Doc ["Edit", "Delete parameter" <> docSuffix])

data Info m = Info
    { iNameEdit :: WithTextPos (Widget (T m GuiState.Update))
    , iDel :: T m ()
    , iMAddNext :: Maybe (Sugar.TagSelection (Name (T m)) (T m) ())
    , iMOrderBefore :: Maybe (T m ())
    , iMOrderAfter :: Maybe (T m ())
    , iId :: Widget.Id
    }

mkParamPickResult :: Sugar.TagInfo -> a -> Menu.PickResult
mkParamPickResult tagInfo _ =
    Menu.PickResult
    { Menu._pickDest = tagInfo ^. Sugar.tagInstance & WidgetIds.fromEntityId
    , Menu._pickNextEntryPoint =
        tagInfo ^. Sugar.tagInstance
        & WidgetIds.fromEntityId
        & TagEdit.addParamId
    }

-- exported for use in definition sugaring.
make ::
    Monad m =>
    Annotation.EvalAnnotationOptions ->
    Widget.Id -> Widget.Id ->
    Sugar.FuncParam (Info m) -> ExprGuiM m [ExpressionGui m]
make annotationOpts prevId nextId param =
    do
        conf <- Lens.view Config.config
        let paramEventMap =
                mconcat
                [ eventParamDelEventMap (iDel info) (Config.delForwardKeys conf) "" nextId
                , eventParamDelEventMap (iDel info) (Config.delBackwardKeys conf) " backwards" prevId
                , (eventMapAddNextParam conf myId <$ iMAddNext info) ^. Lens._Just
                , foldMap (eventMapOrderParam (Config.paramOrderBeforeKeys conf) "before") (iMOrderBefore info)
                , foldMap (eventMapOrderParam (Config.paramOrderAfterKeys conf) "after") (iMOrderAfter info)
                ]
        wideAnnotationBehavior <-
            GuiState.isSubCursor ?? myId
            <&> Annotation.wideAnnotationBehaviorFromSelected
        paramEdit <-
            Annotation.maybeAddAnnotationWith annotationOpts
            wideAnnotationBehavior ExprGui.showAnnotationWhenVerbose
            (param ^. Sugar.fpAnnotation)
            ?? Responsive.fromWithTextPos (iNameEdit info)
            <&> Widget.widget . Widget.eventMapMaker . Lens.mapped %~ (<> paramEventMap)
            & Reader.local (Element.animIdPrefix .~ Widget.toAnimId myId)
        mAddParam <-
            GuiState.isSubCursor ?? addId
            <&> guard
            <&> (>> iMAddNext info)
        addParamEdits <-
            case mAddParam of
            Nothing -> pure []
            Just addParam ->
                TagEdit.makeTagHoleEdit addParam mkParamPickResult addId
                & NameEdit.withNameColor Theme.parameterColor
                <&> Responsive.fromWithTextPos
                <&> (:[])
        paramEdit : addParamEdits & pure
    where
        myId = iId info
        addId = TagEdit.addParamId myId
        info = param ^. Sugar.fpInfo
