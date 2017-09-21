{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.InjectEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Transaction (MonadTransaction)
import           Data.Store.Transaction (Transaction)
import           GUI.Momentu.Align (WithTextPos)
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/|/))
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Options as Options
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import           Lamdu.Config (HasConfig)
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (HasTheme)
import qualified Lamdu.GUI.ExpressionEdit.ApplyEdit as ApplyEdit
import qualified Lamdu.GUI.ExpressionEdit.TagEdit as TagEdit
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.Names.Types (Name(..))
import           Lamdu.Sugar.NearestHoles (NearestHoles)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

makeCommon ::
    ( Monad m, MonadReader env f, MonadTransaction m f
    , HasConfig env, HasTheme env, Widget.HasCursor env, TextEdit.HasStyle env
    , Spacer.HasStdSpacing env, Element.HasAnimIdPrefix env, Menu.HasStyle env
    , Hover.HasStyle env
    ) =>
    Sugar.Tag (Name m) m ->
    Maybe (Transaction m Sugar.EntityId) ->
    NearestHoles -> WithTextPos View -> [ExpressionGui m] ->
    f (ExpressionGui m)
makeCommon tag mDelInject nearestHoles colonLabel valEdits =
    do
        config <- Lens.view Config.config
        let delEventMap =
                case mDelInject of
                Nothing -> mempty
                Just del ->
                    del <&> WidgetIds.fromEntityId
                    & Widget.keysEventMapMovesCursor (Config.delKeys config) (E.Doc ["Edit", "Delete"])
        (Options.boxSpaced ?? Options.disambiguationNone)
            <*>
            ( TagEdit.makeCaseTag TagEdit.WithoutTagHoles nearestHoles tag
                <&> (/|/ colonLabel)
                <&> Lens.mapped %~ E.weakerEvents delEventMap
                <&> Responsive.fromWithTextPos <&> (: valEdits)
            )

make ::
    Monad m =>
    Sugar.Inject (Name m) m (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
make (Sugar.Inject tag mVal) pl =
    case mVal of
    Nothing ->
        makeCommon
        -- Give the tag widget the identity of the whole inject
        (tag & Sugar.tagInfo . Sugar.tagInstance .~ (pl ^. Sugar.plEntityId))
        Nothing (pl ^. Sugar.plData . ExprGuiT.plNearestHoles) Element.empty []
        & ExpressionGui.stdWrap pl
    Just val ->
        do
            arg <-
                ExprGuiM.makeSubexpressionWith ApplyEdit.prefixPrecedence
                (ExpressionGui.before .~ ApplyEdit.prefixPrecedence) val <&> (:[])
            colon <- ExpressionGui.grammarLabel ":"
            makeCommon tag replaceParent (ExprGuiT.nextHolesBefore val) colon arg
                & ExpressionGui.stdWrapParentExpr pl tagInstance
        where
            tagInstance = tag ^. Sugar.tagInfo . Sugar.tagInstance
            replaceParent = val ^. Sugar.rPayload . Sugar.plActions . Sugar.mReplaceParent
