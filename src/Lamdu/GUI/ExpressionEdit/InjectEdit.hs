{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.InjectEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import           Data.Store.Transaction (Transaction)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Options as Options
import qualified GUI.Momentu.Widget as Widget
import qualified Lamdu.Config as Config
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
    Monad m =>
    Sugar.Tag (Name m) m ->
    Maybe (Transaction m Sugar.EntityId) ->
    NearestHoles -> [ExpressionGui m] ->
    ExprGuiM m (ExpressionGui m)
makeCommon tag mDelInject nearestHoles valEdits =
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
            ( TagEdit.makeCaseTag nearestHoles tag
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
        Nothing
        (pl ^. Sugar.plData . ExprGuiT.plNearestHoles) []
        & ExpressionGui.stdWrap pl
    Just val ->
        ExprGuiM.makeSubexpressionWith ApplyEdit.prefixPrecedence
        (ExpressionGui.before .~ ApplyEdit.prefixPrecedence) val <&> (:[])
        >>= makeCommon tag (val ^. Sugar.rPayload . Sugar.plActions . Sugar.mReplaceParent) (ExprGuiT.nextHolesBefore val)
        & ExpressionGui.stdWrapParentExpr pl (tag ^. Sugar.tagInfo . Sugar.tagInstance)
