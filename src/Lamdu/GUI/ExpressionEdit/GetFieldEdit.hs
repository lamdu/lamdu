{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.GetFieldEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Options as Options
import qualified GUI.Momentu.Widget as Widget
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionEdit.TagEdit as TagEdit
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.Names.Types (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

make ::
    Monad m =>
    Sugar.GetField (Name m) m (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
make (Sugar.GetField recExpr tag) pl =
    do
        recExprEdit <- ExprGuiM.makeSubexpressionWith 0 recExpr
        dotLabel <- ExpressionGui.grammarLabel "."
        config <- Lens.view Config.config
        let delEventMap =
                case recExpr ^. Sugar.rPayload . Sugar.plActions . Sugar.mReplaceParent of
                Nothing -> mempty
                Just del ->
                    del <&> WidgetIds.fromEntityId
                    & Widget.keysEventMapMovesCursor (Config.delKeys config) (E.Doc ["Edit", "Delete"])
        tagEdit <-
            TagEdit.makeRecordTag TagEdit.WithoutTagHoles
            (pl ^. Sugar.plData . ExprGuiT.plNearestHoles) tag
            <&> Lens.mapped %~ E.weakerEvents delEventMap
        Options.box Options.disambiguationNone
            [ recExprEdit
            , Responsive.fromTextView dotLabel
            , Responsive.fromWithTextPos tagEdit
            ]
            & return
    & ExpressionGui.stdWrapParentExpr pl (tag ^. Sugar.tagInfo . Sugar.tagInstance)
