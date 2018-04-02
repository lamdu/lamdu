{-# LANGUAGE OverloadedStrings #-}
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
import qualified Lamdu.GUI.ExpressionGui as ExprGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import           Lamdu.GUI.ExpressionGui.Wrap (stdWrapParentExpr)
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Types as Sugar
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

make ::
    Monad m =>
    Sugar.GetField (Name (T m)) (T m) (ExprGui.SugarExpr (T m)) ->
    Sugar.Payload (Name (T m)) (T m) ExprGui.Payload ->
    ExprGuiM (T m) (ExpressionGui (T m))
make (Sugar.GetField recExpr tag) pl =
    do
        recExprEdit <- ExprGuiM.makeSubexpression recExpr
        dotLabel <- Styled.grammarLabel "."
        config <- Lens.view Config.config
        let delEventMap =
                case recExpr ^. Sugar.rPayload . Sugar.plActions . Sugar.mReplaceParent of
                Nothing -> mempty
                Just del ->
                    del <&> WidgetIds.fromEntityId
                    & E.keysEventMapMovesCursor (Config.delKeys config) (E.Doc ["Edit", "Delete"])
        tagEdit <-
            TagEdit.makeRecordTag (pl ^. Sugar.plData . ExprGui.plNearestHoles) tag
            <&> Lens.mapped %~ Widget.weakerEvents delEventMap
        stdWrapParentExpr pl
            ?? Options.box Options.disambiguationNone
                [ recExprEdit
                , Responsive.fromTextView dotLabel
                , Responsive.fromWithTextPos tagEdit
                ]
