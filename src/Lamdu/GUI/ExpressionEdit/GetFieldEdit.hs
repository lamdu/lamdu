module Lamdu.GUI.ExpressionEdit.GetFieldEdit
    ( make
    ) where

import           AST (ann)
import qualified Control.Lens as Lens
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Options as Options
import           GUI.Momentu.State (Gui)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Label as Label
import           Lamdu.Config (config)
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionEdit.TagEdit as TagEdit
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Payload as ExprGui
import           Lamdu.GUI.ExpressionGui.Wrap (stdWrapParentExpr)
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.CodeUI as CodeUI
import qualified Lamdu.I18N.Language as Language
import qualified Lamdu.I18N.Texts as Texts
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

make ::
    (Monad i, Monad o) =>
    Sugar.GetField (Name o) i o (ExprGui.SugarExpr i o) ->
    Sugar.Payload (Name o) i o ExprGui.Payload ->
    ExprGuiM i o (Gui Responsive o)
make (Sugar.GetField recExpr tag) pl =
    do
        recExprEdit <- ExprGuiM.makeSubexpression recExpr
        dotLabel <- Label.make "." & Styled.grammar
        env <- Lens.view id
        let mkDelEventMap del =
                del <&> WidgetIds.fromEntityId
                & E.keysEventMapMovesCursor (Config.delKeys (env ^. config))
                (E.toDoc env
                    [Language.edit, Language.texts . Texts.codeUI . CodeUI.delete])
        let delEventMap =
                recExpr ^. ann . Sugar.plActions . Sugar.mReplaceParent
                & foldMap mkDelEventMap
        tagEdit <-
            TagEdit.makeRecordTag tag
            <&> Lens.mapped %~ Widget.weakerEvents delEventMap
        stdWrapParentExpr pl
            <*> (Options.box ?? Options.disambiguationNone ??
                [ recExprEdit
                , Responsive.fromTextView dotLabel
                , Responsive.fromWithTextPos tagEdit
                ])
