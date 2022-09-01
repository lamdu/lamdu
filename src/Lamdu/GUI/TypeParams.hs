module Lamdu.GUI.TypeParams
    ( make
    ) where

import qualified Control.Lens as Lens
import           Data.Property (Property)
import           GUI.Momentu (Responsive, EventMap, Update)
import qualified GUI.Momentu as M
import           GUI.Momentu.Direction (Orientation(..), Order(..))
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.DropDownList as DropDownList
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import           GUI.Momentu.Widgets.StdKeys (dirKey)
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Config.Theme.ValAnnotation as ValAnnotation
import qualified Lamdu.GUI.Expr.TagEdit as TagEdit
import qualified Lamdu.GUI.ParamEdit as ParamEdit
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.TaggedList as TaggedList
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.CodeUI as Texts
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

make ::
    _ =>
    Sugar.TaggedList Name i o (Property o Sugar.ParamKind) -> Widget.Id -> Widget.Id ->
    m (EventMap (o Update), [Responsive o])
make params prevId myId =
    do
        o <- Lens.view has <&> (`dirKey` Horizontal)
        keys <-
            traverse Lens.view TaggedList.Keys
            { TaggedList._kAdd = has . Config.addNextParamKeys
            , TaggedList._kOrderBefore = has . Config.orderDirKeys . o Backward
            , TaggedList._kOrderAfter = has . Config.orderDirKeys . o Forward
            }
        (addFirstEventMap, itemsR) <-
            -- TODO: rhs id
            TaggedList.make [has . Texts.parameter] keys (pure prevId) (pure myId) params
        ParamEdit.mkAddParam (params ^. Sugar.tlAddFirst) prevId
            <> (traverse makeParam itemsR <&> concat)
            <&> (,) addFirstEventMap

paramKindEdit :: _ => Property o Sugar.ParamKind -> Widget.Id -> m (M.TextWidget o)
paramKindEdit prop myId@(Widget.Id animId) =
    (DropDownList.make ?? prop)
    <*> Lens.sequenceOf (traverse . _2)
        [(Sugar.TypeParam, Styled.focusableLabel Texts.typ), (Sugar.RowParam, Styled.focusableLabel Texts.row)]
    <*> (DropDownList.defaultConfig <*> Lens.view (has . Texts.parameter))
    ?? myId
    & local (M.animIdPrefix .~ animId)

makeParam :: _ => TaggedList.Item Name i o (Property o Sugar.ParamKind) -> m [Responsive o]
makeParam item =
    (:)
    <$> ( TagEdit.makeParamTag Nothing (item ^. TaggedList.iTag)
            M./-/ (Lens.view (has . Theme.valAnnotation . ValAnnotation.valAnnotationSpacing) >>= Spacer.vspaceLines)
            M./-/ paramKindEdit (item ^. TaggedList.iValue) (Widget.joinId myId ["kind"])
            <&> Responsive.fromWithTextPos
            <&> M.weakerEvents (item ^. TaggedList.iEventMap)
        )
    <*> ParamEdit.mkAddParam (item ^. TaggedList.iAddAfter) myId
    where
        myId = item ^. TaggedList.iTag . Sugar.tagRefTag . Sugar.tagInstance & WidgetIds.fromEntityId
