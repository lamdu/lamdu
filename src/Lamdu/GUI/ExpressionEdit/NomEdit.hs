{-# LANGUAGE NoMonomorphismRestriction #-}
module Lamdu.GUI.ExpressionEdit.NomEdit
    ( makeFromNom, makeToNom
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/|/))
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Expression as ResponsiveExpr
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Config.Theme.TextColors as TextColors
import qualified Lamdu.GUI.ExpressionEdit.BinderEdit as BinderEdit
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExprGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import           Lamdu.GUI.ExpressionGui.Wrap (stdWrapParentExpr)
import qualified Lamdu.GUI.NameView as NameView
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

mReplaceParent ::
    Lens.Traversal'
    (Sugar.Expression name i o (Sugar.Payload name i o a))
    (o Sugar.EntityId)
mReplaceParent = Sugar.annotation . Sugar.plActions . Sugar.mReplaceParent . Lens._Just

makeToNom ::
    (Monad i, Monad o) =>
    Sugar.Nominal (Name o)
    (Sugar.BinderBody (Name o) i o
        (Sugar.Payload (Name o) i o ExprGui.Payload)) ->
    Sugar.Payload (Name o) i o ExprGui.Payload ->
    ExprGuiM i o (ExpressionGui o)
makeToNom nom pl =
    nom <&> BinderEdit.makeBinderBodyEdit
    & mkNomGui id "ToNominal" "«" mDel pl
    where
        bbContent = nom ^. Sugar.nVal . Sugar.bbContent
        mDel = bbContent ^? Sugar._BinderExpr . mReplaceParent

makeFromNom ::
    (Monad i, Monad o) =>
    Sugar.Nominal (Name o) (ExprGui.SugarExpr i o) ->
    Sugar.Payload (Name o) i o ExprGui.Payload ->
    ExprGuiM i o (ExpressionGui o)
makeFromNom nom pl =
    nom <&> ExprGuiM.makeSubexpression
    & mkNomGui reverse "FromNominal" "»" mDel pl
    where
        mDel = nom ^? Sugar.nVal . mReplaceParent

mkNomGui ::
    (Monad i, Monad o) =>
    ([ExpressionGui o] -> [ExpressionGui o]) ->
    Text -> Text -> Maybe (o Sugar.EntityId) ->
    Sugar.Payload (Name o) i o ExprGui.Payload ->
    Sugar.Nominal (Name o) (ExprGuiM i o (ExpressionGui o)) ->
    ExprGuiM i o (ExpressionGui o)
mkNomGui ordering nomStr str mDel pl (Sugar.Nominal tid val) =
    do
        nomColor <- Lens.view (Theme.theme . Theme.textColors . TextColors.nomColor)
        config <- Lens.view Config.config
        let mkEventMap action =
                action <&> WidgetIds.fromEntityId
                & E.keysEventMapMovesCursor (Config.delKeys config)
                (E.Doc ["Edit", "Nominal", "Delete " <> nomStr])
        let eventMap = mDel ^. Lens._Just . Lens.to mkEventMap
        stdWrapParentExpr pl
            <*> ( (ResponsiveExpr.boxSpacedMDisamb ?? ExprGui.mParensId pl)
                    <*>
                    ( sequence
                    [
                        do
                            label <- Styled.grammarLabel str
                            nameGui <-
                                NameView.make
                                (tid ^. Sugar.tidName)
                            Widget.makeFocusableView ?? nameId
                                <&> (Align.tValue %~) ?? label /|/ nameGui
                        <&> Responsive.fromWithTextPos
                        & Reader.local (TextView.color .~ nomColor)
                        <&> Widget.weakerEvents eventMap
                    , val
                    ] <&> ordering
                    )
                )
    where
        myId = WidgetIds.fromExprPayload pl
        nameId = Widget.joinId myId ["name"]
