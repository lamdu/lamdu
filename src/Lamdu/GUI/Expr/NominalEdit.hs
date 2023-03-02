{-# LANGUAGE RankNTypes #-}

module Lamdu.GUI.Expr.NominalEdit
    ( makeFromNom, makeToNom, makeTId, makeTIdView
    ) where
import           GUI.Momentu.Element.Id (ElemId)

import qualified Control.Lens as Lens
import           GUI.Momentu (Responsive)
import qualified GUI.Momentu as M
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.I18N as MomentuTexts
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Expression as ResponsiveExpr
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Label as Label
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Config.Theme.TextColors as TextColors
import           Lamdu.GUI.Monad (GuiM)
import qualified Lamdu.GUI.Monad as GuiM
import qualified Lamdu.GUI.NameView as NameView
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.Types as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.GUI.Wrap (stdWrapParentExpr)
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.Navigation as Texts
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

makeToNom :: _ => ExprGui.Expr Sugar.Nominal i o -> GuiM env i o (Responsive o)
makeToNom (Ann (Const pl) (Sugar.Nominal tid binder)) =
    do
        env <- Lens.view id
        let mkEventMap action =
                action <&> WidgetIds.fromEntityId
                & E.keysEventMapMovesCursor (Config.delKeys env)
                (E.toDoc env
                    [ has . MomentuTexts.edit
                    , has . Texts.nominal
                    , has . MomentuTexts.delete
                    ])
        let eventMap =
                binder ^.
                annotation . Sugar.plActions . Sugar.mReplaceParent .
                Lens._Just . Lens.to mkEventMap
        sequence
            [ makeTId myId tid
                <&> Responsive.fromWithTextPos
                <&> M.weakerEvents eventMap
            , GuiM.makeBinder binder
            ] >>= ResponsiveExpr.boxSpacedMDisamb (ExprGui.mParensId pl)
            & stdWrapParentExpr pl
    where
        myId = WidgetIds.fromExprPayload pl

makeFromNom :: _ => ElemId -> Sugar.TId Name -> GuiM env i o (Responsive o)
makeFromNom myId nom =
    Styled.grammar (Label.make ".") M./|/ makeTId myId nom <&> Responsive.fromWithTextPos

makeJumpToNomEventMap :: _ => Sugar.TId Name -> GuiM env i o (E.EventMap (o M.Update))
makeJumpToNomEventMap tid =
    do
        savePrecursor <- GuiM.mkPrejumpPosSaver
        env <- Lens.view id
        openPane <- GuiM.openPane
        let keys = env ^. has . Config.jumpToDefinitionKeys
        let doc = E.toDoc env [has . MomentuTexts.navigation, has . Texts.jumpToDef]
        do
            savePrecursor
            tid ^. Sugar.tidTId & Sugar.GoToNom & openPane
            <&> WidgetIds.fromEntityId
            & E.keysEventMapMovesCursor keys doc
            & pure

makeTIdView :: _ => Sugar.TId Name -> GuiM env i o (M.WithTextPos M.View)
makeTIdView tid =
    do
        nomColor <- Lens.view (has . Theme.textColors . TextColors.nomColor)
        NameView.make (tid ^. Sugar.tidName) & local (TextView.color .~ nomColor)

makeTId :: _ => ElemId -> Sugar.TId Name -> GuiM env i o (M.TextWidget o)
makeTId myId tid =
    do
        jumpToDefinitionEventMap <- makeJumpToNomEventMap tid
        makeTIdView tid
            >>= M.tValue (Widget.makeFocusableView nameId)
            <&> Align.tValue %~ Widget.weakerEvents jumpToDefinitionEventMap
    where
        nameId = myId <> "name"
