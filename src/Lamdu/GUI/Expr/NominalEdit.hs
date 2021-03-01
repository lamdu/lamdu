{-# LANGUAGE RankNTypes #-}

module Lamdu.GUI.Expr.NominalEdit
    ( makeFromNom, makeToNom
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import qualified GUI.Momentu as M
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Expression as ResponsiveExpr
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Label as Label
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Config.Theme.TextColors as TextColors
import qualified Lamdu.GUI.NameView as NameView
import           Lamdu.GUI.Monad (GuiM)
import qualified Lamdu.GUI.Monad as GuiM
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.Types as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.GUI.Wrap (stdWrapParentExpr)
import qualified Lamdu.I18N.CodeUI as Texts
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
                    , has . Texts.deleteToNominal
                    ])
        let eventMap =
                binder ^.
                annotation . _1 . Sugar.plActions . Sugar.mReplaceParent .
                Lens._Just . Lens.to mkEventMap
        (ResponsiveExpr.boxSpacedMDisamb ?? ExprGui.mParensId pl)
            <*>
            sequence
            [ (Widget.makeFocusableView ?? nameId <&> (M.tValue %~))
                <*> mkNomLabel tid
                <&> Responsive.fromWithTextPos
                <&> M.weakerEvents eventMap
            , GuiM.makeBinder binder
            ] & stdWrapParentExpr pl
    where
        myId = WidgetIds.fromExprPayload (pl ^. _1)
        nameId = Widget.joinId myId ["name"]

makeFromNom ::
    _ => Annotated (ExprGui.Payload i o) # Const (Sugar.TId Name) -> GuiM env i o (Responsive o)
makeFromNom (Ann (Const pl) (Const nom)) =
    Styled.grammar (Label.make ".")
    M./|/ mkNomLabel nom
    <&> Responsive.fromTextView
    & stdWrapParentExpr pl

mkNomLabel :: _ => Sugar.TId Name -> GuiM env i o (M.WithTextPos M.View)
mkNomLabel tid =
    do
        nomColor <- Lens.view (has . Theme.textColors . TextColors.nomColor)
        NameView.make (tid ^. Sugar.tidName) & Reader.local (TextView.color .~ nomColor)
