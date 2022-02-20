module Lamdu.GUI.Expr.BinderEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import           Hyper (annValue)
import qualified GUI.Momentu as M
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.GUI.Expr.AssignmentEdit as AssignmentEdit
import qualified Lamdu.GUI.Expr.EventMap as ExprEventMap
import qualified Lamdu.GUI.Expr.ParamsEdit as ParamsEdit
import           Lamdu.GUI.Monad (GuiM)
import qualified Lamdu.GUI.Monad as GuiM
import           Lamdu.GUI.Styled (grammar, label)
import qualified Lamdu.GUI.Types as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.GUI.Wrap (stdWrapParentExpr)
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.CodeUI as CodeUI
import qualified Lamdu.I18N.Definitions as Definitions
import qualified Lamdu.I18N.Navigation as Texts
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

makeLetEdit :: _ => ExprGui.Body Sugar.Let i o -> Widget.Id -> GuiM env i o (Responsive o)
makeLetEdit item myId =
    do
        env <- Lens.view id
        delKeys <- Config.delKeys
        let eventMap =
                foldMap
                ( E.keysEventMapMovesCursor (env ^. has . Config.extractKeys)
                    (E.toDoc env
                        [ has . MomentuTexts.edit
                        , has . CodeUI.letClause
                        , has . Definitions.extractToOuter
                        ])
                    . fmap ExprEventMap.extractCursor
                ) (item ^? Sugar.lValue . annotation . Sugar.plActions . Sugar.extract)
                <>
                foldMap
                ( E.keysEventMapMovesCursor delKeys
                    (E.toDoc env [has . MomentuTexts.edit, has . Texts.let_, has . MomentuTexts.delete])
                    . fmap (const bodyId)
                ) (item ^? Sugar.lNames . Sugar._LhsVar . Sugar.vDelete)
        (_, nameEdit) <- ParamsEdit.make True (pure Nothing) ParamsEdit.ScopeNavNotFocused myId myId bodyId (item ^. Sugar.lNames)
        grammar (label Texts.let_) M./|/ Spacer.stdHSpace M./|/ pure nameEdit
            >>= AssignmentEdit.make Nothing myId binder
            <&> M.weakerEvents eventMap
            <&> M.padAround (env ^. has . Theme.letItemPadding)
    where
        bodyId = item ^. Sugar.lBody . annotation & WidgetIds.fromExprPayload
        binder = item ^. Sugar.lValue

make :: _ => ExprGui.Expr Sugar.Binder i o -> GuiM env i o (Responsive o)
make x =
    do
        letEventMap <- x ^. hVal . Sugar.bAddOuterLet & ExprEventMap.addLetEventMap
        makeBody (x & annValue %~ (^. Sugar.bBody))
            <&> M.weakerEvents letEventMap

makeBody :: _ => ExprGui.Expr Sugar.BinderBody i o -> GuiM env i o (Responsive o)
makeBody (Ann (Const pl) (Sugar.BinderTerm assignmentBody)) =
    Ann (Const pl) assignmentBody & GuiM.makeSubexpression
makeBody (Ann (Const pl) (Sugar.BinderLet l)) =
    do
        env <- Lens.view id
        let moveToInnerEventMap =
                body
                ^? hVal . Sugar.bBody . Sugar._BinderLet
                . Sugar.lValue . annotation . Sugar.plActions
                . Sugar.extract
                & foldMap
                (E.keysEventMap (env ^. has . Config.moveLetInwardKeys)
                (E.toDoc env
                    [ has . MomentuTexts.edit
                    , has . CodeUI.letClause
                    , has . Texts.moveInwards
                    ]) . void)
        Responsive.vboxSpaced
            <*>
            sequence
            [ makeLetEdit l myId <&> M.weakerEvents moveToInnerEventMap
            , make body
            ]
        & stdWrapParentExpr pl
        & local (M.animIdPrefix .~ Widget.toAnimId myId)
    where
        myId = WidgetIds.fromExprPayload pl
        body = l ^. Sugar.lBody
