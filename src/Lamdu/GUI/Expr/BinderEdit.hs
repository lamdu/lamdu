{-# LANGUAGE TypeOperators #-}
module Lamdu.GUI.Expr.BinderEdit
    ( make
    ) where

import           Control.Applicative (liftA2)
import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import qualified Data.Map as Map
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/|/))
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Choice as Choice
import qualified GUI.Momentu.Widgets.Grid as Grid
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import           Hyper.Combinator.Ann (Annotated)
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Config.Theme.TextColors as TextColors
import qualified Lamdu.GUI.Expr.AssignmentEdit as AssignmentEdit
import qualified Lamdu.GUI.Expr.EventMap as ExprEventMap
import           Lamdu.GUI.ExpressionGui.Monad (GuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as GuiM
import qualified Lamdu.GUI.ExpressionGui.Payload as ExprGui
import           Lamdu.GUI.ExpressionGui.Wrap (stdWrapParentExpr)
import           Lamdu.GUI.Styled (grammar, label)
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.CodeUI as CodeUI
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.Definitions as Definitions
import qualified Lamdu.I18N.Definitions as Texts
import qualified Lamdu.I18N.Name as Texts
import qualified Lamdu.I18N.Navigation as Texts
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

makeLetEdit ::
    ( Monad i, Monad o
    , Grid.HasTexts env
    , TextEdit.HasTexts env
    , SearchMenu.HasTexts env
    , Has (Choice.Texts Text) env
    , Has (Texts.Code Text) env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Definitions Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Navigation Text) env
    ) =>
    Sugar.Let Name i o # Ann (Const (Sugar.Payload Name i o ExprGui.Payload)) ->
    GuiM env i o (Responsive o)
makeLetEdit item =
    do
        env <- Lens.view id
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
                E.keysEventMapMovesCursor (Config.delKeys env)
                (E.toDoc env
                    [ has . MomentuTexts.edit
                    , has . CodeUI.letClause
                    , has . MomentuTexts.delete
                    ])
                (bodyId <$ item ^. Sugar.lDelete)
                <>
                foldMap
                ( E.keysEventMapMovesCursor (env ^. has . Config.inlineKeys)
                    (E.toDoc env
                        [ has . MomentuTexts.navigation
                        , has . Texts.jumpToFirstUse
                        ])
                    . pure . WidgetIds.fromEntityId
                ) (item ^? Sugar.lUsages . Lens.ix 0)
        grammar (label Texts.let_)
            /|/ Spacer.stdHSpace
            /|/ (AssignmentEdit.make Nothing (item ^. Sugar.lName)
                    TextColors.letColor binder
                    <&> Widget.weakerEvents eventMap
                    <&> Element.padAround (env ^. has . Theme.letItemPadding))
    where
        bodyId = item ^. Sugar.lBody . annotation & WidgetIds.fromExprPayload
        binder = item ^. Sugar.lValue

lookupMKey :: Ord k => Maybe k -> Map k a -> Maybe a
lookupMKey k m = k >>= (`Map.lookup` m)

make ::
    ( Monad i, Monad o
    , Grid.HasTexts env
    , TextEdit.HasTexts env
    , SearchMenu.HasTexts env
    , Has (Choice.Texts Text) env
    , Has (Texts.Code Text) env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Definitions Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Navigation Text) env
    ) =>
    Annotated (Sugar.Payload Name i o ExprGui.Payload) # Sugar.Binder Name i o ->
    GuiM env i o (Responsive o)
make (Ann (Const pl) (Sugar.BinderExpr assignmentBody)) =
    Ann (Const pl) assignmentBody & GuiM.makeSubexpression
make (Ann (Const pl) (Sugar.BinderLet l)) =
    do
        env <- Lens.view id
        let moveToInnerEventMap =
                body
                ^? hVal . Sugar._BinderLet
                . Sugar.lValue . annotation . Sugar.plActions
                . Sugar.extract
                & foldMap
                (E.keysEventMap (env ^. has . Config.moveLetInwardKeys)
                (E.toDoc env
                    [ has . MomentuTexts.edit
                    , has . CodeUI.letClause
                    , has . Texts.moveInwards
                    ]) . void)
        mOuterScopeId <- GuiM.readMScopeId
        let letBodyScope = liftA2 lookupMKey mOuterScopeId (l ^. Sugar.lBodyScope)
        Responsive.vboxSpaced
            <*>
            sequence
            [ makeLetEdit l <&> Widget.weakerEvents moveToInnerEventMap
            , make body & GuiM.withLocalMScopeId letBodyScope
            ]
        & stdWrapParentExpr pl
        & Reader.local (Element.animIdPrefix .~ Widget.toAnimId myId)
    where
        myId = WidgetIds.fromExprPayload pl
        body = l ^. Sugar.lBody
