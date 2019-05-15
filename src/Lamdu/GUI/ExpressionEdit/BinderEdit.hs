{-# LANGUAGE NamedFieldPuns, FlexibleContexts, NoMonomorphismRestriction #-}
module Lamdu.GUI.ExpressionEdit.BinderEdit
    ( make
    ) where

import           AST (Tree)
import           AST.Knot.Ann (Ann(..), ann, val)
import           Control.Applicative (liftA2)
import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import qualified Data.Map as Map
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/|/))
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import           GUI.Momentu.State (Gui)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Config.Theme.TextColors as TextColors
import qualified Lamdu.GUI.ExpressionEdit.AssignmentEdit as AssignmentEdit
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Payload as ExprGui
import           Lamdu.GUI.ExpressionGui.Wrap (stdWrapParentExpr)
import           Lamdu.GUI.Styled (label, grammar)
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.CodeUI as CodeUI
import qualified Lamdu.I18N.Definitions as Definitions
import qualified Lamdu.I18N.Language as Language
import qualified Lamdu.I18N.Texts as Texts
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

makeLetEdit ::
    (Monad i, Monad o) =>
    Tree (Sugar.Let (Name o) i o)
        (Ann (Sugar.Payload (Name o) i o ExprGui.Payload)) ->
    ExprGuiM i o (Gui Responsive o)
makeLetEdit item =
    do
        env <- Lens.view id
        let eventMap =
                foldMap
                ( E.keysEventMapMovesCursor (env ^. has . Config.extractKeys)
                    (E.toDoc (env ^. Language.texts)
                        [ Texts.edit
                        , Texts.codeUI . CodeUI.letClause
                        , Texts.definitions . Definitions.extractToOuter
                        ])
                    . fmap ExprEventMap.extractCursor
                ) (item ^? Sugar.lValue . ann . Sugar.plActions . Sugar.extract)
                <>
                E.keysEventMapMovesCursor (Config.delKeys env)
                (E.toDoc (env ^. Language.texts)
                    [ Texts.edit
                    , Texts.codeUI . CodeUI.letClause
                    , Texts.codeUI . CodeUI.delete
                    ])
                (bodyId <$ item ^. Sugar.lDelete)
                <>
                foldMap
                ( E.keysEventMapMovesCursor (env ^. has . Config.inlineKeys)
                    (E.toDoc (env ^. Language.texts)
                        [ Texts.navigation
                        , Texts.navigationTexts . Texts.jumpToFirstUse
                        ])
                    . pure . WidgetIds.fromEntityId
                ) (item ^? Sugar.lUsages . Lens.ix 0)
        grammar (label (Texts.code . Texts.let_))
            /|/ Spacer.stdHSpace
            /|/ (AssignmentEdit.make Nothing mempty (item ^. Sugar.lName)
                    TextColors.letColor binder
                    <&> Widget.weakerEvents eventMap
                    <&> Element.padAround (env ^. has . Theme.letItemPadding))
    where
        bodyId = item ^. Sugar.lBody . ann & WidgetIds.fromExprPayload
        binder = item ^. Sugar.lValue

lookupMKey :: Ord k => Maybe k -> Map k a -> Maybe a
lookupMKey k m = k >>= (`Map.lookup` m)

make ::
    (Monad i, Monad o) =>
    Tree (Ann (Sugar.Payload (Name o) i o ExprGui.Payload))
        (Sugar.Binder (Name o) i o) ->
    ExprGuiM i o (Gui Responsive o)
make (Ann pl (Sugar.BinderExpr assignmentBody)) =
    Ann pl assignmentBody & ExprGuiM.makeSubexpression
make (Ann pl (Sugar.BinderLet l)) =
    do
        env <- Lens.view id
        let moveToInnerEventMap =
                body
                ^? val . Sugar._BinderLet
                . Sugar.lValue . ann . Sugar.plActions
                . Sugar.extract
                & foldMap
                (E.keysEventMap (env ^. has . Config.moveLetInwardKeys)
                (E.toDoc (env ^. Language.texts)
                    [ Texts.edit
                    , Texts.codeUI . CodeUI.letClause
                    , Texts.navigationTexts . Texts.moveInwards
                    ]) . void)
        mOuterScopeId <- ExprGuiM.readMScopeId
        let letBodyScope = liftA2 lookupMKey mOuterScopeId (l ^. Sugar.lBodyScope)
        stdWrapParentExpr pl
            <*>
            ( Responsive.vboxSpaced
                <*>
                sequence
                [ makeLetEdit l <&> Widget.weakerEvents moveToInnerEventMap
                , make body
                & ExprGuiM.withLocalMScopeId letBodyScope
                ]
            )
        & Reader.local (Element.animIdPrefix .~ Widget.toAnimId myId)
    where
        myId = WidgetIds.fromExprPayload pl
        body = l ^. Sugar.lBody
