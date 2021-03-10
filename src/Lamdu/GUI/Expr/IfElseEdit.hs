{-# LANGUAGE TemplateHaskell #-}

module Lamdu.GUI.Expr.IfElseEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           Data.Functor.Compose (Compose(..))
import           Data.Vector.Vector2 (Vector2(..))
import qualified GUI.Momentu as M
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Expression as ResponsiveExpr
import qualified GUI.Momentu.Responsive.Options as Options
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Label as Label
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.Expr.EventMap as ExprEventMap
import           Lamdu.GUI.Monad (GuiM)
import qualified Lamdu.GUI.Monad as GuiM
import           Lamdu.GUI.Styled (label, grammar)
import qualified Lamdu.GUI.Types as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.GUI.Wrap (stdWrapParentExpr)
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

data Row a = Row
    { _rIndentId :: M.AnimId
    , _rKeyword :: a
    , _rPredicate :: a
    , _rResult :: a
    } deriving (Functor, Foldable, Traversable)
Lens.makeLenses ''Row

data IfKind = If | ElseIf

makeIfThen ::
    _ =>
    IfKind -> M.AnimId -> ExprGui.Body Sugar.IfElse i o ->
    GuiM env i o (Row (Responsive o))
makeIfThen ifKind animId ifElse =
    do
        ifGui <-
            GuiM.makeSubexpression (ifElse ^. Sugar.iIf)
            M./|/ (grammar (Label.make ":") M./|/ Spacer.stdHSpace)
        thenGui <- GuiM.makeSubexpression (ifElse ^. Sugar.iThen)
        keyword <-
            grammar ifKeyword
            M./|/ Spacer.stdHSpace
            <&> Responsive.fromTextView
        env <- Lens.view id
        let eventMap =
                foldMap
                (E.keysEventMapMovesCursor (Config.delKeys env)
                 ( E.toDoc env
                     [has . MomentuTexts.edit, has . MomentuTexts.delete]
                 ) . fmap WidgetIds.fromEntityId)
                (ifElse ^. Sugar.iElse . annotation .
                 _1 . Sugar.plActions . Sugar.mReplaceParent)
        Row animId keyword
            (M.weakerEvents eventMap ifGui)
            (M.weakerEvents eventMap thenGui)
            & pure
    where
        ifKeyword =
            case ifKind of
            If -> label Texts.if_
            ElseIf -> label Texts.elseIf

makeElse :: _ => M.AnimId -> ExprGui.Expr Sugar.Else i o -> GuiM env i o [Row (Responsive o)]
makeElse parentAnimId (Ann (Const pl) (Sugar.SimpleElse expr)) =
    Row elseAnimId
    <$> (grammar (label Texts.else_) <&> Responsive.fromTextView)
    <*> (grammar (Label.make ":")
            & Reader.local (M.animIdPrefix .~ elseAnimId)
            <&> Responsive.fromTextView)
    <*> GuiM.makeSubexpression (Ann (Const pl) expr)
    <&> pure
    where
        elseAnimId = parentAnimId <> ["else"]
makeElse _ (Ann pl (Sugar.ElseIf content)) =
    do
        -- TODO: green evaluation backgrounds, "◗"?
        letEventMap <-
            foldMap ExprEventMap.addLetEventMap (pl ^. Lens._Wrapped . _1 . Sugar.plActions . Sugar.mNewLet)
        (:)
            <$> ( makeIfThen ElseIf animId content
                  <&> Lens.mapped %~ M.weakerEvents letEventMap
                )
            <*> makeElse animId (content ^. Sugar.iElse)
            & Reader.local (M.animIdPrefix .~ animId)
    where
        animId = WidgetIds.fromEntityId entityId & Widget.toAnimId
        entityId = pl ^. Lens._Wrapped . _1 . Sugar.plEntityId

verticalRowRender :: _ => f (Row (Responsive o) -> Responsive o)
verticalRowRender =
    do
        indent <- ResponsiveExpr.indent
        obox <- Options.box
        vbox <- Responsive.vboxSpaced
        pure $
            \row ->
            vbox
            [ obox Options.disambiguationNone [row ^. rKeyword, row ^. rPredicate]
            , indent (row ^. rIndentId) (row ^. rResult)
            ]

renderRows :: _ => Maybe M.AnimId -> f ([Row (Responsive o)] -> Responsive o)
renderRows mParensId =
    do
        vspace <- Spacer.getSpaceSize <&> (^._2)
        -- TODO: better way to make space between rows in grid??
        obox <- Options.box
        pad <- Element.pad
        let -- When there's only "if" and "else", we want to merge the predicate with the keyword
            -- because there are no several predicates to be aligned
            prep2 row =
                row
                & rKeyword .~ obox Options.disambiguationNone [row ^. rKeyword, row ^. rPredicate]
                & rPredicate .~ M.empty
        let spaceAbove = (<&> pad (Vector2 0 vspace) 0)
        let prepareRows [] = []
            prepareRows [x, y] = [prep2 x, spaceAbove (prep2 y)]
            prepareRows (x:xs) = x : (xs <&> spaceAbove)
        vert <- verticalRowRender
        addParens <- maybe (pure id) (ResponsiveExpr.addParens ??) mParensId
        vbox <- Responsive.vboxSpaced
        table <- Options.table
        pure $
            \rows ->
            vbox (rows <&> vert)
            & Options.tryWideLayout table (Compose (prepareRows rows))
            & Responsive.rWideDisambig %~ addParens

make :: _ => ExprGui.Expr Sugar.IfElse i o -> GuiM env i o (Responsive o)
make (Ann (Const pl) ifElse) =
    renderRows (ExprGui.mParensId pl)
    <*>
    ( (:)
        <$> makeIfThen If animId ifElse
        <*> makeElse animId (ifElse ^. Sugar.iElse)
    ) & stdWrapParentExpr pl
    where
        animId = pl ^. _1 & WidgetIds.fromExprPayload & Widget.toAnimId
