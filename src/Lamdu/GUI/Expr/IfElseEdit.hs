{-# LANGUAGE TemplateHaskell #-}

module Lamdu.GUI.Expr.IfElseEdit
    ( make
    ) where

import qualified Control.Lens as Lens
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
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Label as Label
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified Lamdu.GUI.Expr.EventMap as ExprEventMap
import           Lamdu.GUI.Monad (GuiM)
import qualified Lamdu.GUI.Monad as GuiM
import           Lamdu.GUI.Styled (label, grammar)
import qualified Lamdu.GUI.Types as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.GUI.Wrap (stdWrapParentExpr)
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.Navigation as Texts
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
        env <- Lens.view id
        let jumpToThen =
                ifElse ^?! Sugar.iThen .
                (hVal . Sugar._BodyLam . Sugar.lamFunc . Sugar.fBody . annotation <> annotation)
                & WidgetIds.fromExprPayload & GuiState.updateCursor & pure & const
                & E.charGroup Nothing (E.toDoc env [has . MomentuTexts.navigation, has . Texts.jumpToThen]) ":"
        ifGui <-
            GuiM.makeSubexpression (ifElse ^. Sugar.iIf)
            M./|/ (grammar (Label.make ":") M./|/ Spacer.stdHSpace)
            <&> M.weakerEvents jumpToThen
        thenGui <- GuiM.makeSubexpression (ifElse ^. Sugar.iThen)
        keyword <-
            grammar ifKeyword
            M./|/ Spacer.stdHSpace
            <&> Responsive.fromTextView
        Row animId keyword ifGui thenGui & pure
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
            & local (M.animIdPrefix .~ elseAnimId)
            <&> Responsive.fromTextView)
    <*> GuiM.makeSubexpression (Ann (Const pl) expr)
    <&> pure
    where
        elseAnimId = parentAnimId <> ["else"]
makeElse _ (Ann (Const pl) (Sugar.ElseIf (Sugar.ElseIfBody addLet content))) =
    do
        -- TODO: green evaluation backgrounds, "◗"?
        letEventMap <- ExprEventMap.addLetEventMap addLet
        (:)
            <$> ( makeIfThen ElseIf animId content
                  <&> Lens.mapped %~ M.weakerEvents letEventMap
                )
            <*> makeElse animId (content ^. Sugar.iElse)
            & local (M.animIdPrefix .~ animId)
    where
        animId = WidgetIds.fromEntityId entityId & Widget.toAnimId
        entityId = pl ^. Sugar.plEntityId

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
    do
        rows <-
            (:)
            <$> makeIfThen If animId ifElse
            <*> makeElse animId (ifElse ^. Sugar.iElse)
        res <- renderRows (ExprGui.mParensId pl) ?? rows
        case rows of
            [if_, else_]
                | Lens.has (Sugar.iThen . hVal . noLet) ifElse
                && Lens.has (Sugar.iElse . hVal . Sugar._SimpleElse . noLet) ifElse ->
                do
                    hbox <-
                        Options.hbox
                        <*> maybe (pure id) (ResponsiveExpr.addParens ??) (ExprGui.mParensId pl)
                        ?? id
                    s <- Spacer.stdHSpace <&> Responsive.fromView
                    Options.tryWideLayout hbox
                        (if_ ^.. traverse <> [s, else_ ^. rKeyword, else_ ^. rPredicate, s, else_ ^. rResult])
                        res & pure
            _ -> pure res
    & stdWrapParentExpr pl
    where
        animId = WidgetIds.fromExprPayload pl & Widget.toAnimId
        noLet = Sugar._BodyLam . Sugar.lamFunc . Sugar.fBody . hVal . Sugar.bBody . Sugar._BinderTerm
