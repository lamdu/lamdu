-- | A pass on the sugared AST to decide where to put parenthesis
module Lamdu.Sugar.Parens
    ( NeedsParens(..)
    , MinOpPrec
    , addToWorkArea, addToExprWith
    , -- Exposed for tests
      addToExpr
    ) where

import qualified Control.Lens as Lens
import qualified Lamdu.Calc.Val as V
import           Lamdu.Precedence
    (Prec, Precedence(..), HasPrecedence(..), before, after, maxNamePrec)
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

-- | Do we need parenthesis (OR any other visual disambiguation?)
data NeedsParens = NeedsParens | NoNeedForParens
    deriving (Eq, Show)

unambiguous :: Precedence Prec
unambiguous = Precedence 0 0

type MinOpPrec = Prec

addToWorkArea ::
    HasPrecedence name =>
    WorkArea name i o a ->
    WorkArea name i o (MinOpPrec, NeedsParens, a)
addToWorkArea w =
    w
    { _waRepl =
        w ^. waRepl &
        replExpr . SugarLens.binderExprs %~ addToExpr
    , _waPanes =
        w ^. waPanes <&>
        paneDefinition . drBody . _DefinitionBodyExpression .
        deContent . SugarLens.assignmentExprs %~ addToExpr
    }

addToExpr ::
    HasPrecedence name =>
    Expression name i o a ->
    Expression name i o (MinOpPrec, NeedsParens, a)
addToExpr = addToExprWith 0

addToExprWith ::
    HasPrecedence name =>
    Prec -> Expression name i o a ->
    Expression name i o (MinOpPrec, NeedsParens, a)
addToExprWith minOpPrec = loop minOpPrec (Precedence 0 0)

bareInfix ::
    Lens.Prism' (LabeledApply name i o a)
    ( Expression name i o a
    , Node (BinderVarRef name o) a
    , Expression name i o a
    )
bareInfix =
    Lens.prism toLabeledApply fromLabeledApply
    where
        toLabeledApply (l, f, r) = LabeledApply f (Infix l r) [] []
        fromLabeledApply (LabeledApply f (Infix l r) [] []) = Right (l, f, r)
        fromLabeledApply a = Left a

loop ::
    HasPrecedence name =>
    MinOpPrec -> Precedence Prec -> Expression name i o a ->
    Expression name i o (MinOpPrec, NeedsParens, a)
loop minOpPrec parentPrec (PNode (Node pl body_)) =
    Node (resPrec, parens, pl) newBody & PNode
    where
        (resPrec, parens, newBody) = loopExprBody minOpPrec parentPrec body_

loopExprBody ::
    HasPrecedence name =>
    MinOpPrec -> Precedence Prec -> Body name i o a ->
    (MinOpPrec, NeedsParens, Body name i o (MinOpPrec, NeedsParens, a))
loopExprBody minOpPrec parentPrec body_ =
    case body_ of
    BodyPlaceHolder    -> result False BodyPlaceHolder
    BodyLiteral      x -> result False (BodyLiteral x)
    BodyGetVar       x -> result False (BodyGetVar x)
    BodyHole         x -> result False (BodyHole x)
    BodyFragment     x -> mkUnambiguous fExpr BodyFragment x
    BodyRecord       x -> mkUnambiguous Lens.mapped BodyRecord x
    BodyCase         x -> mkUnambiguous Lens.mapped BodyCase x
    BodyLam          x -> leftSymbol (lamFunc . SugarLens.funcExprs) 0 BodyLam x
    BodyToNom        x -> leftSymbol (Lens.mapped . SugarLens.binderExprs) 0 BodyToNom x
    BodyInject       x -> inject x
    BodyFromNom      x -> rightSymbol Lens.mapped 0 BodyFromNom x
    BodyGetField     x -> rightSymbol Lens.mapped 13 BodyGetField x
    BodySimpleApply  x -> simpleApply x
    BodyLabeledApply x -> labeledApply x
    BodyIfElse       x -> ifElse x
    where
        result True = (,,) 0 NeedsParens
        result False = (,,) minOpPrec NoNeedForParens
        mkUnambiguous l cons x =
            x & l %~ loop 0 unambiguous & cons & result False
        childPrec _ True = pure 0
        childPrec modify False = modify parentPrec
        leftSymbol = sideSymbol before after
        rightSymbol = sideSymbol after before
        sideSymbol overrideSide checkSide lens prec cons x =
            x & lens %~ loop prec (childPrec (overrideSide .~ prec) needParens) & cons
            & result needParens
            where
                needParens = parentPrec ^. checkSide > prec
        neverParen = (,,) (maxNamePrec + 1) NoNeedForParens
        inject (Inject t v) =
            case v of
            InjectNullary x -> x <&> neverParen & InjectNullary
            InjectVal x -> loop 0 (childPrec (before .~ 0) needParens) x & InjectVal
            & Inject t & BodyInject
            & result needParens
            where
                needParens =
                    case v of
                    InjectNullary{} -> False
                    InjectVal{} -> parentPrec ^. after > 0
        simpleApply (V.Apply f a) =
            BodySimpleApply V.Apply
            { V._applyFunc =
                loop 0 (childPrec (after .~ 13) needParens) f
            , V._applyArg =
                loop 13 (childPrec (before .~ 13) needParens) a
            } & result needParens
            where
                needParens = parentPrec ^. before > 13 || parentPrec ^. after >= 13
        labeledApply x =
            case x ^? bareInfix of
            Nothing ->
                SugarLens.overLabeledApplyChildren
                (<&> neverParen) (<&> neverParen) (loop 0 unambiguous) x
                & BodyLabeledApply & result False
            Just b -> simpleInfix b
        simpleInfix (l, func, r) =
            bareInfix #
            ( loop 0 (childPrec (after .~ prec) needParens) l
            , func <&> neverParen
            , loop (prec+1) (childPrec (before .~ prec) needParens) r
            ) & BodyLabeledApply & result needParens
            where
                prec = func ^. val . bvNameRef . nrName & precedence
                needParens =
                    parentPrec ^. before >= prec || parentPrec ^. after > prec
        ifElse x =
            x & SugarLens.ifElseChildren %~ loop 0 unambiguous
            & BodyIfElse
            & result needParens
            where
                needParens = parentPrec ^. after > 1
