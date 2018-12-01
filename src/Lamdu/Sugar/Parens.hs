-- | A pass on the sugared AST to decide where to put parenthesis
module Lamdu.Sugar.Parens
    ( NeedsParens(..)
    , MinOpPrec
    , addToWorkArea, addToExprWith
    , addToBinderWith
    , -- Exposed for tests
      addToExpr
    ) where

import           AST (Node, LeafNode)
import           AST.Ann (Ann(..), val, annotations)
import qualified Control.Lens as Lens
import qualified Lamdu.Calc.Term as V
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
    { _waRepl = w ^. waRepl & replExpr %~ addToNode
    , _waPanes =
        w ^. waPanes
        <&> paneDefinition . drBody . _DefinitionBodyExpression . deContent
        %~ addToNode
    }

class AddParens expr where
    addToBody :: expr (Ann a) -> expr (Ann (MinOpPrec, NeedsParens, a))

    addToNode :: Node (Ann a) expr -> Node (Ann (MinOpPrec, NeedsParens, a)) expr
    addToNode (Ann pl x) = Ann (0, NoNeedForParens, pl) (addToBody x)

instance HasPrecedence name => AddParens (AssignmentBody name i o) where
    addToBody (BodyFunction x) = x & fBody %~ addToNode & BodyFunction
    addToBody (BodyPlain x) = x & apBody %~ addToBody & BodyPlain

addToBinderWith ::
    HasPrecedence name =>
    Prec ->
    Node (Ann a) (Binder name i o) ->
    Node (Ann (MinOpPrec, NeedsParens, a)) (Binder name i o)
addToBinderWith minOpPrec (Ann pl x) =
    addToBody x
    & Ann (minOpPrec, NoNeedForParens, pl)

unambiguousBody ::
    HasPrecedence name =>
    Body name i o (Ann a) ->
    Body name i o (Ann (MinOpPrec, NeedsParens, a))
unambiguousBody x =
    -- NOTE: In "0 unambiguous" case, the expr body is necessarily
    -- without parens and cannot reduce minOpPrec further, so ignore
    -- them:
    loopExprBody 0 unambiguous x ^. _3

instance HasPrecedence name => AddParens (Else name i o) where
    addToBody (SimpleElse expr) = unambiguousBody expr & SimpleElse
    addToBody (ElseIf elseIf) =
        elseIf
        & eiContent %~
        SugarLens.overIfElseChildren addToNode (loopExpr 0 unambiguous)
        & ElseIf

instance HasPrecedence name => AddParens (Binder name i o) where
    addToBody (BinderExpr x) = unambiguousBody x & BinderExpr
    addToBody (BinderLet x) =
        BinderLet x
        { _lValue = x ^. lValue & addToNode
        , _lBody = x ^. lBody & addToNode
        }

addToExpr ::
    HasPrecedence name =>
    Expression name i o a ->
    Expression name i o (MinOpPrec, NeedsParens, a)
addToExpr = addToExprWith 0

addToExprWith ::
    HasPrecedence name =>
    Prec ->
    Expression name i o a ->
    Expression name i o (MinOpPrec, NeedsParens, a)
addToExprWith minOpPrec = loopExpr minOpPrec (Precedence 0 0)

bareInfix ::
    Lens.Prism' (LabeledApply name i o (Ann a))
    ( Expression name i o a
    , LeafNode (Ann a) (BinderVarRef name o)
    , Expression name i o a
    )
bareInfix =
    Lens.prism toLabeledApply fromLabeledApply
    where
        toLabeledApply (l, f, r) = LabeledApply f (Infix l r) [] []
        fromLabeledApply (LabeledApply f (Infix l r) [] []) = Right (l, f, r)
        fromLabeledApply a = Left a

loopExpr ::
    HasPrecedence name =>
    MinOpPrec -> Precedence Prec -> Expression name i o a ->
    Expression name i o (MinOpPrec, NeedsParens, a)
loopExpr minOpPrec parentPrec (Ann pl body_) =
    Ann (resPrec, parens, pl) newBody
    where
        (resPrec, parens, newBody) = loopExprBody minOpPrec parentPrec body_

loopExprBody ::
    HasPrecedence name =>
    MinOpPrec -> Precedence Prec -> Body name i o (Ann a) ->
    (MinOpPrec, NeedsParens, Body name i o (Ann (MinOpPrec, NeedsParens, a)))
loopExprBody minOpPrec parentPrec body_ =
    case body_ of
    BodyPlaceHolder    -> result False BodyPlaceHolder
    BodyLiteral      x -> result False (BodyLiteral x)
    BodyGetVar       x -> result False (BodyGetVar x)
    BodyHole         x -> result False (BodyHole x)
    BodyFragment     x -> mkUnambiguous fExpr BodyFragment x
    BodyRecord       x -> mkUnambiguous Lens.mapped BodyRecord x
    BodyCase         x -> mkUnambiguous Lens.mapped BodyCase x
    BodyLam          x -> leftSymbol (lamFunc . fBody) 0 BodyLam x
    BodyToNom        x -> leftSymbol Lens.mapped 0 BodyToNom x
    BodyInject       x -> inject x
    BodyFromNom      x -> rightSymbol Lens.mapped 0 BodyFromNom x
    BodyGetField     x -> rightSymbol Lens.mapped 13 BodyGetField x
    BodySimpleApply  x -> simpleApply x
    BodyLabeledApply x -> labeledApply x
    BodyIfElse       x -> ifElse x
    where
        result True = (,,) minOpPrec NeedsParens
        result False = (,,) minOpPrec NoNeedForParens
        mkUnambiguous l cons x =
            x & l %~ loopExpr 0 unambiguous & cons & result False
        leftSymbol = sideSymbol (\_ _ -> addToNode) before after
        rightSymbol = sideSymbol loopExpr after before
        sideSymbol loop overrideSide checkSide lens prec cons x =
            x & lens %~ loop prec childPrec & cons
            & result needParens
            where
                needParens = parentPrec ^. checkSide > prec
                childPrec
                    | needParens = pure 0
                    | otherwise = parentPrec & overrideSide .~ prec
        neverParen = (,,) (maxNamePrec + 1) NoNeedForParens
        inject (Inject t v) =
            case v of
            InjectNullary x -> x & annotations %~ neverParen & InjectNullary & cons & result False
            InjectVal x -> sideSymbol loopExpr before after id 0 (cons . InjectVal) x
            where
                cons = BodyInject . Inject t
        simpleApply (V.Apply f a) =
            BodySimpleApply V.Apply
            { V._applyFunc =
                loopExpr 0 (newParentPrec & after .~ 13) f
            , V._applyArg =
                loopExpr 13 (newParentPrec & before .~ 13) a
            } & result needParens
            where
                needParens = parentPrec ^. before > 13 || parentPrec ^. after >= 13
                newParentPrec
                    | needParens = pure 0
                    | otherwise = parentPrec
        labeledApply x =
            case x ^? bareInfix of
            Nothing ->
                SugarLens.overLabeledApplyChildren
                (annotations %~ neverParen) (annotations %~ neverParen) (loopExpr 0 unambiguous) x
                & BodyLabeledApply & result False
            Just b -> simpleInfix b
        simpleInfix (l, func, r) =
            bareInfix #
            ( loopExpr 0 (newParentPrec & after .~ prec) l
            , func & annotations %~ neverParen
            , loopExpr (prec+1) (newParentPrec & before .~ prec) r
            ) & BodyLabeledApply & result needParens
            where
                prec = func ^. val . Lens._Wrapped . bvNameRef . nrName & precedence
                needParens =
                    parentPrec ^. before >= prec || parentPrec ^. after > prec
                newParentPrec
                    | needParens = pure 0
                    | otherwise = parentPrec
        ifElse x =
            x & SugarLens.overIfElseChildren addToNode (loopExpr 0 unambiguous)
            & BodyIfElse
            & result needParens
            where
                needParens = parentPrec ^. after > 1
