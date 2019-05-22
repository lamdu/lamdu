-- | A pass on the sugared AST to decide where to put parenthesis
{-# LANGUAGE TypeApplications, TypeFamilies, RankNTypes #-}
module Lamdu.Sugar.Parens
    ( NeedsParens(..)
    , MinOpPrec
    , addToWorkArea, addToExprWith
    , addToBinderWith
    ) where

import           AST (Tree, overChildren)
import           AST.Knot.Ann (Ann(..), val)
import qualified Control.Lens as Lens
import           Data.Functor.Const (Const(..))
import qualified Lamdu.Calc.Term as V
import           Lamdu.Precedence
    (Prec, Precedence(..), HasPrecedence(..), before, after, maxNamePrec)
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
    addToBody :: Tree expr (Ann a) -> Tree expr (Ann (MinOpPrec, NeedsParens, a))

    addToNode :: Tree (Ann a) expr -> Tree (Ann (MinOpPrec, NeedsParens, a)) expr
    addToNode (Ann pl x) = Ann (0, NoNeedForParens, pl) (addToBody x)

instance HasPrecedence name => AddParens (Assignment name i o) where
    addToBody (BodyFunction x) = x & fBody %~ addToNode & BodyFunction
    addToBody (BodyPlain x) = x & apBody %~ addToBody & BodyPlain

addToBinderWith ::
    HasPrecedence name =>
    MinOpPrec ->
    Tree (Ann a) (Binder name i o) ->
    Tree (Ann (MinOpPrec, NeedsParens, a)) (Binder name i o)
addToBinderWith minOpPrec (Ann pl x) =
    addToBody x
    & Ann (minOpPrec, NoNeedForParens, pl)

instance HasPrecedence name => AddParens (Else name i o) where
    addToBody (SimpleElse expr) = addToBody expr & SimpleElse
    addToBody (ElseIf elseIf) = elseIf & eiContent %~ addToBody & ElseIf

instance HasPrecedence name => AddParens (IfElse name i o) where
    addToBody = overChildren (Proxy @AddParens) addToNode

instance HasPrecedence name => AddParens (Binder name i o) where
    addToBody (BinderExpr x) = addToBody x & BinderExpr
    addToBody (BinderLet x) =
        overChildren (Proxy @AddParens) addToNode x & BinderLet

instance HasPrecedence name => AddParens (Body name i o) where
    addToBody = loopExprBody unambiguous <&> (^. _2)
    addToNode = addToExprWith 0

instance AddParens (Const a) where
    addToBody (Const x) = Const x
    addToNode (Ann pl (Const x)) =
        Ann (maxNamePrec + 1, NoNeedForParens, pl) (Const x)

addToExprWith ::
    HasPrecedence name =>
    MinOpPrec ->
    Expression name i o a ->
    Expression name i o (MinOpPrec, NeedsParens, a)
addToExprWith minOpPrec = loopExpr minOpPrec (Precedence 0 0)

bareInfix ::
    Lens.Prism' (Tree (LabeledApply name i o) (Ann a))
    ( Expression name i o a
    , Tree (Ann a) (Lens.Const (BinderVarRef name o))
    , Expression name i o a
    )
bareInfix =
    Lens.prism toLabeledApply fromLabeledApply
    where
        toLabeledApply (l, f, r) = LabeledApply f (Infix l r) [] []
        fromLabeledApply (LabeledApply f (Infix l r) [] []) = Right (l, f, r)
        fromLabeledApply a = Left a

type AnnotateAST a body =
    MinOpPrec -> Precedence Prec ->
    Tree (Ann a) body ->
    Tree (Ann (MinOpPrec, NeedsParens, a)) body

loopExpr ::  HasPrecedence name => AnnotateAST a (Body name i o)
loopExpr minOpPrec parentPrec (Ann pl body_) =
    Ann (minOpPrec, parens, pl) newBody
    where
        (parens, newBody) = loopExprBody parentPrec body_

type SideSymbol =
    forall s t res pl body.
    AnnotateAST pl body ->
    Lens.ASetter' (Precedence Prec) MinOpPrec ->
    Lens.Getting MinOpPrec (Precedence Prec) MinOpPrec ->
    Lens.ASetter s t
    (Tree (Ann pl) body)
    (Tree (Ann (MinOpPrec, NeedsParens, pl)) body) ->
    MinOpPrec -> (t -> res) -> s -> (NeedsParens, res)

loopExprBody ::
    HasPrecedence name =>
    Precedence Prec -> Tree (Body name i o) (Ann a) ->
    (NeedsParens, Tree (Body name i o) (Ann (MinOpPrec, NeedsParens, a)))
loopExprBody parentPrec body_ =
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
        result True = (,) NeedsParens
        result False = (,) NoNeedForParens
        mkUnambiguous l cons x =
            x & l %~ loopExpr 0 unambiguous & cons & result False
        leftSymbol = sideSymbol (\_ _ -> addToNode) before after
        rightSymbol = sideSymbol loopExpr after before
        sideSymbol :: SideSymbol
        sideSymbol loop overrideSide checkSide lens prec cons x =
            x & lens %~ loop prec childPrec & cons
            & result needParens
            where
                needParens = parentPrec ^. checkSide > prec
                childPrec
                    | needParens = pure 0
                    | otherwise = parentPrec & overrideSide .~ prec
        inject (Inject t v) =
            case v of
            InjectNullary x -> addToNode x & InjectNullary & cons & result False
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
                overChildren (Proxy @AddParens) addToNode x
                & BodyLabeledApply & result False
            Just b -> simpleInfix b
        simpleInfix (l, func, r) =
            bareInfix #
            ( loopExpr 0 (newParentPrec & after .~ prec) l
            , addToNode func
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
            addToBody x & BodyIfElse & result (parentPrec ^. after > 1)
