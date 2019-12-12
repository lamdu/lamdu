-- | A pass on the sugared AST to decide where to put parenthesis
{-# LANGUAGE TypeApplications, TypeFamilies, RankNTypes, TypeOperators #-}
module Lamdu.Sugar.Parens
    ( NeedsParens(..)
    , MinOpPrec
    , addToWorkArea, addToExprWith
    , addToBinderWith
    ) where

import qualified Control.Lens as Lens
import           Hyper
import qualified Lamdu.Calc.Term as V
import           Lamdu.Precedence (Prec, Precedence(..), HasPrecedence(..), before, after)
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
        <&> paneBody . _PaneDefinition . drBody . _DefinitionBodyExpression . deContent
        %~ addToNode
    }

class AddParens expr where
    addToBody :: expr # Ann (Const a) -> expr # Ann (Const (MinOpPrec, NeedsParens, a))

    addToNode :: Annotated a expr -> Annotated (MinOpPrec, NeedsParens, a) expr
    addToNode (Ann (Const pl) x) = Ann (Const (0, NoNeedForParens, pl)) (addToBody x)

instance HasPrecedence name => AddParens (Assignment name i o) where
    addToBody (BodyFunction x) = x & fBody %~ addToNode & BodyFunction
    addToBody (BodyPlain x) = x & apBody %~ addToBody & BodyPlain

addToBinderWith ::
    HasPrecedence name =>
    MinOpPrec ->
    Annotated a (Binder name i o) ->
    Annotated (MinOpPrec, NeedsParens, a) (Binder name i o)
addToBinderWith minOpPrec (Ann (Const pl) x) =
    addToBody x
    & Ann (Const (minOpPrec, NoNeedForParens, pl))

instance HasPrecedence name => AddParens (Else name i o) where
    addToBody (SimpleElse expr) = addToBody expr & SimpleElse
    addToBody (ElseIf elseIf) = elseIf & eiContent %~ addToBody & ElseIf

instance HasPrecedence name => AddParens (IfElse name i o) where
    addToBody = hmap (Proxy @AddParens #> addToNode)

instance HasPrecedence name => AddParens (Binder name i o) where
    addToBody (BinderExpr x) = addToBody x & BinderExpr
    addToBody (BinderLet x) =
        hmap (Proxy @AddParens #> addToNode) x & BinderLet

instance HasPrecedence name => AddParens (Body name i o) where
    addToBody = loopExprBody unambiguous <&> (^. _2)
    addToNode = addToExprWith 0

instance AddParens (Const a) where
    addToBody (Const x) = Const x
    addToNode (Ann (Const pl) (Const x)) =
        Ann (Const (0, NoNeedForParens, pl)) (Const x)

addToExprWith ::
    HasPrecedence name =>
    MinOpPrec ->
    Expression name i o a ->
    Expression name i o (MinOpPrec, NeedsParens, a)
addToExprWith minOpPrec = loopExpr minOpPrec (Precedence 0 0)

bareInfix ::
    Lens.Prism' (LabeledApply name i o # Ann (Const a))
    ( Expression name i o a
    , Annotated a (Const (BinderVarRef name o))
    , Expression name i o a
    )
bareInfix =
    Lens.prism' toLabeledApply fromLabeledApply
    where
        toLabeledApply (l, f, r) = LabeledApply f (Operator l r) [] []
        fromLabeledApply (LabeledApply f (Operator l r) [] []) = Just (l, f, r)
        fromLabeledApply _ = Nothing

type AnnotateAST a body =
    MinOpPrec -> Precedence Prec ->
    Annotated a body ->
    Annotated (MinOpPrec, NeedsParens, a) body

loopExpr ::  HasPrecedence name => AnnotateAST a (Body name i o)
loopExpr minOpPrec parentPrec (Ann (Const pl) body_) =
    Ann (Const (minOpPrec, parens, pl)) newBody
    where
        (parens, newBody) = loopExprBody parentPrec body_

type SideSymbol =
    forall s t res pl body.
    AnnotateAST pl body ->
    Lens.ASetter' (Precedence Prec) MinOpPrec ->
    Lens.Getting MinOpPrec (Precedence Prec) MinOpPrec ->
    Lens.ASetter s t (Annotated pl body) (Annotated (MinOpPrec, NeedsParens, pl) body) ->
    MinOpPrec -> (t -> res) -> s -> (NeedsParens, res)

loopExprBody ::
    HasPrecedence name =>
    Precedence Prec -> Body name i o # Ann (Const a) ->
    (NeedsParens, Body name i o # Ann (Const (MinOpPrec, NeedsParens, a)))
loopExprBody parentPrec body_ =
    case body_ of
    BodyPlaceHolder    -> result False BodyPlaceHolder
    BodyLiteral      x -> result False (BodyLiteral x)
    BodyGetVar       x -> result False (BodyGetVar x)
    BodyFromNom      x -> result False (BodyFromNom x)
    BodyHole         x -> result False (BodyHole x)
    BodyFragment     x -> rightSymbol fExpr 12 BodyFragment x
    BodyRecord       x -> hmap (p #> addToNode) x & BodyRecord & result False
    BodyCase         x -> hmap (p #> addToNode) x & BodyCase & result False
    BodyLam          x -> leftSymbol (lamFunc . fBody) 0 BodyLam x
    BodyToNom        x -> leftSymbol Lens.mapped 0 BodyToNom x
    BodyInject       x -> inject x
    BodyGetField     x -> rightSymbol Lens.mapped 12 BodyGetField x
    BodySimpleApply  x -> simpleApply x
    BodyLabeledApply x -> labeledApply x
    BodyIfElse       x -> ifElse x
    where
        p = Proxy @AddParens
        result True = (,) NeedsParens
        result False = (,) NoNeedForParens
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
        newParentPrec needParens
            | needParens = pure 0
            | otherwise = parentPrec
        simpleApply (V.App f a) =
            BodySimpleApply V.App
            { V._appFunc =
                loopExpr 0 (newParentPrec needParens & after .~ 13) f
            , V._appArg =
                loopExpr 13 (newParentPrec needParens & before .~ 13) a
            } & result needParens
            where
                needParens = parentPrec ^. before > 13 || parentPrec ^. after >= 13
        labeledApply x =
            case x ^? bareInfix of
            Nothing ->
                hmap (p #> addToNode) x
                & BodyLabeledApply & result False
            Just b -> simpleInfix b
        simpleInfix (l, func, r) =
            bareInfix #
            ( loopExpr 0 (newParentPrec needParens & after .~ prec) l
            , addToNode func
            , loopExpr (prec+1) (newParentPrec needParens & before .~ prec) r
            ) & BodyLabeledApply & result needParens
            where
                prec = func ^. hVal . Lens._Wrapped . bvNameRef . nrName & precedence
                needParens =
                    parentPrec ^. before >= prec || parentPrec ^. after > prec
        ifElse x =
            addToBody x & BodyIfElse & result (parentPrec ^. after > 1)
