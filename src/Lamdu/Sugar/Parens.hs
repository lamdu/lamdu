-- | A pass on the sugared AST to decide where to put parenthesis
{-# LANGUAGE TypeApplications, TypeFamilies #-}
module Lamdu.Sugar.Parens
    ( MinOpPrec
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

type MinOpPrec = Prec

addToWorkArea ::
    HasPrecedence name =>
    WorkArea name i o a ->
    WorkArea name i o (ParenInfo, a)
addToWorkArea w =
    w
    { _waRepl = w ^. waRepl & replExpr %~ addToNode
    , _waPanes =
        w ^. waPanes
        <&> paneBody . _PaneDefinition . drBody . _DefinitionBodyExpression . deContent
        %~ addToNode
    }

class AddParens expr where
    addToBody :: expr # Annotated a -> expr # Annotated (ParenInfo, a)

    addToNode :: Annotated a # expr -> Annotated (ParenInfo, a) # expr
    addToNode (Ann (Const pl) x) = Ann (Const (ParenInfo 0 False, pl)) (addToBody x)

instance HasPrecedence name => AddParens (Assignment v name i o) where
    addToBody (BodyFunction x) = x & fBody %~ addToNode & BodyFunction
    addToBody (BodyPlain x) = x & apBody %~ addToBody & BodyPlain

addToBinderWith ::
    HasPrecedence name =>
    MinOpPrec ->
    Annotated a # Binder v name i o ->
    Annotated (ParenInfo, a) # Binder v name i o
addToBinderWith minOpPrec (Ann (Const pl) x) =
    addToBody x
    & Ann (Const (ParenInfo minOpPrec False, pl))

instance HasPrecedence name => AddParens (Else v name i o) where
    addToBody (SimpleElse expr) = addToBody expr & SimpleElse
    addToBody (ElseIf elseIf) = addToBody elseIf & ElseIf

instance HasPrecedence name => AddParens (IfElse name i o) where
    addToBody = hmap (Proxy @AddParens #> addToNode)

instance HasPrecedence name => AddParens (Binder v name i o) where
    addToBody (BinderTerm x) = addToBody x & BinderTerm
    addToBody (BinderLet x) =
        hmap (Proxy @AddParens #> addToNode) x & BinderLet

instance HasPrecedence name => AddParens (Term v name i o) where
    addToBody =
        loopExprBody unambiguous <&> (^. _2)
        where
            unambiguous = Precedence 0 0
    addToNode = addToExprWith 0

instance AddParens (Const a) where
    addToBody (Const x) = Const x
    addToNode (Ann (Const pl) (Const x)) =
        Ann (Const (ParenInfo 0 False, pl)) (Const x)

addToExprWith ::
    HasPrecedence name =>
    MinOpPrec ->
    Annotated a # Term v name i o ->
    Annotated (ParenInfo, a) # Term v name i o
addToExprWith minOpPrec = loopExpr minOpPrec (Precedence 0 0)

bareInfix ::
    Lens.Prism' (LabeledApply v name i o # Annotated a)
    ( Annotated a # Term v name i o
    , Annotated a # Const (BinderVarRef name o)
    , Annotated a # Term v name i o
    )
bareInfix =
    Lens.prism' toLabeledApply fromLabeledApply
    where
        toLabeledApply (l, f, r) = LabeledApply f (Operator l r) [] []
        fromLabeledApply (LabeledApply f (Operator l r) [] []) = Just (l, f, r)
        fromLabeledApply _ = Nothing

type AnnotateAST a body =
    MinOpPrec -> Precedence Prec ->
    Annotated a # body ->
    Annotated (ParenInfo, a) # body

loopExpr ::  HasPrecedence name => AnnotateAST a (Term v name i o)
loopExpr minOpPrec parentPrec (Ann (Const pl) body_) =
    Ann (Const (ParenInfo minOpPrec (parens == NeedsParens), pl)) newBody
    where
        (parens, newBody) = loopExprBody parentPrec body_

loopExprBody ::
    HasPrecedence name =>
    Precedence Prec -> Term v name i o # Annotated a ->
    (NeedsParens, Term v name i o # Annotated (ParenInfo, a))
loopExprBody parentPrec body_ =
    case body_ of
    BodyPlaceHolder    -> result False BodyPlaceHolder
    BodyLiteral      x -> result False (BodyLiteral x)
    BodyGetVar       x -> result False (BodyGetVar x)
    BodyFromNom      x -> result False (BodyFromNom x)
    BodyHole         x -> result False (BodyHole x)
    BodyFragment     x -> rightSymbol fExpr BodyFragment x
    BodyRecord       x -> hmap (p #> addToNode) x & BodyRecord & result False
    BodyCase         x -> hmap (p #> addToNode) x & BodyCase & result (caseNeedsParens x)
    BodyLam          x -> leftSymbol (lamFunc . fBody) 0 BodyLam x
    BodyToNom        x -> leftSymbol nVal 0 BodyToNom x
    BodyInject       x -> inject x
    BodyGetField     x -> rightSymbol gfRecord BodyGetField x
    BodySimpleApply  x -> simpleApply x
    BodyLabeledApply x -> labeledApply x
    BodyIfElse       x -> ifElse x
    where
        p = Proxy @AddParens
        result True = (,) NeedsParens
        result False = (,) NoNeedForParens
        leftSymbol ::
            AddParens body =>
            Lens.ASetter s t (Annotated pl # body) (Annotated (ParenInfo, pl) # body) ->
            MinOpPrec -> (t -> res) -> s -> (NeedsParens, res)
        leftSymbol l prec = sideSymbol (\_ _ -> addToNode) before (parentPrec ^. after > prec) l prec
        rightSymbol ::
            HasPrecedence name =>
            Lens.ASetter s t
            (Annotated pl # Term v name i o)
            (Annotated (ParenInfo, pl) # Term v name i o) ->
            (t -> res) -> s -> (NeedsParens, res)
        rightSymbol l =
            sideSymbol loopExpr after dotSomethingNeedParens l 12
        dotSomethingNeedParens = parentPrec ^. before >= 12 || parentPrec ^. after > 12
        caseNeedsParens x =
            case x ^. cKind of
            LambdaCase -> False
            CaseWithArg{} -> dotSomethingNeedParens
        sideSymbol ::
            AnnotateAST pl body ->
            Lens.ASetter' (Precedence Prec) MinOpPrec ->
            Bool ->
            Lens.ASetter s t (Annotated pl # body) (Annotated (ParenInfo, pl) # body) ->
            MinOpPrec -> (t -> res) -> s -> (NeedsParens, res)
        sideSymbol loop overrideSide needParens lens prec cons x =
            x & lens %~ loop prec childPrec & cons
            & result needParens
            where
                childPrec
                    | needParens = pure 0
                    | otherwise = parentPrec & overrideSide .~ prec
        inject (Inject t v) =
            case v of
            InjectNullary x -> addToNode x & InjectNullary & cons & result False
            InjectVal x -> sideSymbol loopExpr before (parentPrec ^. after > 0) id 0 (cons . InjectVal) x
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
