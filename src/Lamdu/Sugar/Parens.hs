-- | A pass on the sugared AST to decide where to put parenthesis
module Lamdu.Sugar.Parens
    ( NeedsParens(..)
    , MinOpPrec
    , add, addWith
    ) where

import qualified Control.Lens as Lens
import qualified Lamdu.Calc.Val as V
import           Lamdu.Precedence
    ( Prec, Precedence(..), HasPrecedence(..), before, after )
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

-- | Do we need parenthesis (OR any other visual disambiguation?)
data NeedsParens = NeedsParens | NoNeedForParens
    deriving (Eq, Show)

unambiguous :: Precedence Prec
unambiguous = Precedence 0 0

type MinOpPrec = Prec

add ::
    HasPrecedence name =>
    Expression name i o a -> Expression name i o (MinOpPrec, NeedsParens, a)
add = addWith 0

addWith ::
    HasPrecedence name =>
    Prec -> Expression name i o a ->
    Expression name i o (MinOpPrec, NeedsParens, a)
addWith minOpPrec = loop minOpPrec (Precedence 0 0)

simpleInfix :: LabeledApply name i o expr -> Maybe (expr, expr)
simpleInfix apply =
    case apply ^. aSpecialArgs of
    Infix l r
        | null (apply ^. aAnnotatedArgs)
        && null (apply ^. aRelayedArgs) -> Just (l, r)
    _ -> Nothing

loop ::
    HasPrecedence name =>
    MinOpPrec -> Precedence Prec -> Expression name i o a ->
    Expression name i o (MinOpPrec, NeedsParens, a)
loop minOpPrec parentPrec (Expression pl body_) =
    case body_ of
    BodyPlaceHolder        -> result False BodyPlaceHolder
    BodyLiteral x          -> result False (BodyLiteral x)
    BodyGetVar x           -> result False (BodyGetVar x)
    BodyHole x             -> result False (BodyHole x)
    BodyFragment x         -> mkUnambiguous BodyFragment x
    BodyRecord x           -> mkUnambiguous BodyRecord x
    BodyCase x             -> mkUnambiguous BodyCase x
    BodyLam x              -> leftSymbol Lens.mapped 0 BodyLam x
    BodyToNom x            -> leftSymbol (Lens.mapped . Lens.mapped) 0 BodyToNom x
    BodyInject x           -> leftSymbol Lens.mapped 0 BodyInject x
    BodyFromNom x          -> rightSymbol Lens.mapped 0 BodyFromNom x
    BodyGetField x         -> rightSymbol Lens.mapped 13 BodyGetField x
    BodySimpleApply x      ->
        BodySimpleApply V.Apply
        { V._applyFunc =
            loop 0 (childPrec (after .~ 13) needParens) f
        , V._applyArg =
            loop 13 (childPrec (before .~ 13) needParens) a
        } & result needParens
        where
            V.Apply f a = x
            needParens = parentPrec ^. before > 13 || parentPrec ^. after >= 13
    BodyLabeledApply x ->
        -- TODO: Use prism -- so we don't have to put ugly empty lists below
        case simpleInfix x of
        Nothing -> mkUnambiguous BodyLabeledApply x
        Just (l, r) ->
            BodyLabeledApply LabeledApply
            { _aFunc = func
            , _aSpecialArgs =
                Infix
                (loop 0 (childPrec (after .~ prec) needParens) l)
                (loop (prec+1) (childPrec (before .~ prec) needParens) r)
            , _aAnnotatedArgs = []
            , _aRelayedArgs = []
            } & result needParens
            where
                func = x ^. aFunc
                prec = func ^. afVar . bvNameRef . nrName & precedence
                needParens =
                    parentPrec ^. before >= prec || parentPrec ^. after > prec
    BodyIfElse x ->
        x <&> loop 0 unambiguous
        & BodyIfElse
        & result needParens
        where
            needParens = parentPrec ^. after > 1
    where
        result True = pl <&> (,,) 0 NeedsParens & Expression
        result False = pl <&> (,,) minOpPrec NoNeedForParens & Expression
        mkUnambiguous cons x =
            x <&> loop 0 unambiguous & cons & result False
        childPrec _ True = pure 0
        childPrec modify False = modify parentPrec
        leftSymbol = sideSymbol before after
        rightSymbol = sideSymbol after before
        sideSymbol overrideSide checkSide lens prec cons x =
            x & lens %~ loop prec (childPrec (overrideSide .~ prec) needParens) & cons
            & result needParens
            where
                needParens = parentPrec ^. checkSide > prec
