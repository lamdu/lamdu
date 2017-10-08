-- | A pass on the sugared AST to decide where to put parenthesis
{-# LANGUAGE NoImplicitPrelude, LambdaCase #-}
module Lamdu.Sugar.Parens
    ( NeedsParens(..)
    , MinOpPrec
    , add, addWith
    ) where

import qualified Lamdu.Calc.Val as V
import           Lamdu.Precedence (Precedence(..), HasPrecedence(..))
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

-- | Do we need parenthesis (OR any other visual disambiguation?)
data NeedsParens = NeedsParens | NoNeedForParens
    deriving (Eq)

data PrecCheck = Never | IfGreater !Int | IfGreaterOrEqual !Int

check :: PrecCheck -> Int -> Bool
check Never = const False
check (IfGreater x) = (> x)
check (IfGreaterOrEqual x) = (>= x)

data Classifier
    = NeverParen
    | ParenIf PrecCheck PrecCheck

unambiguous :: Precedence (Maybe Int)
unambiguous = Precedence (Just 0) (Just 0)

type MinOpPrec = Int

-- First "line" gets specified precedence override.
-- Rest of "lines" get 0/0 (unambiguous) override
binderBodyFirstLine ::
    Maybe MinOpPrec -> Precedence (Maybe Int) ->
    BinderBody name m (Maybe MinOpPrec -> Precedence (Maybe Int) -> a) ->
    BinderBody name m a
binderBodyFirstLine minOpPrecOverride override =
    bbContent %~ f
    where
        f (BinderLet let_) =
            BinderLet let_
            { _lValue = _lValue let_ & bBody %~ binderBodyFirstLine minOpPrecOverride override
            , _lBody = _lBody let_ <&> (\expr -> expr (Just 0) unambiguous)
            }
        f (BinderExpr expr) = expr minOpPrecOverride override & BinderExpr

mkUnambiguous ::
    Functor sugar =>
    (sugar a -> b) ->
    sugar (Maybe MinOpPrec -> Precedence (Maybe Int) -> a) -> (Classifier, b)
mkUnambiguous cons x = (NeverParen, cons (x ?? Just 0 ?? unambiguous))

precedenceOfGuard ::
    Guard m (Maybe MinOpPrec -> Precedence (Maybe Int) -> a) -> (Classifier, Guard m a)
precedenceOfGuard (Guard if_ then_ elseIfs else_ _del) =
    ( ParenIf Never (IfGreater 1)
    , Guard
        (if_ (Just 0) unambiguous)
        (then_ (Just 0) (Precedence (Just 0) Nothing)) -- then appears in end of first line
        (elseIfs <&> (\expr -> expr ?? Just 0 ?? unambiguous))
        (else_ (Just 0) unambiguous)
        _del
    )

binderName :: Lens (BinderVar namea m) (BinderVar nameb m) namea nameb
binderName = bvNameRef . nrName

precedenceOfLabeledApply ::
    HasPrecedence name =>
    LabeledApply name m (Maybe MinOpPrec -> Precedence (Maybe Int) -> a) ->
    (Classifier, LabeledApply name m a)
precedenceOfLabeledApply apply@(LabeledApply func specialArgs annotatedArgs relayedArgs) =
    case specialArgs of
    Infix l r ->
        ( ParenIf (IfGreaterOrEqual prec) (IfGreater prec)
        , LabeledApply func
            (Infix
             (l (Just 0) (Precedence Nothing (Just prec)))
             (r (Just appendOpPrec) (Precedence (Just prec) Nothing)))
            newAnnotatedArgs relayedArgs
        )
        where
            appendOpPrec
                | notBoxed = prec+1
                | otherwise = 0
            prec = func ^. binderName & precedence
    Object arg | notBoxed ->
        ( ParenIf (IfGreater 13) (IfGreaterOrEqual 13)
        , LabeledApply func (Object (arg (Just 13) (Precedence (Just 13) Nothing)))
            newAnnotatedArgs relayedArgs
        )
    _ -> (NeverParen, apply ?? Just 0 ?? unambiguous)
    where
        notBoxed = null annotatedArgs && null relayedArgs
        newAnnotatedArgs = annotatedArgs <&> (?? Just 0) <&> (?? unambiguous)

precedenceOfPrefixApply ::
    Apply (Maybe MinOpPrec -> Precedence (Maybe Int) -> expr) -> (Classifier, Body name m expr)
precedenceOfPrefixApply (V.Apply f arg) =
    ( ParenIf (IfGreater 13) (IfGreaterOrEqual 13)
    , V.Apply
        (f (Just 0) (Precedence Nothing (Just 13)))
        (arg (Just 13) (Precedence (Just 13) Nothing))
        & BodySimpleApply
    )

precedenceOf ::
    HasPrecedence name =>
    Body name m (Maybe MinOpPrec -> Precedence (Maybe Int) -> a) ->
    (Classifier, Body name m a)
precedenceOf =
    \case
    BodyInjectedExpression -> (NeverParen, BodyInjectedExpression)
    BodyLiteral x          -> (NeverParen, BodyLiteral x)
    BodyGetVar x           -> (NeverParen, BodyGetVar x)
    BodyHole x             -> mkUnambiguous BodyHole x
    BodyRecord x           -> mkUnambiguous BodyRecord x
    BodyCase x             -> mkUnambiguous BodyCase x
    BodyLam x              ->
        ( ParenIf Never (IfGreaterOrEqual 1)
        , x & lamBinder . bBody %~
          binderBodyFirstLine (Just 0) (Precedence (Just 1) Nothing) & BodyLam
        )
    BodyFromNom x          -> rightSymbol 1 BodyFromNom x
    BodyToNom x            ->
        ( ParenIf Never (IfGreaterOrEqual 1)
        , x <&> binderBodyFirstLine (Just 0) (Precedence (Just 1) Nothing) & BodyToNom
        )
    BodyInject x           -> leftSymbol 1 BodyInject x
    BodyGetField x         -> rightSymbol 14 BodyGetField x
    BodySimpleApply x      -> precedenceOfPrefixApply x
    BodyLabeledApply x     -> precedenceOfLabeledApply x & _2 %~ BodyLabeledApply
    BodyGuard x            -> precedenceOfGuard x & _2 %~ BodyGuard
    where
        leftSymbol prec cons x =
            ( ParenIf Never (IfGreaterOrEqual prec)
            , cons (x ?? Just 0 ?? Precedence (Just 0) Nothing)
            )
        rightSymbol prec cons x =
            ( ParenIf (IfGreaterOrEqual prec) Never
            , cons (x ?? Just 0 ?? Precedence Nothing (Just 0))
            )

add ::
    HasPrecedence name =>
    Expression name m a -> Expression name m (MinOpPrec, NeedsParens, a)
add = addWith 0

addWith ::
    HasPrecedence name =>
    Int -> Expression name m a -> Expression name m (MinOpPrec, NeedsParens, a)
addWith minOpPrec = loop minOpPrec (Precedence 0 0)

loop ::
    HasPrecedence name =>
    MinOpPrec -> Precedence Int -> Expression name m a ->
    Expression name m (MinOpPrec, NeedsParens, a)
loop minOpPrec parentPrec (Expression body pl) =
    Expression finalBody (pl & plData %~ (,,) minOpPrec needsParens)
    where
        f expr minOpPrecOverride override newParentPrec =
            loop (fromMaybe minOpPrec minOpPrecOverride)
            (fromMaybe <$> newParentPrec <*> override) expr
        Precedence parentBefore parentAfter = parentPrec
        needsParens
            | haveParens = NeedsParens
            | otherwise = NoNeedForParens
        haveParens =
            case classifier of
            NeverParen -> False
            ParenIf lCheck rCheck ->
                check lCheck parentBefore || check rCheck parentAfter
        finalBody =
            newBody
            ?? if haveParens
                 then Precedence 0 0
                 else parentPrec
        (classifier, newBody) =
            precedenceOf (body <&> f)
