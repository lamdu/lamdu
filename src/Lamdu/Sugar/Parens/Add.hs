-- | A pass on the sugared AST to decide where to put parenthesis
{-# LANGUAGE NoImplicitPrelude, LambdaCase, TemplateHaskell #-}
module Lamdu.Sugar.Parens.Add
    ( NeedsParens(..)
    , add
    ) where

import qualified Control.Lens as Lens
import qualified Lamdu.Calc.Val as V
import qualified Lamdu.CharClassification as Chars
import           Lamdu.Sugar.Names.Types (Name)
import qualified Lamdu.Sugar.Names.Types as Name
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

data Precedence = Precedence
    { _before :: {-# UNPACK #-}!Int
    , _after  :: {-# UNPACK #-}!Int
    } deriving Show
Lens.makeLenses ''Precedence

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

-- First "line" gets specified l/r precedence overrides.
-- Rest of "lines" get 0/0 (unambiguous) override
binderBodyFirstLine ::
    Maybe Int -> Maybe Int -> BinderBody name m (Maybe Int -> Maybe Int -> a) ->
    BinderBody name m a
binderBodyFirstLine l r =
    bbContent %~ f
    where
        f (BinderLet let_) =
            BinderLet let_
            { _lValue = _lValue let_ & bBody %~ binderBodyFirstLine l r
            , _lBody = _lBody let_ <&> (\expr -> expr (Just 0) (Just 0))
            }
        f (BinderExpr expr) =
            expr l r
            & BinderExpr

mkUnambiguous ::
    Functor sugar =>
    (sugar a -> b) -> sugar (Maybe Int -> Maybe Int -> a) -> (Classifier, b)
mkUnambiguous cons x =
    (NeverParen, cons (x ?? Just 0 ?? Just 0))

precedenceOfGuard ::
    Guard m (Maybe Int -> Maybe Int -> a) -> (Classifier, Guard m a)
precedenceOfGuard (Guard if_ then_ elseIfs else_ _del) =
    ( ParenIf Never (IfGreater 1)
    , Guard
        (if_ (Just 0) (Just 0))
        (then_ (Just 0) Nothing) -- then appears in end of first line
        (elseIfs <&> (\expr -> expr ?? Just 0 ?? Just 0))
        (else_ (Just 0) (Just 0))
        _del
    )

visibleFuncName :: Lens.Getter (BinderVar (Name n) m) Text
visibleFuncName = bvNameRef . nrName . Name.form . Lens.to Name.visible . Lens._1

precedenceOfLabeledApply ::
    LabeledApply (Name n) m (Maybe Int -> Maybe Int -> a) ->
    (Classifier, LabeledApply (Name n) m a)
precedenceOfLabeledApply apply@(LabeledApply func specialArgs annotatedArgs relayedArgs) =
    case specialArgs of
    NoSpecialArgs -> (NeverParen, apply ?? Just 0 ?? Just 0)
    ObjectArg arg ->
        ( ParenIf (IfGreater 10) (IfGreaterOrEqual 10)
        , LabeledApply func (ObjectArg (arg (Just 10) Nothing))
            newAnnotatedArgs relayedArgs
        )
    InfixArgs l r ->
        ( ParenIf (IfGreaterOrEqual prec) (IfGreater prec)
        , LabeledApply func
            (InfixArgs (l Nothing (Just prec)) (r (Just prec) Nothing))
            newAnnotatedArgs relayedArgs
        )
        where
            prec =
                func ^? visibleFuncName . Lens.ix 0
                & maybe 20 Chars.precedence
    where
        markUnambiguous expr = expr (Just 0) (Just 0)
        newAnnotatedArgs = annotatedArgs <&> fmap markUnambiguous

precedenceOfPrefixApply ::
    Apply (Maybe Int -> Maybe Int -> expr) -> (Classifier, Body name m expr)
precedenceOfPrefixApply (V.Apply f arg) =
    ( ParenIf (IfGreater 10) (IfGreaterOrEqual 10)
    , V.Apply (f Nothing (Just 10)) (arg (Just 10) Nothing)
        & BodySimpleApply
    )

precedenceOf ::
    Body (Name n) m (Maybe Int -> Maybe Int -> a) -> (Classifier, Body (Name n) m a)
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
        , x & lamBinder . bBody %~ binderBodyFirstLine (Just 1) Nothing & BodyLam
        )
    BodyFromNom x          -> rightSymbol 1 BodyFromNom x
    BodyToNom x            ->
        ( ParenIf Never (IfGreaterOrEqual 1)
        , x <&> binderBodyFirstLine (Just 1) Nothing & BodyToNom
        )
    BodyInject x           -> leftSymbol 1 BodyInject x
    BodyGetField x         -> rightSymbol 11 BodyGetField x
    BodySimpleApply x      -> precedenceOfPrefixApply x
    BodyLabeledApply x     -> precedenceOfLabeledApply x & _2 %~ BodyLabeledApply
    BodyGuard x            -> precedenceOfGuard x & _2 %~ BodyGuard
    where
        leftSymbol prec cons x =
            ( ParenIf Never (IfGreaterOrEqual prec)
            , cons (x ?? Just 0 ?? Nothing)
            )
        rightSymbol prec cons x =
            ( ParenIf (IfGreaterOrEqual prec) Never
            , cons (x ?? Nothing ?? Just 0)
            )

add :: Expression (Name n) m a -> Expression (Name n) m (NeedsParens, a)
add = loop (Precedence 0 0)

loop ::
    Precedence -> Expression (Name n) m a ->
    Expression (Name n) m (NeedsParens, a)
loop parentPrec (Expression body pl) =
    Expression finalBody (pl & plData %~ (,) needsParens)
    where
        f expr lOverride rOverride newParentPrec =
            newParentPrec
            & maybe id (before .~) lOverride
            & maybe id (after  .~) rOverride
            & (loop ?? expr)
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
