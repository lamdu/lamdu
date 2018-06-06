-- | A pass on the sugared AST to decide where to put parenthesis
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
    deriving (Eq, Show)

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
    BinderBody name i o (Maybe MinOpPrec -> Precedence (Maybe Int) -> a) ->
    BinderBody name i o a
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

precedenceOfIfElse ::
    IfElse name i o (Maybe MinOpPrec -> Precedence (Maybe Int) -> a) ->
    (Classifier, IfElse name i o a)
precedenceOfIfElse (IfElse (IfThen if_ then_ del) else_) =
    ( ParenIf Never (IfGreater 1)
    , IfElse
        (IfThen
            (if_ (Just 0) unambiguous)
            (then_ (Just 0) (Precedence (Just 0) Nothing)) -- then appears in end of first line
            del
        )
        (else_ ?? Just 0 ?? unambiguous)
    )

precedenceOfLabeledApply ::
    HasPrecedence name =>
    LabeledApply name i o (Maybe MinOpPrec -> Precedence (Maybe Int) -> a) ->
    (Classifier, LabeledApply name i o a)
precedenceOfLabeledApply apply@(LabeledApply func specialArgs annotatedArgs relayedArgs) =
    case specialArgs of
    Infix l r ->
        ( ParenIf (IfGreaterOrEqual prec) (IfGreater prec)
        , LabeledApply
            { _aFunc = func
            , _aSpecialArgs =
              Infix
              (l (Just 0) (Precedence Nothing (Just prec)))
              (r (Just appendOpPrec) (Precedence (Just prec) Nothing))
            , _aAnnotatedArgs = newAnnotatedArgs
            , _aRelayedArgs = relayedArgs
            }
        )
        where
            appendOpPrec
                | notBoxed = prec+1
                | otherwise = 0
            prec = func ^. afVar . bvNameRef . nrName & precedence
    Object arg | notBoxed ->
        ( ParenIf (IfGreater 13) (IfGreaterOrEqual 13)
        , LabeledApply
            { _aFunc = func
            , _aSpecialArgs = Object (arg (Just 13) (Precedence (Just 13) Nothing))
            , _aAnnotatedArgs = newAnnotatedArgs
            , _aRelayedArgs = relayedArgs
            }
        )
    _ -> (NeverParen, apply ?? Just 0 ?? unambiguous)
    where
        notBoxed = null annotatedArgs && null relayedArgs
        newAnnotatedArgs = annotatedArgs <&> (?? Just 0) <&> (?? unambiguous)

precedenceOfPrefixApply ::
    Apply (Maybe MinOpPrec -> Precedence (Maybe Int) -> expr) ->
    (Classifier, Body name i o expr)
precedenceOfPrefixApply (V.Apply f arg) =
    ( ParenIf (IfGreater 13) (IfGreaterOrEqual 13)
    , V.Apply
        (f (Just 0) (Precedence Nothing (Just 13)))
        (arg (Just 13) (Precedence (Just 13) Nothing))
        & BodySimpleApply
    )

precedenceOf ::
    HasPrecedence name =>
    Body name i o (Maybe MinOpPrec -> Precedence (Maybe Int) -> a) ->
    (Classifier, Body name i o a)
precedenceOf =
    \case
    BodyPlaceHolder        -> (NeverParen, BodyPlaceHolder)
    BodyLiteral x          -> (NeverParen, BodyLiteral x)
    BodyGetVar x           -> (NeverParen, BodyGetVar x)
    BodyHole x             -> (NeverParen, BodyHole x)
    BodyFragment x          -> mkUnambiguous BodyFragment x
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
    BodyIfElse x           -> precedenceOfIfElse x & _2 %~ BodyIfElse
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
    Expression name i o a -> Expression name i o (MinOpPrec, NeedsParens, a)
add = addWith 0

addWith ::
    HasPrecedence name =>
    Int -> Expression name i o a ->
    Expression name i o (MinOpPrec, NeedsParens, a)
addWith minOpPrec = loop minOpPrec (Precedence 0 0)

loop ::
    HasPrecedence name =>
    MinOpPrec -> Precedence Int -> Expression name i o a ->
    Expression name i o (MinOpPrec, NeedsParens, a)
loop minOpPrecFromParent parentPrec (Expression pl body) =
    Expression (pl & plData %~ res) finalBody
    where
        f expr minOpPrecOverride override newParentPrec =
            loop (fromMaybe minOpPrecFromParent minOpPrecOverride)
            (fromMaybe <$> newParentPrec <*> override) expr
        Precedence parentBefore parentAfter = parentPrec
        res
            | haveParens = (,,) 0 NeedsParens
            | otherwise = (,,) minOpPrecFromParent NoNeedForParens
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
        (classifier, newBody) = precedenceOf (body <&> f)
