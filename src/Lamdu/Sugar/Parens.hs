-- | A pass on the sugared AST to decide where to put parenthesis
module Lamdu.Sugar.Parens
    ( NeedsParens(..)
    , MinOpPrec
    , add, addWith
    ) where

import qualified Control.Lens as Lens
import qualified Lamdu.Calc.Val as V
import           Lamdu.Precedence (Prec, Precedence(..), HasPrecedence(..))
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

-- | Do we need parenthesis (OR any other visual disambiguation?)
data NeedsParens = NeedsParens | NoNeedForParens
    deriving (Eq, Show)

data PrecCheck = Never | IfGreater !Prec | IfGreaterOrEqual !Prec

check :: PrecCheck -> Prec -> Bool
check Never = const False
check (IfGreater x) = (> x)
check (IfGreaterOrEqual x) = (>= x)

data Classifier
    = NeverParen
    | ParenIf PrecCheck PrecCheck

unambiguous :: Precedence (Maybe Prec)
unambiguous = Precedence (Just 0) (Just 0)

type MinOpPrec = Prec

unambiguousContext :: (Maybe Prec -> Precedence (Maybe Prec) -> a) -> a
unambiguousContext x = x (Just 0) unambiguous

mkUnambiguous ::
    Functor sugar =>
    (sugar a -> b) ->
    sugar (Maybe MinOpPrec -> Precedence (Maybe Prec) -> a) -> (Classifier, b)
mkUnambiguous cons x = (NeverParen, cons (x <&> unambiguousContext))

precedenceOfIfElse ::
    IfElse name i o (Maybe MinOpPrec -> Precedence (Maybe Prec) -> a) ->
    (Classifier, IfElse name i o a)
precedenceOfIfElse (IfElse (IfThen if_ then_ del) else_) =
    ( ParenIf Never (IfGreater 1)
    , IfElse
        (IfThen
            (if_ (Just 0) unambiguous)
            (then_ (Just 0) (Precedence (Just 0) Nothing)) -- then appears in end of first line
            del
        )
        (else_ <&> unambiguousContext)
    )

precedenceOfLabeledApply ::
    HasPrecedence name =>
    LabeledApply name i o (Maybe MinOpPrec -> Precedence (Maybe Prec) -> a) ->
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
    _ -> (NeverParen, apply <&> unambiguousContext)
    where
        notBoxed = null annotatedArgs && null relayedArgs
        newAnnotatedArgs = annotatedArgs <&> fmap unambiguousContext

precedenceOfPrefixApply ::
    Apply (Maybe MinOpPrec -> Precedence (Maybe Prec) -> expr) ->
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
    Body name i o (Maybe MinOpPrec -> Precedence (Maybe Prec) -> a) ->
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
    BodyLam x              -> leftSymbol Lens.mapped 0 BodyLam x
    BodyFromNom x          -> rightSymbol 0 BodyFromNom x
    BodyToNom x            -> leftSymbol (Lens.mapped . Lens.mapped) 0 BodyToNom x
    BodyInject x           -> leftSymbol Lens.mapped 0 BodyInject x
    BodyGetField x         -> rightSymbol 13 BodyGetField x
    BodySimpleApply x      -> precedenceOfPrefixApply x
    BodyLabeledApply x     -> precedenceOfLabeledApply x & _2 %~ BodyLabeledApply
    BodyIfElse x           -> precedenceOfIfElse x & _2 %~ BodyIfElse
    where
        leftSymbol lens prec cons x =
            ( ParenIf Never (IfGreater prec)
            , cons (x & lens %~ \expr -> expr (Just prec) (Precedence (Just prec) Nothing))
            )
        rightSymbol prec cons x =
            ( ParenIf (IfGreater prec) Never
            , cons (x ?? Just prec ?? Precedence Nothing (Just prec))
            )

add ::
    HasPrecedence name =>
    Expression name i o a -> Expression name i o (MinOpPrec, NeedsParens, a)
add = addWith 0

addWith ::
    HasPrecedence name =>
    Prec -> Expression name i o a ->
    Expression name i o (MinOpPrec, NeedsParens, a)
addWith minOpPrec = loop minOpPrec (Precedence 0 0)

loop ::
    HasPrecedence name =>
    MinOpPrec -> Precedence Prec -> Expression name i o a ->
    Expression name i o (MinOpPrec, NeedsParens, a)
loop minOpPrecFromParent parentPrec (Expression pl bod) =
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
            & bodyChildren %~
                ( $ if haveParens
                    then Precedence 0 0
                    else parentPrec
                )
        (classifier, newBody) = precedenceOf (bod & bodyChildren %~ f)
