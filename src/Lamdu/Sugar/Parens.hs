-- | A pass on the sugared AST to decide where to put parenthesis
{-# LANGUAGE TypeApplications, TypeFamilies, DefaultSignatures, ScopedTypeVariables #-}
module Lamdu.Sugar.Parens
    ( MinOpPrec
    , addToWorkArea, addToTopLevel
    ) where

import qualified Control.Lens as Lens
import           Hyper
import           Hyper.Recurse (Recursive(..), proxyArgument)
import qualified Lamdu.Calc.Term as V
import           Lamdu.Precedence (Prec, Precedence(..), HasPrecedence(..), before, after)
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

-- | Do we need parenthesis (OR any other visual disambiguation?)
type NeedsParens = Bool
type MinOpPrec = Prec

addToWorkArea ::
    HasPrecedence name =>
    WorkArea v name i o a ->
    WorkArea v name i o (ParenInfo, a)
addToWorkArea w =
    w
    { _waRepl = w ^. waRepl & replExpr %~ addToTopLevel 0
    , _waPanes = w ^. waPanes <&> SugarLens.paneBinder %~ addToTopLevel 0
    }

unambiguousChild :: h # expr -> (Const (MinOpPrec, Precedence Prec) :*: h) # expr
unambiguousChild = (Const (0, Precedence 0 0) :*:)

unambiguousBody :: HFunctor expr => (expr # h) -> expr # (Const (MinOpPrec, Precedence Prec) :*: h)
unambiguousBody = hmap (const unambiguousChild)

class GetPrec h where
    getPrec :: HasPrecedence name => h # Const (BinderVarRef name o) -> Prec

instance GetPrec (Ann a) where
    getPrec = precedence . (^. hVal . Lens._Wrapped . bvNameRef . nrName)

class HFunctor expr => AddParens expr where
    parenInfo ::
        GetPrec h =>
        Precedence Prec -> expr # h ->
        (NeedsParens, expr # (Const (MinOpPrec, Precedence Prec) :*: h))
    parenInfo _ = (,) False . unambiguousBody

    addParensRecursive :: Proxy expr -> Dict (HNodesConstraint expr AddParens)
    default addParensRecursive ::
        HNodesConstraint expr AddParens =>
        Proxy expr -> Dict (HNodesConstraint expr AddParens)
    addParensRecursive _ = Dict

instance Recursive AddParens where
    recurse = addParensRecursive . proxyArgument

addToTopLevel :: AddParens expr => MinOpPrec -> Annotated a # expr -> Annotated (ParenInfo, a) # expr
addToTopLevel = (`addToNode` Precedence 0 0)

addToNode ::
    forall expr a.
    AddParens expr =>
    MinOpPrec -> Precedence Prec -> Annotated a # expr -> Annotated (ParenInfo, a) # expr
addToNode minOpPrec parentPrec (Ann (Const pl) b0) =
    withDict (recurse (Proxy @(AddParens expr))) $
    hmap (Proxy @AddParens #> \(Const (opPrec, prec) :*: x) -> addToNode opPrec prec x) b1
    & Ann (Const (ParenInfo minOpPrec r, pl))
    where
        (r, b1) = parenInfo parentPrec b0

instance AddParens (Const a)
instance HasPrecedence name => AddParens (Else v name i o)
instance HasPrecedence name => AddParens (PostfixFunc v name i o)

instance HasPrecedence name => AddParens (Assignment v name i o) where
    parenInfo parentPrec (BodyPlain x) = apBody (parenInfo parentPrec) x & _2 %~ BodyPlain
    parenInfo _ (BodyFunction x) = (False, unambiguousBody x & BodyFunction)

instance HasPrecedence name => AddParens (Binder v name i o) where
    parenInfo parentPrec (BinderTerm x) = parenInfo parentPrec x & _2 %~ BinderTerm
    parenInfo _ (BinderLet x) = (False, unambiguousBody x & BinderLet)

instance HasPrecedence name => AddParens (Term v name i o) where
    parenInfo parentPrec =
        \case
        BodyRecord       x -> (False, unambiguousBody x & BodyRecord)
        BodyPostfixFunc  x -> (parentPrec ^. before >= 12, unambiguousBody x & BodyPostfixFunc)
        BodyLam          x -> (parentPrec ^. after > 0, unambiguousBody x & BodyLam)
        BodyToNom        x -> (parentPrec ^. after > 0, unambiguousBody x & BodyToNom)
        BodySimpleApply  x -> simpleApply x
        BodyLabeledApply x -> labeledApply x
        BodyPostfixApply x -> postfixApply x
        BodyIfElse       x -> (parentPrec ^. after > 1, unambiguousBody x & BodyIfElse)
        BodyFragment     x -> (True, x & fExpr %~ (Const (13, pure 1) :*:) & BodyFragment)
        BodyNullaryInject x -> (False, unambiguousBody x & BodyNullaryInject)
        BodyLeaf         x ->
            -- A quite hacky rule for inject
            (Lens.has _LeafInject x && parentPrec ^. after /= 13, BodyLeaf x)
        where
            simpleApply (V.App f a) =
                ( needParens
                , BodySimpleApply V.App
                    { V._appFunc = Const (0, p & after .~ 13) :*: f
                    , V._appArg = Const (13, p & before .~ 13) :*: a
                    }
                )
                where
                    needParens = parentPrec ^. before > 13 || parentPrec ^. after >= 13
                    p = newParentPrec needParens
            newParentPrec needParens
                | needParens = pure 0
                | otherwise = parentPrec
            labeledApply x =
                maybe (False, BodyLabeledApply (unambiguousBody x)) simpleInfix (x ^? bareInfix)
            simpleInfix (func, OperatorArgs l r) =
                ( needParens
                , bareInfix #
                    ( unambiguousChild func
                    , OperatorArgs
                        (Const (0, p & after .~ prec) :*: l)
                        (Const (prec+1, p & before .~ prec) :*: r)
                    ) & BodyLabeledApply
                )
                where
                    prec = getPrec func
                    needParens = parentPrec ^. before >= prec || parentPrec ^. after > prec
                    p = newParentPrec needParens
            postfixApply (PostfixApply a f) =
                ( needParens
                , BodyPostfixApply PostfixApply
                    { _pArg = Const (0, p & after .~ 12) :*: a
                    , _pFunc = Const (13, Precedence 0 0) :*: f
                    }
                )
                where
                    needParens = parentPrec ^. before >= 13 || parentPrec ^. after > 12
                    p = newParentPrec needParens

bareInfix ::
    Lens.Prism' (LabeledApply v name i o # h)
    ( h # Const (BinderVarRef name o)
    , OperatorArgs v name i o # h
    )
bareInfix =
    Lens.prism' toLabeledApply fromLabeledApply
    where
        toLabeledApply (f, a) = LabeledApply f (Just a) [] []
        fromLabeledApply (LabeledApply f (Just a) [] []) = Just (f, a)
        fromLabeledApply _ = Nothing
