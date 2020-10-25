{-# LANGUAGE TypeApplications, FlexibleInstances, DefaultSignatures #-}
module Lamdu.Sugar.Lens
    ( SugarExpr(..)
    , childPayloads
    , bodyUnfinished
    , defSchemes
    , assignmentBodyAddFirstParam
    , binderFuncParamActions
    , binderResultExpr
    , holeTransformExprs, holeOptionTransformExprs
    , getVarName
    , paneBinder
    , evalResults
    ) where

import           Control.Lens (Traversal)
import qualified Control.Lens as Lens
import           Hyper
import           Hyper.Recurse (Recursive(..), proxyArgument)
import           Hyper.Type.AST.App (appChildren)
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

childPayloads ::
    HTraversable expr =>
    Lens.Traversal' (expr # Annotated a) a
childPayloads f =
    htraverse (const (annotation f))

class HTraversable t => SugarExpr t where
    isUnfinished :: t f -> Bool
    isUnfinished _ = False

    isForbiddenInLightLam :: t f -> Bool
    isForbiddenInLightLam = isUnfinished

    sugarExprRecursive ::
        Proxy t -> Dict (HNodesConstraint t SugarExpr)
    default sugarExprRecursive ::
        HNodesConstraint t SugarExpr =>
        Proxy t -> Dict (HNodesConstraint t SugarExpr)
    sugarExprRecursive _ = Dict

instance Recursive SugarExpr where
    recurse = sugarExprRecursive . proxyArgument

instance SugarExpr (Const (GetVar name o))
instance SugarExpr (Const (NullaryVal name i o))

instance SugarExpr (Const (BinderVarRef name o)) where
    isUnfinished (Const x) = Lens.has binderVarRefUnfinished x

instance SugarExpr (Assignment v name i o) where
    isUnfinished (BodyPlain x) = isUnfinished (x ^. apBody)
    isUnfinished BodyFunction{} = False

instance SugarExpr (Else v name i o) where
    isUnfinished (SimpleElse x) = isUnfinished x
    isUnfinished ElseIf{} = False

instance SugarExpr (Function v name i o) where
    isForbiddenInLightLam = Lens.has (fParams . _Params)

instance SugarExpr (Binder v name i o) where
    isUnfinished (BinderTerm x) = isUnfinished x
    isUnfinished BinderLet{} = False
    isForbiddenInLightLam BinderLet{} = True
    isForbiddenInLightLam (BinderTerm x) = isForbiddenInLightLam x

instance SugarExpr (Term v name i o) where
    isUnfinished BodyHole{} = True
    isUnfinished BodyFragment{} = True
    isUnfinished (BodyGetVar (GetBinder x)) = isUnfinished (Const x)
    isUnfinished _ = False
    isForbiddenInLightLam (BodyLam f) = isForbiddenInLightLam (f ^. lamFunc)
    isForbiddenInLightLam x = isUnfinished x

binderVarRefUnfinished :: Lens.Traversal' (BinderVarRef name m) ()
binderVarRefUnfinished =
    bvForm . _GetDefinition . Lens.failing _DefDeleted (_DefTypeChanged . Lens.united)

bodyUnfinished :: Lens.Traversal' (Term v name i o # Ann a) ()
bodyUnfinished =
    _BodyHole . Lens.united
    & Lens.failing (_BodyFragment . Lens.united)
    & Lens.failing (_BodyGetVar . _GetBinder . binderVarRefUnfinished)
    & Lens.failing (_BodyLabeledApply . aFunc . hVal . Lens._Wrapped . binderVarRefUnfinished)

defBodySchemes :: Lens.Traversal' (DefinitionBody v name i o expr) (Scheme name)
defBodySchemes f (DefinitionBodyBuiltin b) =
    b & biType %%~ f
    <&> DefinitionBodyBuiltin
defBodySchemes f (DefinitionBodyExpression de) =
    de & deType %%~ f
    <&> DefinitionBodyExpression

defSchemes :: Lens.Traversal' (Definition v name i o expr) (Scheme name)
defSchemes = drBody . defBodySchemes

binderFuncParamActions ::
    Lens.Traversal' (BinderParams v name i o) (FuncParamActions name i o)
binderFuncParamActions _ (NullParam a) = pure (NullParam a)
binderFuncParamActions f (Params ps) = (traverse . _2 . piActions) f ps <&> Params

binderResultExpr ::
    Lens.IndexedLens' (Term v name i o # Annotated ()) (Annotated a # Binder v name i o) a
binderResultExpr f (Ann (Const pl) x) =
    case x of
    BinderTerm e ->
        Lens.indexed f
        (hmap (Proxy @(Recursively HFunctor) #> hflipped %~ hmap (\_ Const{} -> Const ())) e)
        pl
        <&> Const
        <&> (`Ann` x)
    BinderLet l ->
        lBody (binderResultExpr f) l
        <&> BinderLet
        <&> Ann (Const pl)

holeOptionTransformExprs ::
    Monad i =>
    (Expr Binder v0 n0 i o () -> i (Expr Binder v1 n1 i o ())) ->
    HoleOption v0 n0 i o ->
    HoleOption v1 n1 i o
holeOptionTransformExprs onExpr option =
    option
    { _hoSugaredBaseExpr = option ^. hoSugaredBaseExpr >>= onExpr
    , _hoResults = option ^. hoResults <&> _2 %~ (>>= holeResultConverted onExpr)
    }

holeTransformExprs ::
    Monad i =>
    (Expr Binder v0 n0 i o () -> i (Expr Binder v1 n1 i o ())) ->
    Hole v0 n0 i o -> Hole v1 n1 i o
holeTransformExprs onExpr =
    holeOptions . Lens.mapped . traverse %~ holeOptionTransformExprs onExpr

assignmentBodyAddFirstParam :: Lens' (Assignment v name i o a) (AddFirstParam name i o)
assignmentBodyAddFirstParam f (BodyFunction x) = fAddFirstParam f x <&> BodyFunction
assignmentBodyAddFirstParam f (BodyPlain x) = apAddFirstParam f x <&> BodyPlain

getVarName :: Lens.Traversal' (GetVar a o) a
getVarName f (GetParam x) = (pNameRef . nrName) f x <&> GetParam
getVarName f (GetBinder x) = (bvNameRef . nrName) f x <&> GetBinder
getVarName _ (GetParamsRecord x) = GetParamsRecord x & pure

binderParamsFuncParams ::
    Traversal
    (BinderParams v0 name i o)
    (BinderParams v1 name i o)
    (FuncParam v0 name)
    (FuncParam v1 name)
binderParamsFuncParams f (NullParam x) = _1 f x <&> NullParam
binderParamsFuncParams f (Params x) = (traverse . _1) f x <&> Params

paneBinder :: Traversal (Pane v0 n i o a0) (Pane v1 n i o a1) (Annotated a0 # Assignment v0 n i o) (Annotated a1 # Assignment v1 n i o)
paneBinder = paneBody . _PaneDefinition . drBody . _DefinitionBodyExpression . deContent

evalResults ::
    (EvalResults e, Functor i) =>
    Traversal (Expr e v0 name i o a) (Expr e v1 name i o a) v0 v1
evalResults f (Ann a b) =
    Ann
    <$> (Lens._Wrapped . Lens._1 . plAnnotation . _AnnotationVal) f a
    <*> bodyEvalResults f b

constEvalResults ::
    Traversal
    (Annotated (Payload v0 name i o, a) # Const x)
    (Annotated (Payload v1 name i o, a) # Const x)
    v0 v1
constEvalResults f (Ann a (Const b)) =
    (Lens._Wrapped . Lens._1 . plAnnotation . _AnnotationVal) f a
    <&> (`Ann` Const b)

class EvalResults e where
    bodyEvalResults ::
        Functor i =>
        Traversal (Body e v0 name i o a) (Body e v1 name i o a) v0 v1

instance EvalResults Assignment where
    bodyEvalResults f (BodyFunction x) = bodyEvalResults f x <&> BodyFunction
    bodyEvalResults f (BodyPlain x) = (apBody . bodyEvalResults) f x <&> BodyPlain

instance EvalResults Binder where
    bodyEvalResults f (BinderLet x) = bodyEvalResults f x <&> BinderLet
    bodyEvalResults f (BinderTerm x) = bodyEvalResults f x <&> BinderTerm

instance EvalResults Composite where
    bodyEvalResults f (Composite i p t a) =
        Composite
        <$> (traverse . ciExpr . evalResults) f i
        <*> (traverse . constEvalResults) f p
        <*> (_OpenComposite . _2 . evalResults) f t
        <*> pure a

instance EvalResults Else where
    bodyEvalResults f (SimpleElse x) = bodyEvalResults f x <&> SimpleElse
    bodyEvalResults f (ElseIf x) = bodyEvalResults f x <&> ElseIf

instance EvalResults Function where
    bodyEvalResults f x =
        (,)
        <$> (binderParamsFuncParams . fpAnnotation . _AnnotationVal) f (x ^. fParams)
        <*> evalResults f (x ^. fBody)
        <&> \(p, b) -> x { _fParams = p, _fBody = b }

instance EvalResults IfElse where
    bodyEvalResults f (IfElse i t e) =
        IfElse
        <$> evalResults f i
        <*> evalResults f t
        <*> evalResults f e

instance EvalResults Let where
    bodyEvalResults f x =
        (,)
        <$> evalResults f (x ^. lValue)
        <*> evalResults f (x ^. lBody)
        <&> \(v, b) -> x { _lValue = v, _lBody = b }

instance EvalResults Term where
    bodyEvalResults _ BodyPlaceHolder = pure BodyPlaceHolder
    bodyEvalResults _ (BodyLiteral x) = BodyLiteral x & pure
    bodyEvalResults _ (BodyGetVar x) = BodyGetVar x & pure
    bodyEvalResults _ (BodyFromNom x) = BodyFromNom x & pure
    bodyEvalResults f (BodyLam x) = (lamFunc . bodyEvalResults) f x <&> BodyLam
    bodyEvalResults f (BodySimpleApply x) = (appChildren . evalResults) f x <&> BodySimpleApply
    bodyEvalResults f (BodyIfElse x) = bodyEvalResults f x <&> BodyIfElse
    bodyEvalResults f (BodyGetField x) = (gfRecord . evalResults) f x <&> BodyGetField
    bodyEvalResults f (BodyRecord x) = bodyEvalResults f x <&> BodyRecord
    bodyEvalResults f (BodyToNom x) = (nVal . evalResults) f x <&> BodyToNom
    bodyEvalResults f (BodyInject x) =
        iContent
        ( \case
            InjectNullary y -> constEvalResults f y <&> InjectNullary
            InjectVal y -> evalResults f y <&> InjectVal
        ) x <&> BodyInject
    bodyEvalResults f (BodyCase (Case k b)) =
        Case
        <$> (_CaseWithArg . caVal . evalResults) f k
        <*> bodyEvalResults f b
        <&> BodyCase
    bodyEvalResults f (BodyLabeledApply (LabeledApply u s a p)) =
        LabeledApply
        <$> constEvalResults f u
        <*> (traverse . evalResults) f s
        <*> (traverse . aaExpr . evalResults) f a
        <*> (traverse . constEvalResults) f p
        <&> BodyLabeledApply
    bodyEvalResults _ (BodyHole x) =
        -- TODO: This is a "cheat".
        -- Should we solve it by hole results not having eval results?
        x & holeOptions . Lens.mapped .~ []
        & BodyHole & pure
    bodyEvalResults f (BodyFragment x) =
        evalResults f (x ^. fExpr) <&>
        \e ->
        BodyFragment x
        { _fExpr = e
        , _fOptions = [] <$ x ^. fOptions
        }
