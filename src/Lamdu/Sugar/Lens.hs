{-# LANGUAGE TypeApplications, FlexibleInstances, DefaultSignatures, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables, TypeFamilies, TupleSections #-}

module Lamdu.Sugar.Lens
    ( SugarExpr(..), Annotations(..), HAnnotations(..)
    , childPayloads
    , bodyUnfinished
    , defSchemes
    , assignmentBodyAddFirstParam
    , binderFuncParamActions
    , binderResultExpr
    , holeTransformExprs, holeOptionTransformExprs
    , getVarName
    , paneBinder
    ) where

import           Control.Lens (Traversal)
import qualified Control.Lens as Lens
import           Hyper
import           Hyper.Class.Morph
import           Hyper.Recurse (Recursive(..), proxyArgument)
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
instance SugarExpr (Const (TId name))
instance SugarExpr (Const (TagChoice name i o EntityId))

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

instance SugarExpr (PostfixFunc v name i o)

instance SugarExpr (Binder v name i o) where
    isUnfinished (BinderTerm x) = isUnfinished x
    isUnfinished BinderLet{} = False
    isForbiddenInLightLam BinderLet{} = True
    isForbiddenInLightLam (BinderTerm x) = isForbiddenInLightLam x

instance SugarExpr (Term v name i o) where
    isUnfinished (BodyLeaf LeafHole{}) = True
    isUnfinished BodyFragment{} = True
    isUnfinished (BodyLeaf (LeafGetVar (GetBinder x))) = isUnfinished (Const x)
    isUnfinished _ = False
    isForbiddenInLightLam (BodyLam f) = isForbiddenInLightLam (f ^. lamFunc)
    isForbiddenInLightLam x = isUnfinished x

binderVarRefUnfinished :: Lens.Traversal' (BinderVarRef name m) ()
binderVarRefUnfinished =
    bvForm . _GetDefinition . Lens.failing _DefDeleted (_DefTypeChanged . Lens.united)

bodyUnfinished :: Lens.Traversal' (Term v name i o # Ann a) ()
bodyUnfinished =
    _BodyLeaf . _LeafHole . Lens.united
    & Lens.failing (_BodyFragment . Lens.united)
    & Lens.failing (_BodyLeaf . _LeafGetVar . _GetBinder . binderVarRefUnfinished)
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
    (Expr Binder (Annotation () n) n i o () -> i (Expr Binder (Annotation () n) n i o ())) ->
    HoleOption n i o ->
    HoleOption n i o
holeOptionTransformExprs onExpr =
    hoResults . Lens.mapped . _2 %~ (>>= holeResultConverted onExpr)

holeTransformExprs ::
    Monad i =>
    (Expr Binder (Annotation () n) n i o () -> i (Expr Binder (Annotation () n) n i o ())) ->
    Hole n i o -> Hole n i o
holeTransformExprs onExpr =
    holeOptions . Lens.mapped . Lens.mapped . Lens.mapped %~ holeOptionTransformExprs onExpr

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

class Annotations a b s t where
    annotations :: Traversal s t a b
    default annotations ::
        ( s ~ (e0 # h0), t ~ (e1 # h1)
        , HAnnotations a b e0 e1, HAnnotations a b h0 h1
        ) => Traversal s t a b
    annotations = hAnnotations

class HAnnotations a b s t where
    hAnnotations ::
        HAnnotations a b h0 h1 =>
        Traversal (s # h0) (t # h1) a b
    default hAnnotations ::
        ( HMorphWithConstraint s t (HAnnotations a b)
        , HTraversable t
        , HAnnotations a b h0 h1
        ) => Traversal (s # h0) (t # h1) a b
    hAnnotations f = morphTraverse (Proxy @(HAnnotations a b) #?> hAnnotations f)

instance Annotations a b (BinderVarRef n o) (BinderVarRef n o) where annotations _ x = pure x
instance Annotations a b (GetVar n o) (GetVar n o) where annotations _ x = pure x
instance Annotations a b (TagChoice n i o e) (TagChoice n i o e) where annotations _ x = pure x

instance Annotations a b s0 t0 => Annotations a b (s0, x) (t0, x) where
    annotations = _1 . annotations

instance Annotations a b (BinderParams a n i o) (BinderParams b n i o) where
    annotations = binderParamsFuncParams . fpAnnotation

instance Annotations a b (Payload a n i o) (Payload b n i o) where
    annotations = plAnnotation

instance Annotations a b s t => Annotations a b (WorkArea a n i o s) (WorkArea b n i o t) where
    annotations f w =
        WorkArea
        <$> (traverse . paneBinder . hAnnotations) f (w ^. waPanes)
        <*> (replExpr . hAnnotations) f (w ^. waRepl)
        ?? w ^. waGlobals

instance HAnnotations a b p0 p1 => HAnnotations a b (Ann p0) (Ann p1) where
    hAnnotations f (Ann p x) = Ann <$> hAnnotations f p <*> hAnnotations f x

instance Annotations a b s t => HAnnotations a b (Const s) (Const t) where
    hAnnotations = Lens._Wrapped . annotations

instance HAnnotations a b (Composite a n i o) (Composite b n i o)
instance HAnnotations a b (IfElse a n i o) (IfElse b n i o)
instance HAnnotations a b (LabeledApply a n i o) (LabeledApply b n i o)
instance HAnnotations a b (Let a n i o) (Let b n i o)
instance HAnnotations a b (PostfixApply a n i o) (PostfixApply b n i o)
instance HAnnotations a b (PostfixFunc a n i o) (PostfixFunc b n i o)

instance HAnnotations a b (Assignment a n i o) (Assignment b n i o) where
    hAnnotations f (BodyFunction x) = hAnnotations f x <&> BodyFunction
    hAnnotations f (BodyPlain x) = (apBody . hAnnotations) f x <&> BodyPlain

instance HAnnotations a b (Binder a n i o) (Binder b n i o) where
    hAnnotations f (BinderLet x) = hAnnotations f x <&> BinderLet
    hAnnotations f (BinderTerm x) = hAnnotations f x <&> BinderTerm

instance HAnnotations a b (Else a n i o) (Else b n i o) where
    hAnnotations f (SimpleElse x) = hAnnotations f x <&> SimpleElse
    hAnnotations f (ElseIf x) = hAnnotations f x <&> ElseIf

instance HAnnotations a b (Fragment a n i o) (Fragment b n i o) where
    hAnnotations = fExpr . hAnnotations

instance HAnnotations a b (Function a n i o) (Function b n i o) where
    hAnnotations f x =
        (,)
        <$> annotations f (x ^. fParams)
        <*> hAnnotations f (x ^. fBody)
        <&> \(p, b) -> x { _fParams = p, _fBody = b }

instance HAnnotations a b (Term a n i o) (Term b n i o) where
    hAnnotations _ (BodyLeaf x) = BodyLeaf x & pure
    hAnnotations f (BodyLam x) = (lamFunc . hAnnotations) f x <&> BodyLam
    hAnnotations f (BodyIfElse x) = hAnnotations f x <&> BodyIfElse
    hAnnotations f (BodyRecord x) = hAnnotations f x <&> BodyRecord
    hAnnotations f (BodyToNom x) = (nVal . hAnnotations) f x <&> BodyToNom
    hAnnotations f (BodySimpleApply x) = (morphTraverse1 . hAnnotations) f x <&> BodySimpleApply
    hAnnotations f (BodyLabeledApply x) = hAnnotations f x <&> BodyLabeledApply
    hAnnotations f (BodyFragment x) = hAnnotations f x <&> BodyFragment
    hAnnotations f (BodyPostfixApply x) = hAnnotations f x <&> BodyPostfixApply
    hAnnotations f (BodyPostfixFunc x) = hAnnotations f x <&> BodyPostfixFunc
    hAnnotations f (BodyNullaryInject x) = (iContent . hAnnotations) f x <&> BodyNullaryInject
