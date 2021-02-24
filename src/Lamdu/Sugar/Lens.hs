{-# LANGUAGE TypeApplications, FlexibleInstances, DefaultSignatures, MultiParamTypeClasses #-}

module Lamdu.Sugar.Lens
    ( SugarExpr(..), Annotations(..)
    , childPayloads
    , bodyUnfinished
    , defSchemes
    , assignmentBodyAddFirstParam
    , binderFuncParamActions
    , binderResultExpr
    , holeTransformExprs, holeOptionTransformExprs
    , getVarName
    , paneBinder
    , workAreaAnnotations, binderParamsAnnotations
    ) where

import           Control.Lens (Traversal, LensLike)
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
instance SugarExpr (Const (NullaryVal name i o))
instance SugarExpr (Const (TId name))

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

workAreaAnnotations ::
    Applicative f =>
    LensLike f (WorkArea v0 n i o (Payload v0 n i o, a)) (WorkArea v1 n i o (Payload v1 n i o, a)) v0 v1
workAreaAnnotations f w =
    WorkArea
    <$> (traverse . paneBinder . annotations) f (w ^. waPanes)
    <*> (replExpr . annotations) f (w ^. waRepl)
    ?? w ^. waGlobals

binderParamsAnnotations :: Traversal (BinderParams v0 n i o) (BinderParams v1 n i o) v0 v1
binderParamsAnnotations = binderParamsFuncParams . fpAnnotation

class Annotations n i o v0 v1 t0 t1 where
    annotations ::
        Applicative f =>
        LensLike f (Annotated (Payload v0 n i o, a) # t0) (Annotated (Payload v1 n i o, a) # t1) v0 v1

instance Annotations n i o v0 v1 (Const x) (Const x) where
    annotations f (Ann a (Const b)) =
        (Lens._Wrapped . Lens._1 . plAnnotation) f a
        <&> (`Ann` Const b)

instance BodyAnnotations e => Annotations n i o v0 v1 (e v0 n i o) (e v1 n i o) where
    annotations f (Ann a b) =
        Ann
        <$> (Lens._Wrapped . Lens._1 . plAnnotation) f a
        <*> bodyAnnotations f b

class BodyAnnotations e where
    bodyAnnotations ::
        Applicative f =>
        LensLike f (Body e v0 n i o a) (Body e v1 n i o a) v0 v1
    default bodyAnnotations ::
        ( HMorphWithConstraint (e v0 n i o) (e v1 n i o) (Annotations n i o v0 v1)
        , HTraversable (e v1 n i o), Applicative f
        ) =>
        LensLike f (Body e v0 n i o a) (Body e v1 n i o a) v0 v1
    bodyAnnotations =
        withP (\p f -> morphTraverse (p #?> annotations f))
        where
            withP ::
                (Proxy (Annotations n i o v0 v1) -> ((v0 -> f v1) -> Body e v0 n i o a -> r)) ->
                (v0 -> f v1) -> Body e v0 n i o a -> r
            withP x = x Proxy

instance BodyAnnotations Assignment where
    bodyAnnotations f (BodyFunction x) = bodyAnnotations f x <&> BodyFunction
    bodyAnnotations f (BodyPlain x) = (apBody . bodyAnnotations) f x <&> BodyPlain

instance BodyAnnotations Binder where
    bodyAnnotations f (BinderLet x) = bodyAnnotations f x <&> BinderLet
    bodyAnnotations f (BinderTerm x) = bodyAnnotations f x <&> BinderTerm

instance BodyAnnotations Composite

instance BodyAnnotations Else where
    bodyAnnotations f (SimpleElse x) = bodyAnnotations f x <&> SimpleElse
    bodyAnnotations f (ElseIf x) = bodyAnnotations f x <&> ElseIf

instance BodyAnnotations Function where
    bodyAnnotations f x =
        (,)
        <$> binderParamsAnnotations f (x ^. fParams)
        <*> annotations f (x ^. fBody)
        <&> \(p, b) -> x { _fParams = p, _fBody = b }

instance BodyAnnotations Fragment where
    bodyAnnotations = fExpr . annotations

instance BodyAnnotations IfElse
instance BodyAnnotations LabeledApply
instance BodyAnnotations Let
instance BodyAnnotations PostfixApply
instance BodyAnnotations PostfixFunc

instance BodyAnnotations Term where
    bodyAnnotations _ (BodyLeaf x) = BodyLeaf x & pure
    bodyAnnotations f (BodyLam x) = (lamFunc . bodyAnnotations) f x <&> BodyLam
    bodyAnnotations f (BodyIfElse x) = bodyAnnotations f x <&> BodyIfElse
    bodyAnnotations f (BodyRecord x) = bodyAnnotations f x <&> BodyRecord
    bodyAnnotations f (BodyToNom x) = (nVal . annotations) f x <&> BodyToNom
    bodyAnnotations f (BodySimpleApply x) = (morphTraverse1 . annotations) f x <&> BodySimpleApply
    bodyAnnotations f (BodyLabeledApply x) = bodyAnnotations f x <&> BodyLabeledApply
    bodyAnnotations f (BodyFragment x) = bodyAnnotations f x <&> BodyFragment
    bodyAnnotations f (BodyPostfixApply x) = bodyAnnotations f x <&> BodyPostfixApply
    bodyAnnotations f (BodyPostfixFunc x) = bodyAnnotations f x <&> BodyPostfixFunc
