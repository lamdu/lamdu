{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, FlexibleInstances, KindSignatures, DefaultSignatures, MultiParamTypeClasses #-}
module Lamdu.Sugar.Lens
    ( SugarExpr(..)
    , HasBinderParams(..)
    , childPayloads
    , bodyUnfinished
    , defSchemes
    , assignmentBodyAddFirstParam
    , binderFuncParamActions
    , binderResultExpr
    , holeTransformExprs, holeOptionTransformExprs
    , annotationTypes
    , onSubExprParams
    , paramsAnnotations
    , stripAnnotations
    ) where

import           AST (Node, Children(..), ChildrenWithConstraint, overChildren)
import           AST.Class.Recursive (Recursive(..), ChildrenRecursive, hoistBody)
import           AST.Functor.Ann (Ann(..), ann, val)
import qualified Control.Lens as Lens
import           Data.Constraint
import           Data.Functor.Const (Const(..))
import           Data.Proxy (Proxy(..))
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

childPayloads :: ChildrenWithConstraint expr Children =>
    Lens.Traversal' (expr (Ann a)) a
childPayloads f =
    children (Proxy :: Proxy Children) (ann f)

class SugarExpr (t :: (* -> *) -> *) where
    isUnfinished :: t f -> Bool
    isUnfinished _ = False

    isForbiddenInLightLam :: t f -> Bool
    isForbiddenInLightLam = isUnfinished

    sugarExprRecursive :: Dict (ChildrenWithConstraint t SugarExpr)
    default sugarExprRecursive ::
        ChildrenWithConstraint t SugarExpr =>
        Dict (ChildrenWithConstraint t SugarExpr)
    sugarExprRecursive = Dict

instance Recursive SugarExpr where
    recursive _ _ = Sub sugarExprRecursive

instance SugarExpr (Const (GetVar name o))
instance SugarExpr (Const (NullaryVal name i o))
instance SugarExpr (Else name i o)

instance SugarExpr (Const (BinderVarRef name o)) where
    isUnfinished (Const x) = Lens.has binderVarRefUnfinished x

instance SugarExpr (AssignmentBody name i o) where
    isUnfinished (BodyPlain x) = isUnfinished (x ^. apBody)
    isUnfinished BodyFunction{} = False

instance SugarExpr (Function name i o) where
    isForbiddenInLightLam = Lens.has (fParams . _Params)

instance SugarExpr (Binder name i o) where
    isUnfinished (BinderExpr x) = isUnfinished x
    isUnfinished BinderLet{} = False
    isForbiddenInLightLam BinderLet{} = True
    isForbiddenInLightLam (BinderExpr x) = isForbiddenInLightLam x

instance SugarExpr (Body name i o) where
    isUnfinished BodyHole{} = True
    isUnfinished BodyFragment{} = True
    isUnfinished (BodyGetVar (GetBinder x)) = isUnfinished (Const x)
    isUnfinished _ = False

binderVarRefUnfinished :: Lens.Traversal' (BinderVarRef name m) ()
binderVarRefUnfinished =
    bvForm . _GetDefinition . Lens.failing _DefDeleted (_DefTypeChanged . Lens.united)

stripAnnotations :: ChildrenRecursive expr => expr (Ann a) -> expr (Ann ())
stripAnnotations = hoistBody (ann .~ ())

bodyUnfinished :: Lens.Traversal' (Body name i o (Ann a)) ()
bodyUnfinished =
    _BodyHole . Lens.united
    & Lens.failing (_BodyFragment . Lens.united)
    & Lens.failing (_BodyGetVar . _GetBinder . binderVarRefUnfinished)
    & Lens.failing (_BodyLabeledApply . aFunc . val . Lens._Wrapped . binderVarRefUnfinished)

defBodySchemes :: Lens.Traversal' (DefinitionBody name i o expr) (Scheme name)
defBodySchemes f (DefinitionBodyBuiltin b) =
    b & biType %%~ f
    <&> DefinitionBodyBuiltin
defBodySchemes f (DefinitionBodyExpression de) =
    de & deType %%~ f
    <&> DefinitionBodyExpression

defSchemes :: Lens.Traversal' (Definition name i o expr) (Scheme name)
defSchemes = drBody . defBodySchemes

binderFuncParamActions ::
    Lens.Traversal' (BinderParams name i o) (FuncParamActions name i o)
binderFuncParamActions _ (NullParam a) = pure (NullParam a)
binderFuncParamActions f (Params ps) = (traverse . fpInfo . piActions) f ps <&> Params

binderResultExpr :: Lens.IndexedLens' (Body name i o (Ann ())) (Node (Ann a) (Binder name i o)) a
binderResultExpr f (Ann pl x) =
    case x of
    BinderExpr e ->
        Lens.indexed f (stripAnnotations e) pl
        <&> (`Ann` x)
    BinderLet l ->
        lBody (binderResultExpr f) l
        <&> BinderLet
        <&> Ann pl

holeOptionTransformExprs ::
    Monad i =>
    (Node (Ann (Payload n0 i o ())) (Binder n0 i o) ->
        i (Node (Ann (Payload n1 i o ())) (Binder n1 i o))) ->
    HoleOption n0 i o -> HoleOption n1 i o
holeOptionTransformExprs onExpr option =
    option
    { _hoSugaredBaseExpr = option ^. hoSugaredBaseExpr >>= onExpr
    , _hoResults = option ^. hoResults <&> Lens._2 %~ (>>= holeResultConverted onExpr)
    }

holeTransformExprs ::
    Monad i =>
    (Node (Ann (Payload n0 i o ())) (Binder n0 i o) ->
        i (Node (Ann (Payload n1 i o ())) (Binder n1 i o))) ->
    Hole n0 i o -> Hole n1 i o
holeTransformExprs onExpr hole =
    hole
    { _holeOptions = hole ^. holeOptions <&> traverse %~ holeOptionTransformExprs onExpr
    , _holeOptionLiteral =
        hole ^. holeOptionLiteral <&> Lens.mapped . Lens._2 %~ (>>= holeResultConverted onExpr)
    }

assignmentBodyAddFirstParam :: Lens' (AssignmentBody name i o a) (AddFirstParam name i o)
assignmentBodyAddFirstParam f (BodyFunction x) = fAddFirstParam f x <&> BodyFunction
assignmentBodyAddFirstParam f (BodyPlain x) = apAddFirstParam f x <&> BodyPlain

annotationTypes :: Lens.Traversal' (Annotation name i) (Type name)
annotationTypes _ AnnotationNone = pure AnnotationNone
annotationTypes f (AnnotationType x) = f x <&> AnnotationType
annotationTypes f (AnnotationVal x) = (annotationType . Lens._Just) f x <&> AnnotationVal

-- TODO: rename paramsAnnotations
paramsAnnotations :: Lens.Traversal' (BinderParams name i o) (Annotation name i)
paramsAnnotations f (NullParam x) = fpAnnotation f x <&> NullParam
paramsAnnotations f (Params xs) = (traverse . fpAnnotation) f xs <&> Params

class HasBinderParams p (expr :: (* -> *) -> *) where
    binderParams :: Lens.Setter' (expr f) p

    binderParamsRecursive :: Proxy p -> Dict (ChildrenWithConstraint expr (HasBinderParams p))
    default binderParamsRecursive ::
        ChildrenWithConstraint expr (HasBinderParams p) =>
        Proxy p -> Dict (ChildrenWithConstraint expr (HasBinderParams p))
    binderParamsRecursive _ = Dict

instance Recursive (HasBinderParams p) where
    recursive _ _ = Sub (binderParamsRecursive (Proxy :: Proxy p))

instance HasBinderParams (BinderParams name i o) (AssignmentBody name i o) where
    binderParams f (BodyPlain x) = (apBody . binderParams) f x <&> BodyPlain
    binderParams f (BodyFunction x) = binderParams f x <&> BodyFunction

instance HasBinderParams (BinderParams name i o) (Binder name i o) where
    binderParams f (BinderExpr x) = binderParams f x <&> BinderExpr
    binderParams _ x = pure x

instance HasBinderParams (BinderParams name i o) (Body name i o) where
    binderParams f (BodyLam x) = (lamFunc . binderParams) f x <&> BodyLam
    binderParams _ x = pure x

instance HasBinderParams (BinderParams name i o) (Const a) where
    binderParams _ x = pure x

instance HasBinderParams (BinderParams name i o) (Else name i o) where
    binderParams f (SimpleElse x) = binderParams f x <&> SimpleElse
    binderParams _ x = pure x

instance HasBinderParams (BinderParams name i o) (Function name i o) where
    binderParams = fParams

onSubExprParams ::
    forall p expr f.
    (HasBinderParams p expr, Functor f) =>
    Proxy p -> (p -> p) -> expr f -> expr f
onSubExprParams p f x =
    x
    & binderParams %~ f
    & overChildren pc (fmap (onSubExprParams p f))
    \\ recursive pc (Proxy :: Proxy expr)
    where
        pc = Proxy :: Proxy (HasBinderParams p)
