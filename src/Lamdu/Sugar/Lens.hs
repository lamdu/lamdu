{-# LANGUAGE FlexibleContexts, TypeApplications, ScopedTypeVariables, FlexibleInstances, KindSignatures, MultiParamTypeClasses #-}
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
import           AST.Class.Recursive (Recursive(..), RecursiveConstraint, hoistBody)
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
    children (Proxy @Children) (ann f)

class SugarExpr (t :: (* -> *) -> *) where
    isUnfinished :: t f -> Bool
    isUnfinished _ = False

    isForbiddenInLightLam :: t f -> Bool
    isForbiddenInLightLam = isUnfinished

instance SugarExpr (Const (GetVar name o))
instance SugarExpr (Const (NullaryVal name i o))

instance SugarExpr (Const (BinderVarRef name o)) where
    isUnfinished (Const x) = Lens.has binderVarRefUnfinished x

instance SugarExpr (Assignment name i o) where
    isUnfinished (BodyPlain x) = isUnfinished (x ^. apBody)
    isUnfinished BodyFunction{} = False
instance Recursive SugarExpr (Assignment name i o)

instance SugarExpr (Else name i o) where
    isUnfinished (SimpleElse x) = isUnfinished x
    isUnfinished ElseIf{} = False
instance Recursive SugarExpr (Else name i o)

instance SugarExpr (Function name i o) where
    isForbiddenInLightLam = Lens.has (fParams . _Params)
instance Recursive SugarExpr (Function name i o)

instance SugarExpr (Binder name i o) where
    isUnfinished (BinderExpr x) = isUnfinished x
    isUnfinished BinderLet{} = False
    isForbiddenInLightLam BinderLet{} = True
    isForbiddenInLightLam (BinderExpr x) = isForbiddenInLightLam x
instance Recursive SugarExpr (Binder name i o)

instance SugarExpr (Body name i o) where
    isUnfinished BodyHole{} = True
    isUnfinished BodyFragment{} = True
    isUnfinished (BodyGetVar (GetBinder x)) = isUnfinished (Const x)
    isUnfinished _ = False
instance Recursive SugarExpr (Body name i o)

binderVarRefUnfinished :: Lens.Traversal' (BinderVarRef name m) ()
binderVarRefUnfinished =
    bvForm . _GetDefinition . Lens.failing _DefDeleted (_DefTypeChanged . Lens.united)

stripAnnotations :: Recursive Children expr => expr (Ann a) -> expr (Ann ())
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

assignmentBodyAddFirstParam :: Lens' (Assignment name i o a) (AddFirstParam name i o)
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

instance HasBinderParams (BinderParams name i o) (Assignment name i o) where
    binderParams f (BodyPlain x) = (apBody . binderParams) f x <&> BodyPlain
    binderParams f (BodyFunction x) = binderParams f x <&> BodyFunction
instance Recursive (HasBinderParams (BinderParams name i o)) (Assignment name i o)

instance HasBinderParams (BinderParams name i o) (Binder name i o) where
    binderParams f (BinderExpr x) = binderParams f x <&> BinderExpr
    binderParams _ x = pure x
instance Recursive (HasBinderParams (BinderParams name i o)) (Binder name i o)

instance HasBinderParams (BinderParams name i o) (Body name i o) where
    binderParams f (BodyLam x) = (lamFunc . binderParams) f x <&> BodyLam
    binderParams _ x = pure x
instance Recursive (HasBinderParams (BinderParams name i o)) (Body name i o)

instance HasBinderParams (BinderParams name i o) (Const a) where
    binderParams _ = pure

instance HasBinderParams (BinderParams name i o) (Else name i o) where
    binderParams f (SimpleElse x) = binderParams f x <&> SimpleElse
    binderParams _ x = pure x
instance Recursive (HasBinderParams (BinderParams name i o)) (Else name i o)

instance HasBinderParams (BinderParams name i o) (Function name i o) where
    binderParams = fParams
instance Recursive (HasBinderParams (BinderParams name i o)) (Function name i o)

onSubExprParams ::
    forall p expr f.
    (Recursive (HasBinderParams p) expr, Functor f) =>
    Proxy p -> (p -> p) -> expr f -> expr f
onSubExprParams p f x =
    withDict (recursive :: Dict (RecursiveConstraint expr (HasBinderParams p))) $
    x
    & binderParams %~ f
    & overChildren
        (Proxy @(Recursive (HasBinderParams p)))
        (fmap (onSubExprParams p f))
