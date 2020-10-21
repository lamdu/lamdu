{-# LANGUAGE TypeApplications, FlexibleInstances, DefaultSignatures, GADTs, MultiParamTypeClasses #-}

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
    , workAreaAnnotations, binderParamsAnnotations, holeOptionAnnotations
    ) where

import           Control.Lens (Traversal, LensLike)
import qualified Control.Lens as Lens
import           Hyper
import           Hyper.Class.Morph
import           Hyper.Recurse (Recursive(..), proxyArgument)
import           Hyper.Type.AST.App (MorphWitness(..))
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
    holeOptions . Lens.mapped . Lens.mapped %~ holeOptionTransformExprs onExpr

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

holeOptionAnnotations ::
    Functor i =>
    Lens.Setter (HoleOption v0 n i o) (HoleOption v1 n i o) v0 v1
holeOptionAnnotations =
    Lens.setting $
    \f (HoleOption i b r) ->
    HoleOption i (b <&> onNode f)
    (r <&> _2 . Lens.mapped . holeResultConverted %~ onNode f)
    where
        onNode f = annotations (<&> Lens.mapped . holeOptionAnnotations %~ f) %~ f

workAreaAnnotations ::
    Applicative f =>
    (i [HoleOption v0 n i o] -> i [HoleOption v1 n i o]) ->
    LensLike f (WorkArea v0 n i o (Payload v0 n i o, a)) (WorkArea v1 n i o (Payload v1 n i o, a)) v0 v1
workAreaAnnotations h f w =
    WorkArea
    <$> (traverse . paneBinder . annotations h) f (w ^. waPanes)
    <*> (replExpr . annotations h) f (w ^. waRepl)
    ?? w ^. waGlobals

binderParamsAnnotations :: Traversal (BinderParams v0 n i o) (BinderParams v1 n i o) v0 v1
binderParamsAnnotations = binderParamsFuncParams . fpAnnotation

class Annotations n i o v0 v1 t0 t1 where
    annotations ::
        Applicative f =>
        (i [HoleOption v0 n i o] -> i [HoleOption v1 n i o]) ->
        LensLike f (Annotated (Payload v0 n i o, a) # t0) (Annotated (Payload v1 n i o, a) # t1) v0 v1

instance Annotations n i o v0 v1 (Const x) (Const x) where
    annotations _ f (Ann a (Const b)) =
        (Lens._Wrapped . Lens._1 . plAnnotation) f a
        <&> (`Ann` Const b)

instance BodyAnnotations e => Annotations n i o v0 v1 (e v0 n i o) (e v1 n i o) where
    annotations h f (Ann a b) =
        Ann
        <$> (Lens._Wrapped . Lens._1 . plAnnotation) f a
        <*> bodyAnnotations h f b

class BodyAnnotations e where
    bodyAnnotations ::
        Applicative f =>
        (i [HoleOption v0 n i o] -> i [HoleOption v1 n i o]) ->
        LensLike f (Body e v0 n i o a) (Body e v1 n i o a) v0 v1
    default bodyAnnotations ::
        ( HMorphWithConstraint (e v0 n i o) (e v1 n i o) (Annotations n i o v0 v1)
        , HTraversable (e v1 n i o), Applicative f
        ) =>
        (i [HoleOption v0 n i o] -> i [HoleOption v1 n i o]) ->
        LensLike f (Body e v0 n i o a) (Body e v1 n i o a) v0 v1
    bodyAnnotations h =
        withP (\p f -> morphTraverse (p #?> annotations h f))
        where
            withP ::
                (Proxy (Annotations n i o v0 v1) -> ((v0 -> f v1) -> Body e v0 n i o a -> r)) ->
                (v0 -> f v1) -> Body e v0 n i o a -> r
            withP x = x Proxy

instance BodyAnnotations Assignment where
    bodyAnnotations h f (BodyFunction x) = bodyAnnotations h f x <&> BodyFunction
    bodyAnnotations h f (BodyPlain x) = (apBody . bodyAnnotations h) f x <&> BodyPlain

instance BodyAnnotations Binder where
    bodyAnnotations h f (BinderLet x) = bodyAnnotations h f x <&> BinderLet
    bodyAnnotations h f (BinderTerm x) = bodyAnnotations h f x <&> BinderTerm

instance BodyAnnotations Case
instance BodyAnnotations Composite

instance BodyAnnotations Else where
    bodyAnnotations h f (SimpleElse x) = bodyAnnotations h f x <&> SimpleElse
    bodyAnnotations h f (ElseIf x) = bodyAnnotations h f x <&> ElseIf

instance BodyAnnotations Function where
    bodyAnnotations h f x =
        (,)
        <$> binderParamsAnnotations f (x ^. fParams)
        <*> annotations h f (x ^. fBody)
        <&> \(p, b) -> x { _fParams = p, _fBody = b }

instance BodyAnnotations Fragment where
    bodyAnnotations h f (Fragment e0 heal t o) =
        annotations h f e0 <&> \e1 -> Fragment e1 heal t (h o)

instance BodyAnnotations IfElse
instance BodyAnnotations InjectContent
instance BodyAnnotations LabeledApply
instance BodyAnnotations Let

instance BodyAnnotations Term where
    bodyAnnotations _ _ BodyPlaceHolder = pure BodyPlaceHolder
    bodyAnnotations _ _ (BodyLiteral x) = BodyLiteral x & pure
    bodyAnnotations _ _ (BodyGetVar x) = BodyGetVar x & pure
    bodyAnnotations _ _ (BodyFromNom x) = BodyFromNom x & pure
    bodyAnnotations h f (BodyLam x) = (lamFunc . bodyAnnotations h) f x <&> BodyLam
    bodyAnnotations h f (BodyIfElse x) = bodyAnnotations h f x <&> BodyIfElse
    bodyAnnotations h f (BodyGetField x) = (gfRecord . annotations h) f x <&> BodyGetField
    bodyAnnotations h f (BodyRecord x) = bodyAnnotations h f x <&> BodyRecord
    bodyAnnotations h f (BodyToNom x) = (nVal . annotations h) f x <&> BodyToNom
    bodyAnnotations h f (BodySimpleApply x) =
        morphTraverse (\M_App_expr -> annotations h f) x <&> BodySimpleApply
    bodyAnnotations h f (BodyInject x) = (iContent . bodyAnnotations h) f x <&> BodyInject
    bodyAnnotations h f (BodyCase x) = bodyAnnotations h f x <&> BodyCase
    bodyAnnotations h f (BodyLabeledApply x) = bodyAnnotations h f x <&> BodyLabeledApply
    bodyAnnotations h _ (BodyHole x) = x & holeOptions %~ h & BodyHole & pure
    bodyAnnotations h f (BodyFragment x) = bodyAnnotations h f x <&> BodyFragment
