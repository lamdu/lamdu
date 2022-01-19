{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}

module Lamdu.Sugar.Lens
    ( childPayloads
    , bodyUnfinished
    , defSchemes
    , binderResultExpr
    , getVarName
    , unfinishedPayloads
    , taggedListItems, taggedListBodyItems
    , taggedItemTagChoices
    , tagChoiceOptions
    ) where

import           Control.Lens (Traversal)
import qualified Control.Lens as Lens
import           Control.Monad.Unit (Unit)
import           Hyper
import           Lamdu.Sugar.Props (SugarExpr(..), varRefUnfinished)
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

childPayloads ::
    HTraversable expr =>
    Lens.Traversal' (expr # Annotated a) a
childPayloads f =
    htraverse (const (annotation f))

unfinishedPayloads ::
    forall t a.
    SugarExpr t =>
    Lens.Traversal' (Annotated a # t) a
unfinishedPayloads f (Ann (Const a) x) =
    withDict (sugarExprRecursive (Proxy @t)) $
    flip Ann
    <$> htraverse (Proxy @SugarExpr #> unfinishedPayloads f) x
    <*> ((if isUnfinished x then f a else pure a) <&> Const)

bodyUnfinished :: Lens.Traversal' (Term v name i o # Ann a) ()
bodyUnfinished =
    _BodyLeaf . _LeafHole . Lens.united
    & Lens.failing (_BodyFragment . Lens.united)
    & Lens.failing (_BodyLeaf . _LeafGetVar . _GetVar . varRefUnfinished)
    & Lens.failing (_BodyLabeledApply . aFunc . hVal . Lens._Wrapped . varRefUnfinished)

defBodySchemes :: Lens.Traversal' (DefinitionBody v name i o expr) (Scheme name Unit)
defBodySchemes f (DefinitionBodyBuiltin b) =
    b & biType %%~ f
    <&> DefinitionBodyBuiltin
defBodySchemes f (DefinitionBodyExpression de) =
    de & deType %%~ f
    <&> DefinitionBodyExpression

defSchemes :: Lens.Traversal' (Definition v name i o expr) (Scheme name Unit)
defSchemes = drBody . defBodySchemes

binderResultExpr ::
    Lens.IndexedLens' (Term v name i o # Annotated ()) (Annotated a # Binder v name i o) a
binderResultExpr f (Ann (Const pl) x) =
    case x ^. bBody of
    BinderTerm e ->
        Lens.indexed f
        (hmap (Proxy @(Recursively HFunctor) #> hflipped %~ hmap (\_ Const{} -> Const ())) e)
        pl
        <&> Const
        <&> (`Ann` x)
    BinderLet l ->
        lBody (binderResultExpr f) l
        <&> BinderLet
        <&> Binder (x ^. bAddOuterLet)
        <&> Ann (Const pl)

getVarName :: Lens.Traversal' (GetVar a o) a
getVarName f (GetParam x) = (pNameRef . nrName) f x <&> GetParam
getVarName f (GetVar x) = (vNameRef . nrName) f x <&> GetVar
getVarName _ (GetParamsRecord x) = GetParamsRecord x & pure

taggedListBodyItems ::
    Traversal (TaggedListBody n0 i0 o a0) (TaggedListBody n1 i1 o a1)
    (TaggedItem n0 i0 o a0) (TaggedItem n1 i1 o a1)
taggedListBodyItems f (TaggedListBody hd tl) = TaggedListBody <$> f hd <*> (traverse . tsiItem) f tl

taggedListItems ::
    Traversal (TaggedList n i o a0) (TaggedList n i o a1)
    (TaggedItem n i o a0) (TaggedItem n i o a1)
taggedListItems = tlItems . Lens._Just . taggedListBodyItems

taggedItemTagChoices :: Functor i => Lens.Setter' (TaggedItem n i o a) (TagChoice n o)
taggedItemTagChoices =
    Lens.sets (\f (TaggedItem t d a v) -> TaggedItem (t & tagRefReplace . Lens.mapped %~ f) d (a <&> f) v)

tagChoiceOptions :: Lens.Setter (TagChoice n0 o0) (TagChoice n1 o1) (TagOption n0 o0) (TagOption n1 o1)
tagChoiceOptions = Lens.sets (\f (TagChoice o n) -> TagChoice (o <&> f) (f n))
