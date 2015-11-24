{-# LANGUAGE NoImplicitPrelude, FlexibleContexts, RecordWildCards, RankNTypes #-}
module Lamdu.Sugar.Lens
    ( bitraverseExpression, subExprPayloads, payloadsIndexedByPath
    , payloadsOf
    , holePayloads, holeArgs, subExprsOf
    , defSchemes
    , binderNamedParams
    , binderNamedParamsActions
    , binderFuncParamAdds
    , binderFuncParamDeletes
    , binderContentExpr
    , binderContentEntityId
    ) where

import           Control.Lens (Lens')
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad (void)
import           Data.Store.Transaction (Transaction)
import qualified Lamdu.Data.Definition as Def
import           Lamdu.Expr.Scheme (Scheme)
import           Lamdu.Sugar.Types

import           Prelude.Compat

bitraverseExpression ::
    Applicative f =>
    (Body name m (Expression name m a) ->
     f (Body name m (Expression name m b))) ->
    (Payload m a -> f (Payload m b)) ->
    Expression name m a -> f (Expression name m b)
bitraverseExpression onBody onPl (Expression body pl) =
    Expression <$> onBody body <*> onPl pl

subExprPayloads ::
    Lens.IndexedTraversal
    (Expression name m ())
    (Expression name m a)
    (Expression name m b)
    (Payload m a) (Payload m b)
subExprPayloads f val@(Expression body pl) =
    Expression
    <$> (body & Lens.traversed .> subExprPayloads %%~ f)
    <*> Lens.indexed f (void val) pl

payloadsIndexedByPath ::
    Lens.IndexedTraversal
    [Expression name m ()]
    (Expression name m a)
    (Expression name m b)
    (Payload m a) (Payload m b)
payloadsIndexedByPath f =
    go []
    where
        go path val@(Expression body pl) =
            Expression
            <$> Lens.traversed (go newPath) body
            <*> Lens.indexed f newPath pl
            where
                newPath = void val : path

payloadsOf ::
    Lens.Fold (Body name m (Expression name m ())) a ->
    Lens.IndexedTraversal'
    (Expression name m ())
    (Expression name m b)
    (Payload m b)
payloadsOf body =
    subExprPayloads . Lens.ifiltered predicate
    where
        predicate idx _ = Lens.has (rBody . body) idx

holePayloads ::
    Lens.IndexedTraversal'
    (Expression name m ())
    (Expression name m a)
    (Payload m a)
holePayloads = payloadsOf _BodyHole

subExprsOf ::
    Lens.Traversal' (Body name m (Expression name m ())) b ->
    Lens.IndexedTraversal'
    [Expression name m ()]
    (Expression name m a)
    (Payload m a)
subExprsOf f =
    payloadsIndexedByPath . Lens.ifiltered predicate
    where
        predicate (_:parent:_) _ = Lens.has (rBody . f) parent
        predicate _ _ = False

holeArgs ::
    Lens.IndexedTraversal'
    [Expression name m ()]
    (Expression name m a)
    (Payload m a)
holeArgs = subExprsOf _BodyHole

defTypeInfoSchemes :: Lens.Traversal' (DefinitionTypeInfo m) Scheme
defTypeInfoSchemes f (DefinitionExportedTypeInfo s) =
    f s <&> DefinitionExportedTypeInfo
defTypeInfoSchemes f (DefinitionNewType (AcceptNewType ot nt a)) =
    DefinitionNewType <$>
    ( AcceptNewType
      <$> (ot & Def._ExportedType %%~ f)
      <*> f nt
      <*> pure a
    )

defBodySchemes :: Lens.Traversal' (DefinitionBody name m expr) Scheme
defBodySchemes f (DefinitionBodyBuiltin b) =
    b & biType %%~ f
    <&> DefinitionBodyBuiltin
defBodySchemes f (DefinitionBodyExpression de) =
    de & deTypeInfo . defTypeInfoSchemes %%~ f
    <&> DefinitionBodyExpression

defSchemes :: Lens.Traversal' (Definition name m expr) Scheme
defSchemes = drBody . defBodySchemes

binderNamedParams ::
    Lens.Traversal
    (BinderParams a m)
    (BinderParams b m)
    (FuncParam (NamedParamInfo a m))
    (FuncParam (NamedParamInfo b m))
binderNamedParams _ DefintionWithoutParams = pure DefintionWithoutParams
binderNamedParams _ (NullParam a) = pure (NullParam a)
binderNamedParams f (VarParam p) = VarParam <$> f p
binderNamedParams f (FieldParams ps) = FieldParams <$> (Lens.traverse . _2) f ps

binderNamedParamsActions ::
    Lens.Traversal' (BinderParams name m) (FuncParamActions m)
binderNamedParamsActions = binderNamedParams . fpInfo . npiMActions . Lens._Just

binderBodyFuncParamOps ::
    Lens.Traversal' (Binder name m (Expression name m a)) t ->
    Lens.Traversal' (BinderBody name m (Expression name m a)) t
binderBodyFuncParamOps ops = bbContent . binderContentFuncParamOps ops

binderContentFuncParamOps ::
    Lens.Traversal' (Binder name m (Expression name m a)) t ->
    Lens.Traversal' (BinderContent name m (Expression name m a)) t
binderContentFuncParamOps ops f (BinderLet Let{..}) =
    (\_lValue _lBody -> BinderLet Let{..})
    <$> ops f _lValue
    <*> binderBodyFuncParamOps ops f _lBody
binderContentFuncParamOps ops f (BinderExpr e) =
    onExpr e <&> BinderExpr
    where
        onExpr = rBody %%~ onBody
        onBody (BodyLam lam) =
            lam & lamBinder . ops %%~ f <&> BodyLam
        onBody body = body & Lens.traversed %%~ onExpr

binderFuncParamAdds ::
    Lens.Traversal'
    (Binder name m (Expression name m a))
    (Transaction m ParamAddResult)
binderFuncParamAdds f Binder{..} =
    (\_bParams _bBody _bMActions -> Binder{..})
    <$> (_bParams & binderNamedParamsActions . fpAddNext %%~ f)
    <*> binderBodyFuncParamOps binderFuncParamAdds f _bBody
    <*> (_bMActions & Lens._Just . baAddFirstParam %%~ f)

binderFuncParamDeletes ::
    Lens.Traversal'
    (Binder name m (Expression name m a))
    (Transaction m ParamDelResult)
binderFuncParamDeletes f Binder{..} =
    (\_bParams _bBody -> Binder{..})
    <$> (_bParams & binderNamedParamsActions . fpDelete %%~ f)
    <*> binderBodyFuncParamOps binderFuncParamDeletes f _bBody

binderContentExpr :: Lens' (BinderContent name m a) a
binderContentExpr f (BinderLet l) = l & lBody . bbContent . binderContentExpr %%~ f <&> BinderLet
binderContentExpr f (BinderExpr e) = f e <&> BinderExpr

binderContentEntityId ::
    Lens' (BinderContent name m (Expression name m a)) EntityId
binderContentEntityId f (BinderExpr e) =
    e & rPayload . plEntityId %%~ f <&> BinderExpr
binderContentEntityId f (BinderLet l) =
    l & lEntityId %%~ f <&> BinderLet
