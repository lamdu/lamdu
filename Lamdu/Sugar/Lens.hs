{-# LANGUAGE NoImplicitPrelude, FlexibleContexts, RecordWildCards #-}
module Lamdu.Sugar.Lens
    ( subExprPayloads, payloadsIndexedByPath
    , holePayloads, holeArgs
    , defSchemes
    , binderParams
    , binderParamsActions
    , binderFuncParamAdds
    , binderFuncParamDeletes
    ) where

import           Prelude.Compat

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad (void)
import           Data.Store.Transaction (Transaction)
import qualified Lamdu.Data.Definition as Def
import           Lamdu.Expr.Scheme (Scheme)
import           Lamdu.Sugar.Types

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

holePayloads ::
    Lens.IndexedTraversal'
    (Expression name m ())
    (Expression name m a)
    (Payload m a)
holePayloads =
    subExprPayloads . Lens.ifiltered predicate
    where
        predicate idx _ = Lens.has (rBody . _BodyHole) idx

holeArgs ::
    Lens.IndexedTraversal'
    [Expression name m ()]
    (Expression name m a)
    (Payload m a)
holeArgs =
    payloadsIndexedByPath . Lens.ifiltered predicate
    where
        predicate (_:parent:_) _ = Lens.has (rBody . _BodyHole) parent
        predicate _ _ = False

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

binderParams ::
    Lens.Traversal
    (BinderParams a m)
    (BinderParams b m)
    (FuncParam a m)
    (FuncParam b m)
binderParams _ DefintionWithoutParams = pure DefintionWithoutParams
binderParams _ (NullParam a) = pure (NullParam a)
binderParams f (VarParam p) = VarParam <$> f p
binderParams f (FieldParams ps) = FieldParams <$> (Lens.traverse . _2) f ps

binderParamsActions ::
    Lens.Traversal' (BinderParams name m) (FuncParamActions m)
binderParamsActions = binderParams . fpInfo . fpiMActions . Lens._Just

binderFuncParamAdds ::
    Lens.Traversal'
    (Binder name m (Expression name m a))
    (Transaction m ParamAddResult)
binderFuncParamAdds f Binder{..} =
    (\_bParams _bBody _bLetItems _bMActions -> Binder{..})
    <$> (_bParams & binderParamsActions . fpAddNext %%~ f)
    <*> onExpr _bBody
    <*> (_bLetItems & Lens.traversed . liValue . binderFuncParamAdds %%~ f)
    <*> (_bMActions & Lens._Just . baAddFirstParam %%~ f)
    where
        onExpr = rBody %%~ onBody
        onBody (BodyLam binder) = binder & binderFuncParamAdds %%~ f <&> BodyLam
        onBody body = body & Lens.traversed %%~ onExpr

binderFuncParamDeletes ::
    Lens.Traversal'
    (Binder name m (Expression name m a))
    (Transaction m ParamDelResult)
binderFuncParamDeletes f Binder{..} =
    (\_bParams _bBody _bLetItems -> Binder{..})
    <$> (_bParams & binderParamsActions . fpDelete %%~ f)
    <*> onExpr _bBody
    <*> (_bLetItems & Lens.traversed . liValue . binderFuncParamDeletes %%~ f)
    where
        onExpr = rBody %%~ onBody
        onBody (BodyLam binder) = binder & binderFuncParamDeletes %%~ f <&> BodyLam
        onBody body = body & Lens.traversed %%~ onExpr
