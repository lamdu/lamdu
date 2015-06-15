{-# LANGUAGE FlexibleContexts, RecordWildCards #-}
module Lamdu.Sugar.Lens
    ( subExprPayloads, payloadsIndexedByPath
    , holePayloads, holeArgs
    , defSchemes
    , binderFuncParamAdds
    , binderFuncParamDeletes
    ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Monad (void)
import Data.Store.Transaction (Transaction)
import Lamdu.Expr.Scheme (Scheme)
import Lamdu.Sugar.Types
import qualified Lamdu.Data.Definition as Def
import qualified Control.Lens as Lens

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

binderParamsActions ::
    Lens.Traversal' (BinderParams name m) (FuncParamActions m)
binderParamsActions _ NoParams = pure NoParams
binderParamsActions f (VarParam p) =
    p & fpMActions . Lens._Just %%~ f <&> VarParam
binderParamsActions f (FieldParams ps) =
    ps & Lens.traversed . fpMActions . Lens._Just %%~ f <&> FieldParams

binderFuncParamAdds ::
    Lens.Traversal'
    (Binder name m (Expression name m a))
    (Transaction m ParamAddResult)
binderFuncParamAdds f Binder{..} =
    (\_bParams _bBody _bWhereItems _bMActions -> Binder{..})
    <$> (_bParams & binderParamsActions . fpAddNext %%~ f)
    <*> onExpr _bBody
    <*> (_bWhereItems & Lens.traversed . wiValue . binderFuncParamAdds %%~ f)
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
    (\_bParams _bBody _bWhereItems -> Binder{..})
    <$> (_bParams & binderParamsActions . fpDelete %%~ f)
    <*> onExpr _bBody
    <*> (_bWhereItems & Lens.traversed . wiValue . binderFuncParamDeletes %%~ f)
    where
        onExpr = rBody %%~ onBody
        onBody (BodyLam binder) = binder & binderFuncParamDeletes %%~ f <&> BodyLam
        onBody body = body & Lens.traversed %%~ onExpr
