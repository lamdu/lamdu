{-# LANGUAGE ScopedTypeVariables #-}
module Lamdu.Sugar.Convert.DefExpr
    ( convert
    ) where

import qualified Control.Lens as Lens
import qualified Data.Property as Property
import           Data.Tree.Diverse (ann)
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Type.Scheme as Scheme
import qualified Lamdu.Data.Definition as Definition
import           Lamdu.Expr.IRef (DefI)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Infer as Infer
import           Lamdu.Sugar.Convert.Binder (convertDefinitionBinder)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.Convert.Type as ConvertType
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

convert ::
    (Monoid a, Monad m) =>
    Scheme.Scheme -> Definition.Expr (Val (Input.Payload m a)) -> DefI m ->
    ConvertM m (DefinitionBody InternalName (T m) (T m) (ConvertPayload m a))
convert defType defExpr defI =
    do
        (presMode, content) <-
            convertDefinitionBinder defI (defExpr ^. Definition.expr)
        inferContext <- Lens.view ConvertM.scInferContext
        let inferredType =
                defExpr ^. Definition.expr . ann . Input.inferredType
                & Infer.makeScheme inferContext
        unless (Scheme.alphaEq defType inferredType) $
            fail "Def type mismatches its inferred type!"
        defTypeS <- ConvertType.convertScheme (EntityId.currentTypeOf entityId) defType
        DefinitionBodyExpression DefinitionExpression
            { _deType = defTypeS
            , _dePresentationMode = presMode <&> (^. Property.mkProperty)
            , _deContent = content
            } & pure
    where
        entityId = ExprIRef.globalId defI & EntityId.ofBinder
