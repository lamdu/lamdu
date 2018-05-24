module Lamdu.Sugar.Convert.PostProcess
    ( Result(..), def, expr
    ) where

import qualified Control.Lens as Lens
import           Data.Property (MkProperty')
import qualified Data.Property as Property
import qualified Lamdu.Calc.Val.Annotated as Val
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Debug as Debug
import           Lamdu.Expr.IRef (DefI, ValI)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Load as ExprLoad
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Infer.Error as InferErr
import qualified Lamdu.Sugar.Convert.Load as Load
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction

import           Lamdu.Prelude

type T = Transaction

data Result = GoodExpr | BadExpr InferErr.Error

def :: Monad m => Debug.Monitors -> DefI m -> T m Result
def monitors defI =
    do
        loadedDef <- ExprLoad.def defI <&> void
        case loadedDef ^. Definition.defBody of
            Definition.BodyBuiltin {} -> pure GoodExpr
            Definition.BodyExpr defExpr ->
                do
                    checked <-
                        ExprIRef.globalId defI
                        & Load.inferCheckDef monitors defExpr
                    case checked of
                        Left err -> BadExpr err & pure
                        Right (inferredVal, inferContext) ->
                            GoodExpr <$
                            ( loadedDef
                            & Definition.defType .~
                                Infer.makeScheme inferContext inferredType
                            & Definition.defBody . Definition._BodyExpr .
                                Definition.exprFrozenDeps .~
                                Definition.pruneDefExprDeps defExpr
                            & Definition.defBody . Lens.mapped %~
                                (^. Val.payload . Property.pVal)
                            & Transaction.writeIRef defI
                            )
                            where
                                inferredType = inferredVal ^. Val.payload . _1 . Infer.plType

expr ::
    Monad m =>
    Debug.Monitors -> MkProperty' (T m) (Definition.Expr (ValI m)) ->
    T m Result
expr monitors prop =
    do
        defExprLoaded <- ExprLoad.defExpr prop
        -- TODO: This is code duplication with the above Load.inferCheckDef
        -- & functions inside Load itself
        inferred <- Load.inferCheckDefExpr monitors defExprLoaded
        case inferred of
            Left err -> BadExpr err & pure
            Right _ ->
                GoodExpr <$
                Property.modP prop
                (Definition.exprFrozenDeps .~ Definition.pruneDefExprDeps defExprLoaded)

