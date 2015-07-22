module Lamdu.Sugar.Convert.Hole.Suggest
    ( suggestValueWith, suggestRecordWith
    , suggestValueConversion
    , stateMkVar
    ) where

import           Control.Applicative (Applicative(..), (<$>))
import           Control.Compose ((:.)(..), unO)
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.MonadA (MonadA)
import           Control.Monad.Trans.State (StateT(..))
import qualified Control.Monad.Trans.State as State
import           Data.Monoid (Monoid(..))
import           Data.String (IsString(..))
import qualified Lamdu.Expr.Lens as ExprLens
import           Lamdu.Expr.Nominal (Nominal)
import qualified Lamdu.Expr.Nominal as Nominal
import qualified Lamdu.Expr.Pure as P
import           Lamdu.Expr.Scheme (schemeType)
import           Lamdu.Expr.Type (Type)
import qualified Lamdu.Expr.Type as T
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Val as V

stateMkVar :: Monad m => StateT Int m V.Var
stateMkVar =
    do
        i <- State.get
        State.modify (+1)
        "var" ++ show i & fromString & return

suggestValueConversion ::
    (Monoid a, Applicative f, MonadA m) =>
    (T.Id -> m Nominal) ->
    f V.Var -> Val a -> Type -> Type -> m [f (Val a)]
suggestValueConversion loadNominal mkVar arg (T.TInst name params) r =
    do
        fromNomType <-
            loadNominal name <&> Nominal.apply params
            -- TODO: Instantiate instead of access type?
            -- I think this happens to be fine for suggest but there are less
            -- doubts if using a proper instantiantion of the scheme..
            <&> (^. schemeType)
        suggestValueConversion loadNominal mkVar fromNom fromNomType r
    <&> (++ [pure fromNom])
    where
        fromNom = V.Nom name arg & V.BFromNom & V.Val mempty
suggestValueConversion _ mkVar arg (T.TSum composite) r =
    suggestCaseWith mkVar composite r
    <&> Lens.mapped %~ applyCase
    & return
    where
        applyCase c =
            c
            & Lens.traversed .~ mempty
            & (`V.Apply` arg) & V.BApp & V.Val mempty
suggestValueConversion _ _ arg (T.TRecord composite) _ =
    composite ^.. ExprLens.compositeTags
    <&> pure . V.Val mempty . V.BGetField . V.GetField arg
    & return
suggestValueConversion _ _ _ _ _ = return []

suggestValueWith :: Applicative f => f V.Var -> Type -> [f (Val ())]
suggestValueWith _ T.TVar{}                  = [pure P.hole]
suggestValueWith _ T.TInst{}                 = [pure P.hole]
-- TODO: Need access to the Nominals map here, to only suggest
-- nominals that can fromNominal, and also offer to build the inner
-- value
suggestValueWith mkVar (T.TSum (T.CExtend f t sumType)) =
    (suggestValueWith mkVar t <&> Lens.mapped %~ P.inject f)  ++
    suggestValueWith mkVar (T.TSum sumType)
suggestValueWith _ (T.TSum T.CEmpty)         = [] -- Void value uninhabitable
suggestValueWith _ (T.TSum T.CVar {})        = [pure P.hole]
suggestValueWith _ T.TInt                    = [pure P.hole]
suggestValueWith mkVar (T.TRecord composite) =
    suggestRecordWith mkVar composite
suggestValueWith mkVar (T.TFun (T.TSum composite) r) =
    suggestCaseWith mkVar composite r
suggestValueWith mkVar (T.TFun _ r)          =
    unO $ P.abs <$> O [mkVar] <*> O (suggestValueWith mkVar r)

suggestRecordWith :: Applicative f => f V.Var -> T.Product -> [f (Val ())]
suggestRecordWith _ T.CVar{}          = [pure P.hole]
suggestRecordWith _ T.CEmpty          = [pure P.recEmpty]
suggestRecordWith mkVar (T.CExtend f t r) =
    unO $ P.recExtend f <$> O (suggestValueWith mkVar t) <*> O (suggestRecordWith mkVar r)

suggestCaseWith :: Applicative f => f V.Var -> T.Sum -> Type -> [f (Val ())]
suggestCaseWith _ T.CVar{} _ = [pure P.hole]
suggestCaseWith _ T.CEmpty _ = [pure P.absurd]
suggestCaseWith mkVar (T.CExtend f t r) res =
    unO $ P._case f
    <$> O (suggestValueWith mkVar (T.TFun t res))
    <*> O (suggestCaseWith mkVar r res)
