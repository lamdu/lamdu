{-# LANGUAGE TemplateHaskell, RankNTypes, GADTs #-}

module Lamdu.Sugar.Convert.Binder.Redex
    ( Redex(..), lam, lamPl, paramRefs, arg
    , hmapRedex
    , check
    ) where

import qualified Control.Lens as Lens
import           Hyper
import           Hyper.Recurse (HRecWitness(..))
import           Hyper.Syntax (W_TypedLam(..))
import           Hyper.Type.Prune (Prune)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Types (EntityId)

import           Lamdu.Prelude

data Redex a = Redex
    { _lam :: V.TypedLam V.Var (HCompose Prune T.Type) V.Term # Ann (GetHyperType a)
    , _lamPl :: a :# V.Term
    , _paramRefs :: [EntityId]
    , _arg :: Ann (GetHyperType a) # V.Term
    }
Lens.makeLenses ''Redex

hmapRedex ::
    (forall n. HRecWitness V.Term n -> p # n -> q # n) ->
    Redex # p -> Redex # q
hmapRedex f r =
    r
    { _lamPl = f HRecSelf (r ^. lamPl)
    , _lam =
        hmap
        ( \case
            HWitness W_TypedLam_expr ->
                hflipped %~ hmap (\(HWitness w) -> f w)
            HWitness W_TypedLam_typ ->
                hflipped %~ hmap
                (\(HWitness w) -> f (HRecSub (HWitness V.W_Term_HCompose_Prune_Type) w))
        ) (r ^. lam)
    , _arg = r ^. arg & hflipped %~ hmap (\(HWitness w) -> f w)
    }

check ::
    V.Term # Ann (Input.Payload m a) ->
    Maybe (Redex # Input.Payload m a)
check term =
    do
        V.App func a <- term ^? V._BApp
        l <- func ^? hVal . V._BLam
        Just Redex
            { _lam = l
            , _lamPl = func ^. hAnn
            , _arg = a
            , _paramRefs = func ^. hAnn . Input.varRefsOfLambda
            }
