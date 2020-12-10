-- TODO: Split/rename to more generic (non-sugar) modules
{-# LANGUAGE ScopedTypeVariables, TypeApplications, GADTs #-}

module Lamdu.Expr.GenIds
    ( randomizeExprAndParams
    , randomizeParamIdsG
    , randomTag

    , transaction
        -- TODO: To its own module?
    , onNgMakeName
    , NameGen(..), randomNameGen
    ) where

import qualified Control.Lens as Lens
import           Control.Monad (replicateM)
import           Control.Monad.Trans.Reader (ReaderT(..))
import qualified Control.Monad.Trans.Reader as Reader
import           Control.Monad.Trans.State (evalState, state, runState)
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import           Hyper
import           Hyper.Recurse (recurse)
import           Lamdu.Calc.Identifier (Identifier(..))
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction
import           System.Random (Random, RandomGen, random)
import qualified System.Random.Extended as Random

import           Lamdu.Prelude

type T = Transaction

transaction :: Monad m => (Random.StdGen -> (a, Random.StdGen)) -> T m a
transaction f = Transaction.newKey <&> fst . f . Random.genFromHashable

randomIdentifier :: RandomGen g => g -> (Identifier, g)
randomIdentifier =
    runState $ Identifier . BS.pack <$> replicateM idLength (state random)
    where
        idLength = 16

randomVar :: RandomGen g => g -> (V.Var, g)
randomVar g = randomIdentifier g & _1 %~ V.Var

randomTag :: RandomGen g => g -> (T.Tag, g)
randomTag g = randomIdentifier g & _1 %~ T.Tag

randomizeExpr ::
    forall gen r t a.
    (RandomGen gen, Random r, RTraversable t) =>
    gen -> Ann (HFunc (Const r) a) # t -> Ann a # t
randomizeExpr gen (Ann (HFunc pl) body) =
    withDict (recurse (Proxy @(RTraversable t))) $
    (`evalState` gen) $
    do
        r <- state random
        newBody <- htraverse (Proxy @RTraversable #> randomizeSubexpr) body
        Ann (pl (Const r)) newBody & pure
    where
        randomizeSubexpr subExpr = state Random.split <&> (`randomizeExpr` subExpr)

data NameGen par pl = NameGen
    { ngSplit :: (NameGen par pl, NameGen par pl)
    , ngMakeName :: par -> pl -> (par, NameGen par pl)
    }

onNgMakeName ::
    (NameGen par b ->
     (par -> a -> (par, NameGen par b)) ->
     par -> b -> (par, NameGen par b)) ->
    NameGen par a -> NameGen par b
onNgMakeName onMakeName =
    go
    where
        go nameGen =
            result
            where
                result =
                    nameGen
                    { ngMakeName =
                        ngMakeName nameGen
                        <&> Lens.mapped . _2 %~ go
                        & onMakeName result
                    , ngSplit =
                        ngSplit nameGen
                        & Lens.both %~ go
                    }

randomNameGen :: RandomGen g => g -> NameGen V.Var dummy
randomNameGen g = NameGen
    { ngSplit = Random.split g & Lens.both %~ randomNameGen
    , ngMakeName = const . const $ randomVar g & _2 %~ randomNameGen
    }

randomizeParamIdsG ::
    (a # V.Term -> n) ->
    NameGen V.Var n -> Map V.Var V.Var ->
    Ann a # V.Term -> Ann a # V.Term
randomizeParamIdsG preNG gen initMap =
    (`evalState` gen) . (`runReaderT` initMap) . go
    where
        go (Ann s v) =
            case v of
            V.BLam (V.TypedLam oldParamId typ body) ->
                do
                    newParamId <- lift . state $ makeName oldParamId s
                    go body
                        & Reader.local (Map.insert oldParamId newParamId)
                        <&> V.TypedLam newParamId typ
                        <&> V.BLam
            V.BLeaf (V.LVar par) ->
                Reader.ask <&> Map.lookup par <&> fromMaybe par
                <&> V.LVar <&> V.BLeaf
            V.BLeaf{}      -> def
            V.BApp{}       -> def
            V.BGetField{}  -> def
            V.BRecExtend{} -> def
            V.BCase{}      -> def
            V.BInject{}    -> def
            V.BToNom{}     -> def
            <&> Ann s
            where
                def =
                    htraverse
                    ( \case
                        HWitness V.W_Term_Term -> go
                        HWitness V.W_Term_HCompose_Prune_Type -> pure
                    ) v
        makeName oldParamId s nameGen =
            ngMakeName nameGen oldParamId $ preNG s

randomizeParamIds ::
    RandomGen g =>
    g -> Ann a # V.Term -> Ann a # V.Term
randomizeParamIds gen = randomizeParamIdsG id (randomNameGen gen) Map.empty

randomizeExprAndParams ::
    (RandomGen gen, Random r) =>
    gen -> Ann (HFunc (Const r) a) # V.Term -> Ann a # V.Term
randomizeExprAndParams gen =
    randomizeParamIds paramGen . randomizeExpr exprGen
    where
        (exprGen, paramGen) = Random.split gen
