{-# LANGUAGE NoImplicitPrelude #-}
-- TODO: Split/rename to more generic (non-sugar) modules
module Lamdu.Expr.GenIds
    ( randomizeExprAndParams
    , randomizeParamIdsG
    , randomizeParamIds
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
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import           Lamdu.Calc.Identifier (Identifier(..))
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val(..))
import           System.Random (Random, RandomGen, random)
import qualified System.Random as Random
import qualified System.Random.Utils as RandomUtils

import           Lamdu.Prelude

transaction :: Monad m =>
    (Random.StdGen -> (a, Random.StdGen)) -> Transaction m a
transaction f = Transaction.newKey <&> fst . f . RandomUtils.genFromHashable

randomIdentifier :: RandomGen g => g -> (Identifier, g)
randomIdentifier =
    runState $ Identifier . BS.pack <$> replicateM idLength (state random)
    where
        idLength = 16

randomVar :: RandomGen g => g -> (V.Var, g)
randomVar g = randomIdentifier g & _1 %~ V.Var

randomTag :: RandomGen g => g -> (T.Tag, g)
randomTag g = randomIdentifier g & _1 %~ T.Tag

randomizeExpr :: (RandomGen gen, Random r) => gen -> Val (r -> a) -> Val a
randomizeExpr gen (Val pl body) =
    (`evalState` gen) $
    do
        r <- state random
        newBody <- body & traverse %%~ randomizeSubexpr
        Val (pl r) newBody & pure
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
    (a -> n) ->
    NameGen V.Var n -> Map V.Var V.Var ->
    (NameGen V.Var n -> Map V.Var V.Var -> a -> b) ->
    Val a -> Val b
randomizeParamIdsG preNG gen initMap convertPL =
    (`evalState` gen) . (`runReaderT` initMap) . go
    where
        go (Val s v) =
            do
                parMap <- Reader.ask
                newGen <- lift $ state ngSplit
                Val (convertPL newGen parMap s) <$>
                    case v of
                    V.BLam (V.Lam oldParamId body) ->
                        do
                            newParamId <- lift . state $ makeName oldParamId s
                            go body
                                & Reader.local (Map.insert oldParamId newParamId)
                                <&> V.Lam newParamId
                                <&> V.BLam
                    V.BLeaf (V.LVar par) ->
                        pure $ V.BLeaf $ V.LVar $ fromMaybe par $ Map.lookup par parMap
                    x@V.BLeaf {}      -> traverse go x
                    x@V.BApp {}       -> traverse go x
                    x@V.BGetField {}  -> traverse go x
                    x@V.BRecExtend {} -> traverse go x
                    x@V.BCase {}      -> traverse go x
                    x@V.BInject {}    -> traverse go x
                    x@V.BFromNom {}   -> traverse go x
                    x@V.BToNom {}     -> traverse go x
        makeName oldParamId s nameGen =
            ngMakeName nameGen oldParamId $ preNG s

randomizeParamIds :: RandomGen g => g -> Val a -> Val a
randomizeParamIds gen = randomizeParamIdsG id (randomNameGen gen) Map.empty $ \_ _ a -> a

randomizeExprAndParams ::
    (RandomGen gen, Random r) =>
    gen -> Val (r -> a) -> Val a
randomizeExprAndParams gen =
    randomizeParamIds paramGen . randomizeExpr exprGen
    where
        (exprGen, paramGen) = Random.split gen
