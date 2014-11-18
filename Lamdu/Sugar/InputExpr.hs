module Lamdu.Sugar.InputExpr
  ( makePure
  , randomizeExprAndParams
  , randomizeParamIdsG

    -- TODO: To its own module?
  , onNgMakeName
  , NameGen(..), randomNameGen
  ) where

import Control.Applicative ((<$>), Applicative(..))
import Control.Lens.Operators
import Control.Lens.Tuple
import Control.Monad
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.State (evalState, state, runState)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Traversable (traverse)
import Lamdu.Expr.Identifier (Identifier(..))
import Lamdu.Expr.Val (Val(..))
import Lamdu.Sugar.Types (InputPayloadP(..), InputPayload)
import System.Random (Random, RandomGen, random)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Lamdu.Expr.Val as V
import qualified System.Random as Random

randomIdentifier :: RandomGen g => g -> (Identifier, g)
randomIdentifier =
  runState $ Identifier . BS.pack <$> replicateM idLength (state random)
  where
    idLength = 16

randomVar :: RandomGen g => g -> (V.Var, g)
randomVar g = randomIdentifier g & _1 %~ V.Var

randomizeExpr :: (RandomGen gen, Random r) => gen -> Val (r -> a) -> Val a
randomizeExpr gen (Val pl body) =
  (`evalState` gen) $ do
    r <- state random
    newBody <- body & traverse %%~ randomizeSubexpr
    return $ Val (pl r) newBody
  where
    randomizeSubexpr subExpr = do
      localGen <- state Random.split
      return $ randomizeExpr localGen subExpr

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
            & Lens.mapped . Lens.mapped . Lens._2 %~ go
            & onMakeName result
          , ngSplit =
            ngSplit nameGen
            & Lens.both %~ go
          }

randomNameGen :: RandomGen g => g -> NameGen V.Var dummy
randomNameGen g = NameGen
  { ngSplit = Random.split g & Lens.both %~ randomNameGen
  , ngMakeName = const . const $ randomVar g & Lens._2 %~ randomNameGen
  }

randomizeParamIdsG ::
  (a -> n) ->
  NameGen V.Var n -> Map V.Var V.Var ->
  (NameGen V.Var n -> Map V.Var V.Var -> a -> b) ->
  Val a -> Val b
randomizeParamIdsG preNG gen initMap convertPL =
  (`evalState` gen) . (`runReaderT` initMap) . go
  where
    go (Val s v) = do
      parMap <- Reader.ask
      newGen <- lift $ state ngSplit
      Val (convertPL newGen parMap s) <$>
        case v of
        V.BAbs (V.Lam oldParamId body) -> do
          newParamId <- lift . state $ makeName oldParamId s
          fmap (V.BAbs . V.Lam newParamId) $
            Reader.local (Map.insert oldParamId newParamId) $ go body
        V.BLeaf (V.LVar par) ->
          pure $ V.BLeaf $ V.LVar $ fromMaybe par $ Map.lookup par parMap
        x@V.BLeaf {}     -> traverse go x
        x@V.BApp {}      -> traverse go x
        x@V.BGetField {} -> traverse go x
        x@V.BRecExtend {}      -> traverse go x
    makeName oldParamId s nameGen =
      ngMakeName nameGen oldParamId $ preNG s

randomizeParamIds :: RandomGen g => g -> Val a -> Val a
randomizeParamIds gen = randomizeParamIdsG id (randomNameGen gen) Map.empty $ \_ _ a -> a

randomizeExprAndParams ::
  (RandomGen gen, Random r) =>
  gen -> Val (r -> a) -> Val a
randomizeExprAndParams gen = randomizeParamIds paramGen . randomizeExpr exprGen
  where
    (exprGen, paramGen) = Random.split gen

makePure :: RandomGen gen => gen -> Val a -> Val (InputPayload m a)
makePure gen =
  randomizeExprAndParams gen . fmap f
  where
    f a par = InputPayload par Nothing Nothing a
