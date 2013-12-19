{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, OverlappingInstances, GeneralizedNewtypeDeriving #-}
module Utils where

import Control.Applicative ((<$), (<$>), (<*>))
import Control.Lens.Operators
import Control.Monad (void)
import Data.Binary (Binary)
import Data.Map (Map, (!))
import Data.Monoid (mappend)
import Data.Store.Guid (Guid)
import Lamdu.Data.ExampleDB (createBuiltins)
import Lamdu.Data.Expr (Kind(..))
import Lamdu.Data.Expr.IRef (DefI)
import Lamdu.Data.Expr.Utils (pureHole, pureIntegerType)
import Lamdu.Data.Infer.Deref (DerefedTV(..), Restriction(..), dValue, dType)
import qualified Control.Lens as Lens
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Map as MapStore
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Expr as Expr
import qualified Lamdu.Data.Expr.IRef as ExprIRef
import qualified Lamdu.Data.Expr.Lens as ExprLens
import qualified Lamdu.Data.Expr.Utils as ExprUtil

newtype Def = Def String
  deriving (Eq, Ord, Binary)
instance Show Def where
  show (Def d) = '#':d
type Expr = Expr.Expr Def

(==>) :: k -> v -> Map k v
(==>) = Map.singleton

data UnescapedStr = UnescapedStr String
instance Show UnescapedStr where
  show (UnescapedStr x) = x

showStructure :: Show def => Expr.Body def a -> String
showStructure = show . (UnescapedStr "" <$)

namedLambda :: String -> expr -> expr -> Expr.Body def expr
namedLambda = ExprUtil.makeLambda . Guid.fromString

namedPi :: String -> expr -> expr -> Expr.Body def expr
namedPi = ExprUtil.makePi . Guid.fromString

pureApply :: [Expr.Expr def ()] -> Expr.Expr def ()
pureApply = foldl1 ExprUtil.pureApply

bodySet :: Expr.Body def expr
bodySet = ExprLens.bodyType # ()

bodyHole :: Expr.Body def expr
bodyHole = ExprLens.bodyHole # ()

bodyIntegerType :: Expr.Body def expr
bodyIntegerType = ExprLens.bodyIntegerType # ()

-- 1 dependent param
pureApplyPoly1 :: String -> [Expr ()] -> Expr ()
pureApplyPoly1 name xs = pureApply $ pureGetDef name : pureHole : xs

pureLambda ::
  String ->
  Expr.Expr def () ->
  Expr.Expr def () ->
  Expr.Expr def ()
pureLambda name x y = ExprUtil.pureExpr $ namedLambda name x y

purePi ::
  String ->
  Expr.Expr def () ->
  Expr.Expr def () ->
  Expr.Expr def ()
purePi name x y = ExprUtil.pureExpr $ namedPi name x y

pureLiteralInt :: Lens.Prism' (Expr.Expr def ()) Integer
pureLiteralInt = ExprLens.pureExpr . ExprLens.bodyLiteralInteger

pureGetDef :: String -> Expr ()
pureGetDef name =
  ExprLens.pureExpr . ExprLens.bodyDefinitionRef # Def name

pureGetParam :: String -> Expr.Expr def ()
pureGetParam name =
  ExprLens.pureExpr . ExprLens.bodyParameterRef #
  Guid.fromString name

pureGetRecursiveDefI :: Expr ()
pureGetRecursiveDefI =
  ExprLens.pureExpr . ExprLens.bodyDefinitionRef # recursiveDefI

pureParameterRef :: String -> Expr.Expr def ()
pureParameterRef str =
  ExprLens.pureExpr . ExprLens.bodyParameterRef # Guid.fromString str

ansiRed :: String
ansiRed = "\ESC[31m"
ansiYellow :: String
ansiYellow = "\ESC[1;33m"
ansiReset :: String
ansiReset = "\ESC[0m"
ansiAround :: String -> String -> String
ansiAround prefix x = prefix ++ x ++ ansiReset

definitionTypes :: Map Def (Expr ())
definitionTypes =
  exampleDBDefs `mappend` extras
  where
    extras = Def "IntToBoolFunc" ==> purePi "intToBool" pureIntegerType (pureGetDef "Bool")
    exampleDBDefs =
      fst . MapStore.runEmpty . Transaction.run MapStore.mapStore $ do
        (_, defIs) <- createBuiltins id
        (Lens.traversed . ExprLens.exprDef %%~ nameOf)
          =<< Map.fromList <$> mapM readDef defIs

    nameOf = fmap Def . Transaction.getP . Anchors.assocNameRef . IRef.guid
    readDef defI =
      (,)
      <$> nameOf defI
      <*>
      (fmap void . ExprIRef.readExpr . (^. Definition.bodyType) =<<
       Transaction.readIRef defI)

recursiveDefI :: Def
recursiveDefI = Def "recursiveDefI"

innerMostPi :: Expr.Expr def a -> Expr.Expr def a
innerMostPi =
  last . pis
  where
    pis expr =
      case expr ^? ExprLens.exprKindedLam KType . Lens._3 of
      Just resultType -> expr : pis resultType
      _ -> []

piTags :: Lens.Traversal' (Expr.Expr def a) Guid
piTags =
  ExprLens.exprKindedLam KType . Lens._2 .
  ExprLens.exprKindedRecordFields KType .
  Lens.traversed . Lens._1 . ExprLens.exprTag

defParamTags :: Def -> [Guid]
defParamTags defName =
  innerMostPi (definitionTypes ! defName) ^.. piTags

showRestrictions :: Show def => [Restriction def] -> UnescapedStr
showRestrictions xs =
  UnescapedStr .
  List.intercalate "," $
  map (ansiAround ansiYellow . show) xs

canonizeDebug :: Expr.Expr def a -> Expr.Expr def a
canonizeDebug = ExprUtil.randomizeParamIdsG id ExprUtil.debugNameGen Map.empty (\_ _ -> id)

showDerefed :: Show def => Expr.Expr def a -> String
showDerefed = show . canonizeDebug . void

showInferredValType :: Expr.Expr def (DerefedTV (DefI t)) -> String
showInferredValType expr =
  unlines
  [ "Inferred val:  " ++ showDerefed (derefed ^. dValue)
  , "Inferred type: " ++ showDerefed (derefed ^. dType)
  ]
  where
    derefed = expr ^. Expr.ePayload
