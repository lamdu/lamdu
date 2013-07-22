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
import Lamdu.Data.Expression (Expression(..), Kind(..))
import Lamdu.Data.Expression.IRef (DefI)
import Lamdu.Data.Expression.Utils (pureHole, pureIntegerType)
import Lamdu.Data.Infer.Deref (Derefed(..), Restrictions(..))
import qualified Control.Lens as Lens
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Map as MapStore
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.IRef as ExprIRef
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.Utils as ExprUtil

newtype Def = Def String
  deriving (Eq, Ord, Binary)
instance Show Def where
  show (Def d) = '#':d
type Expr = Expr.Expression Def

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

pureApply :: [Expr.Expression def ()] -> Expr.Expression def ()
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
  Expr.Expression def () ->
  Expr.Expression def () ->
  Expr.Expression def ()
pureLambda name x y = ExprUtil.pureExpression $ namedLambda name x y

purePi ::
  String ->
  Expr.Expression def () ->
  Expr.Expression def () ->
  Expr.Expression def ()
purePi name x y = ExprUtil.pureExpression $ namedPi name x y

pureLiteralInt :: Lens.Prism' (Expr.Expression def ()) Integer
pureLiteralInt = ExprLens.pureExpr . ExprLens.bodyLiteralInteger

pureGetDef :: String -> Expr ()
pureGetDef name =
  ExprLens.pureExpr . ExprLens.bodyDefinitionRef # Def name

pureGetParam :: String -> Expr.Expression def ()
pureGetParam name =
  ExprLens.pureExpr . ExprLens.bodyParameterRef #
  Guid.fromString name

pureGetRecursiveDefI :: Expr ()
pureGetRecursiveDefI =
  ExprLens.pureExpr . ExprLens.bodyDefinitionRef # recursiveDefI

pureParameterRef :: String -> Expr.Expression def ()
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
      (fmap void . ExprIRef.readExpression . (^. Definition.bodyType) =<<
       Transaction.readIRef defI)

recursiveDefI :: Def
recursiveDefI = Def "recursiveDefI"

innerMostPi :: Expression def a -> Expression def a
innerMostPi =
  last . pis
  where
    pis expr =
      case expr ^? ExprLens.exprKindedLam KType . Lens._3 of
      Just resultType -> expr : pis resultType
      _ -> []

piTags :: Lens.Traversal' (Expression def a) Guid
piTags =
  ExprLens.exprKindedLam KType . Lens._2 .
  ExprLens.exprKindedRecordFields KType .
  Lens.traversed . Lens._1 . ExprLens.exprTag

defParamTags :: Def -> [Guid]
defParamTags defName =
  innerMostPi (definitionTypes ! defName) ^.. piTags

showRestrictions :: Show def => Restrictions def -> UnescapedStr
showRestrictions (Restrictions xs) =
  UnescapedStr .
  List.intercalate "," $
  map (ansiAround ansiYellow . show) xs

canonizeDebug :: Expression def a -> Expression def a
canonizeDebug = ExprUtil.randomizeParamIdsG id ExprUtil.debugNameGen Map.empty (\_ _ -> id)

showDerefed :: Show def => Expression def (Restrictions def) -> String
showDerefed = show . fmap showRestrictions . canonizeDebug

showInferredValType :: Expression def (Derefed (DefI t)) -> String
showInferredValType expr =
  unlines
  [ "Inferred val:  " ++ showDerefed val
  , "Inferred type: " ++ showDerefed typ
  ]
  where
    Derefed val typ = expr ^. Expr.ePayload
