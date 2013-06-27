{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}
module Utils where

import Control.Applicative ((<$), (<$>), (<*>))
import Control.Lens.Operators
import Control.Monad (void)
import Data.Map (Map, (!))
import Data.Monoid (mappend, mconcat)
import Data.Store.Guid (Guid)
import Lamdu.Data.Expression (Expression(..), Kind(..))
import Lamdu.Data.Expression.IRef (DefI)
import Lamdu.Data.Expression.Utils (pureHole, pureIntegerType)
import Lamdu.Data.ExampleDB (createBuiltins)
import qualified Control.Lens as Lens
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as Map
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Map as MapStore
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.IRef as ExprIRef
import qualified Lamdu.Data.Expression.Infer as Infer
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.Utils as ExprUtil

type PureExpr def = Expr.Expression def ()
type PureExprDefI t = PureExpr (DefI t)

(==>) :: k -> v -> Map k v
(==>) = Map.singleton

data UnescapedStr = UnescapedStr String
instance Show UnescapedStr where
  show (UnescapedStr x) = x

showStructure :: Show def => Expr.Body def a -> String
showStructure = show . (UnescapedStr "" <$)

instance Show def => Show (PureExpr def) where
  show (Expr.Expression value ()) = show value

namedLambda :: String -> expr -> expr -> Expr.Body def expr
namedLambda = ExprUtil.makeLambda . Guid.fromString

namedPi :: String -> expr -> expr -> Expr.Body def expr
namedPi = ExprUtil.makePi . Guid.fromString

pureApply :: [PureExpr def] -> PureExpr def
pureApply = foldl1 ExprUtil.pureApply

bodySet :: Expr.Body def expr
bodySet = ExprLens.bodyType # ()

bodyHole :: Expr.Body def expr
bodyHole = ExprLens.bodyHole # ()

bodyIntegerType :: Expr.Body def expr
bodyIntegerType = ExprLens.bodyIntegerType # ()

-- 1 dependent param
pureApplyPoly1 ::
  String ->
  [ExprIRef.Expression t ()] ->
  ExprIRef.Expression t ()
pureApplyPoly1 name xs = pureApply $ pureGetDef name : pureHole : xs

pureLambda ::
  String -> PureExpr def ->
  PureExpr def ->
  PureExpr def
pureLambda name x y = ExprUtil.pureExpression $ namedLambda name x y

purePi ::
  String -> PureExpr def ->
  PureExpr def ->
  PureExpr def
purePi name x y = ExprUtil.pureExpression $ namedPi name x y

pureLiteralInt :: Lens.Prism' (PureExpr def) Integer
pureLiteralInt = ExprLens.pureExpr . ExprLens.bodyLiteralInteger

pureGetDef :: String -> PureExprDefI t
pureGetDef name =
  ExprLens.pureExpr . ExprLens.bodyDefinitionRef #
  IRef.unsafeFromGuid (Guid.fromString name)

pureGetParam :: String -> PureExpr def
pureGetParam name =
  ExprLens.pureExpr . ExprLens.bodyParameterRef #
  Guid.fromString name

pureGetRecursiveDefI :: PureExprDefI t
pureGetRecursiveDefI =
  ExprLens.pureExpr . ExprLens.bodyDefinitionRef # recursiveDefI

pureParameterRef :: String -> PureExpr def
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

definitionTypes :: Map Guid (PureExprDefI t)
definitionTypes =
  exampleDBDefs `mappend` extras
  where
    g = Guid.fromString
    extras =
      mconcat
      [ g "IntToBoolFunc" ==> purePi "intToBool" pureIntegerType (pureGetDef "Bool")
      ]
    exampleDBDefs =
      fst . MapStore.runEmpty . Transaction.run MapStore.mapStore $ do
        (_, defIs) <- createBuiltins
        Lens.mapMOf (Lens.traversed . ExprLens.exprDef) reIRef
          =<< Map.fromList <$> mapM readDef defIs

    reIRef = fmap IRef.unsafeFromGuid . guidNameOf
    guidNameOf =
      fmap Guid.fromString . Transaction.getP . Anchors.assocNameRef . IRef.guid
    readDef defI =
      (,)
      <$> guidNameOf defI
      <*>
      (fmap void . ExprIRef.readExpression . (^. Definition.defType) =<<
       Transaction.readIRef defI)

recursiveDefI :: DefI t
recursiveDefI = IRef.unsafeFromGuid $ Guid.fromString "Definition"

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

defParamTags :: String -> [Guid]
defParamTags defName =
  innerMostPi (definitionTypes ! Guid.fromString defName) ^.. piTags

simplifyDef :: ExprIRef.Expression t a -> Expression UnescapedStr a
simplifyDef =
  ExprLens.exprDef %~ defIStr
  where
    defIStr = UnescapedStr . BS8.unpack . BS8.takeWhile (/= '\0') . Guid.bs . IRef.guid

showRestricted :: Expr.Expression def Infer.IsRestrictedPoly -> Expr.Expression def UnescapedStr
showRestricted = fmap restrictedStr
  where
    restrictedStr Infer.UnrestrictedPoly = UnescapedStr ""
    restrictedStr Infer.RestrictedPoly = UnescapedStr $ ansiAround ansiYellow "R"

canonizeDebug :: Expression def a -> Expression def a
canonizeDebug = ExprUtil.randomizeParamIdsG id ExprUtil.debugNameGen Map.empty (\_ _ -> id)

showInferred :: ExprIRef.Expression t Infer.IsRestrictedPoly -> String
showInferred =
  show . showRestricted . simplifyDef . canonizeDebug

showInferredValType :: Expression def (Infer.Inferred (DefI t)) -> String
showInferredValType expr =
  unlines
  [ "Inferred val:  " ++ f Infer.iValue
  , "Inferred type: " ++ f Infer.iType
  ]
  where
    f t = expr ^. Expr.ePayload . Lens.to (showInferred . t)
