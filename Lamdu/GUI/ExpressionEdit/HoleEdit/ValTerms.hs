{-# LANGUAGE LambdaCase, NoImplicitPrelude #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.ValTerms
    ( body, ofName
    ) where

import           Control.Lens.Operators
import           Lamdu.Formatting (Format(..))
import           Lamdu.Sugar.Names.Types (Name(..), NameCollision(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Prelude.Compat

ofName :: Name m -> String
ofName (Name _ NoCollision _ varName) = varName
ofName (Name _ (Collision suffix) _ varName) = varName ++ show suffix

formatLiteral :: Sugar.Literal -> String
formatLiteral (Sugar.LiteralNum i) = format i
formatLiteral (Sugar.LiteralText i) = format i
formatLiteral (Sugar.LiteralBytes i) = format i

body :: Sugar.Body (Name m) m expr -> [String]
body =
    \case
    Sugar.BodyLam {} -> ["lambda", "\\", "Λ", "λ"]
    Sugar.BodyApply {} -> ["Apply"]
    Sugar.BodyList {} -> ["list", "[]"]
    Sugar.BodyRecord r ->
        ["record", "{}", "()"] ++
        case r of
        Sugar.Record [] Sugar.ClosedRecord{} _ -> ["empty"]
        _ -> []
    Sugar.BodyGetField gf ->
        [".", "field", "." ++ ofName (gf ^. Sugar.gfTag . Sugar.tagGName)]
    Sugar.BodyCase cas ->
        ["case", ":"] ++
        case cas of
            Sugar.Case Sugar.LambdaCase [] Sugar.ClosedCase{} _ _ -> ["absurd"]
            _ -> []
    Sugar.BodyInject {} -> ["inject", "[]"]
    Sugar.BodyLiteral i -> [formatLiteral i]
    Sugar.BodyGetVar Sugar.GetParamsRecord {} -> ["Params"]
    Sugar.BodyGetVar {} -> []
    Sugar.BodyToNom {} -> []
    Sugar.BodyFromNom {} -> []
    Sugar.BodyHole {} -> []
