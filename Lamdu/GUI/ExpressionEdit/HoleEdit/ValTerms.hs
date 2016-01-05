{-# LANGUAGE LambdaCase, NoImplicitPrelude #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.ValTerms
    ( val, ofNamePair
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Data.Binary.Utils (decodeS)
import qualified Data.ByteString.UTF8 as UTF8
import qualified Lamdu.Builtins.Anchors as Builtins
import qualified Lamdu.Expr.Val as V
import           Lamdu.Formatting (Format(..))
import           Lamdu.Sugar.Names.Types (Name(..), NameSource(..), NameCollision(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Prelude.Compat

ofName :: Name m -> Maybe String
ofName (Name NameSourceOutOfScope _ _ _) = Nothing
ofName (Name _ NoCollision _ varName) = Just varName
ofName (Name _ (Collision suffix) _ varName) = Just (varName ++ show suffix)

ofNamePair :: (Sugar.NameType, Name m) -> [String]
ofNamePair (tagName, name) =
    case tagName of
    Sugar.TagName -> names ++ (names <&> ('.' :))
    _ -> names
    where
        names = ofName name ^.. Lens._Just

formatLiteral :: Sugar.Literal -> String
formatLiteral (Sugar.LiteralNum i) = format i
formatLiteral (Sugar.LiteralText i) = format i
formatLiteral (Sugar.LiteralBytes i) = format i

val :: V.Val () -> [String]
val (V.Val () body) =
    case body of
    V.BLeaf leaf ->
        case leaf of
        V.LLiteral (V.Literal litId litBS)
            | litId == Builtins.floatId ->
              [decodeS litBS & Sugar.LiteralNum & formatLiteral]
            | litId == Builtins.bytesId ->
              [Sugar.LiteralBytes litBS & formatLiteral]
            | otherwise -> []
        V.LRecEmpty -> ["record", "{}", "()", "empty"]
        V.LAbsurd -> ["absurd"]
        V.LHole -> []
        V.LVar {} -> []
        V.LGlobal {} -> []
    V.BRecExtend {} -> ["record", "{}"]
    V.BFromNom {} -> ["fromNom"]
    V.BToNom (V.Nom nomId (V.Val () inner)) ->
        case inner of
        V.BLeaf (V.LLiteral (V.Literal litId litBS))
            | nomId == Builtins.textTid && litId == Builtins.bytesId ->
              [UTF8.toString litBS & Sugar.LiteralText & formatLiteral]
        _ | nomId == Builtins.listTid -> ["list", "[]"]
        _ -> []
    V.BAbs {} -> ["lambda", "\\", "Λ", "λ"]
    V.BApp (V.Apply (V.Val () func) _) ->
        case func of
        V.BCase {} -> ["case", ":"]
        _ -> ["apply"]
    V.BCase {} -> ["case", ":"]
    V.BGetField {} -> [".", "field"]
    V.BInject {} -> ["inject"]
    -- TODO: How can we do "params"?
