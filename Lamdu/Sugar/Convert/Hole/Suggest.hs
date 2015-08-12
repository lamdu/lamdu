{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.Sugar.Convert.Hole.Suggest
    ( value
    , valueConversion
    , valueNoSplit
    ) where

import           Prelude.Compat

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.MonadA (MonadA)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lamdu.Expr.Lens as ExprLens
import           Lamdu.Expr.Nominal (Nominal)
import qualified Lamdu.Expr.Nominal as Nominal
import           Lamdu.Expr.Scheme (schemeType)
import           Lamdu.Expr.Type (Type)
import qualified Lamdu.Expr.Type as T
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Infer as Infer

loadNominalsForType :: Monad m => (T.Id -> m Nominal) -> Type -> m Infer.Loaded
loadNominalsForType loadNominal typ =
    go Map.empty (typ ^. ExprLens.typeTIds . Lens.to Set.singleton)
    where
        go res toLoad
            | Set.null toLoad = Infer.Loaded Map.empty res & return
            | otherwise =
                do
                    loadedNominals <- Map.fromSet loadNominal toLoad & sequenceA
                    let result = mappend res loadedNominals
                    let newTIds =
                            loadedNominals
                            ^. Lens.traversed . Lens.to Nominal.nScheme . schemeType
                            . ExprLens.typeTIds . Lens.to Set.singleton
                            & (`Set.difference` Map.keysSet result)
                    go result newTIds

valueConversion ::
    MonadA m =>
    (T.Id -> m Nominal) -> a ->
    Val (Type, a) -> Type -> m [Val (Type, a)]
valueConversion loadNominal empty src dstType =
    do
        loaded <- loadNominalsForType loadNominal (src ^. V.payload . _1)
        valueConversionH loaded empty src dstType & return

valueConversionH ::
    Infer.Loaded -> a -> Val (Type, a) -> Type -> [Val (Type, a)]
valueConversionH loaded empty src dstType =
    case src ^. V.payload . _1 of
    T.TRecord composite ->
        composite ^.. ExprLens.compositeFields
        <&> getField
        where
            getField (tag, typ) =
                V.GetField src tag & V.BGetField & V.Val (typ, empty)
    _ -> [valueConversionNoSplit loaded empty src dstType]

valueConversionNoSplit ::
    Infer.Loaded -> a -> Val (Type, a) -> Type -> Val (Type, a)
valueConversionNoSplit loaded empty src dstType =
    case src ^. V.payload . _1 of
    T.TInst name params | bodyNot ExprLens._BToNom ->
        valueConversionNoSplit loaded empty fromNom dstType
        where
            fromNom = V.Nom name src & V.BFromNom & V.Val (fromNomType, empty)
            fromNomType =
                Infer.loadedNominals loaded Map.! name
                & Nominal.apply params
                -- TODO: Instantiate instead of access type?
                -- I think this happens to be fine for suggest but there are less
                -- doubts if using a proper instantiantion of the scheme..
                & (^. schemeType)
    T.TFun argType resType | bodyNot ExprLens._BAbs ->
        if Lens.has (ExprLens.valLeafs . ExprLens._LHole) arg
            then
                -- If the suggested argument has holes in it
                -- then stop suggesting there to avoid "overwhelming"..
                applied
            else valueConversionNoSplit loaded empty applied dstType
        where
            arg = valueNoSplit argType & Lens.traversed %~ flip (,) empty
            applied = V.Apply src arg & V.BApp & V.Val (resType, empty)
    T.TSum composite | bodyNot ExprLens._BInject  ->
        suggestCaseWith composite dstType
        & Lens.traversed %~ flip (,) empty
        & (`V.Apply` src) & V.BApp & V.Val (dstType, empty)
    _ -> src
    where
        bodyNot f = Lens.nullOf (V.body . f) src

value :: Type -> [Val Type]
value typ@(T.TSum comp) =
    case comp of
    T.CVar{} -> [V.BLeaf V.LHole]
    _ -> comp ^.. ExprLens.compositeFields <&> inject
    <&> Val typ
    where
        inject (tag, innerTyp) =
            valueNoSplit innerTyp & V.Inject tag & V.BInject
value typ = [valueNoSplit typ]

valueNoSplit :: Type -> Val Type
valueNoSplit (T.TRecord composite) = suggestRecordWith composite
valueNoSplit (T.TFun (T.TSum composite) r) = suggestCaseWith composite r
valueNoSplit typ =
    case typ of
    T.TFun _ r -> valueNoSplit r & V.Lam "var" & V.BAbs
    _ -> V.BLeaf V.LHole
    & Val typ

suggestRecordWith :: T.Product -> Val Type
suggestRecordWith recordType =
    case recordType of
    T.CVar{} -> V.BLeaf V.LHole
    T.CEmpty -> V.BLeaf V.LRecEmpty
    T.CExtend f t r ->
        V.RecExtend f
        (valueNoSplit t)
        (suggestRecordWith r)
        & V.BRecExtend
    & Val (T.TRecord recordType)

suggestCaseWith :: T.Sum -> Type -> Val Type
suggestCaseWith sumType resultType =
    case sumType of
    T.CVar{} -> V.BLeaf V.LHole
    T.CEmpty -> V.BLeaf V.LAbsurd
    T.CExtend tag fieldType rest ->
        V.Case tag
        (valueNoSplit (T.TFun fieldType resultType))
        (suggestCaseWith rest resultType)
        & V.BCase
    & Val (T.TFun (T.TSum sumType) resultType)
