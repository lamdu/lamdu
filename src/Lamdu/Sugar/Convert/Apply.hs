{-# LANGUAGE NoImplicitPrelude, NamedFieldPuns, OverloadedStrings #-}
module Lamdu.Sugar.Convert.Apply
    ( convert
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Trans.Either.Utils (runMatcherT, justToLeft)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Data.Map as Map
import           Data.Maybe.Utils (maybeToMPlus)
import qualified Data.Set as Set
import qualified Data.Store.Property as Property
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Calc.Type.FlatComposite as FlatComposite
import           Lamdu.Calc.Type.Scheme (schemeType)
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val)
import qualified Lamdu.Calc.Val.Annotated as Val
import qualified Lamdu.Expr.UniqueId as UniqueId
import qualified Lamdu.Infer as Infer
import           Lamdu.Sugar.Convert.Case (convertAppliedCase)
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
import           Lamdu.Sugar.Convert.Hole.Wrapper (convertAppliedHole)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

convert ::
    (Monad m, Monoid a) => V.Apply (Val (Input.Payload m a)) ->
    Input.Payload m a -> ConvertM m (ExpressionU m a)
convert app@(V.Apply funcI argI) exprPl =
    runMatcherT $
    do
        (funcS, argS) <-
            do
                argS <- lift $ ConvertM.convertSubexpression argI
                justToLeft $ convertAppliedHole app argS exprPl
                funcS <- ConvertM.convertSubexpression funcI & lift
                protectedSetToVal <- lift ConvertM.typeProtectedSetToVal
                return
                    ( do
                        Lens.has (rBody . _BodyHole) argS & guard
                        let dst = argI ^. Val.payload . Input.stored . Property.pVal
                        let deleteAction =
                                (UniqueId.toUUID dst, EntityId.ofValI dst) <$
                                protectedSetToVal
                                (exprPl ^. Input.stored)
                                dst
                        funcS
                            & rPayload . plActions . setToHole .~ SetToHole deleteAction
                            & return
                        & fromMaybe funcS
                    , argS
                    )
        justToLeft $ convertAppliedCase funcS argS exprPl
        justToLeft $ convertLabeled funcS argS exprPl
        lift $ convertPrefix funcS argS exprPl

noRepetitions :: Ord a => [a] -> Bool
noRepetitions x = length x == Set.size (Set.fromList x)

convertLabeled ::
    (Monad m, Monoid a) =>
    ExpressionU m a -> ExpressionU m a -> Input.Payload m a ->
    MaybeT (ConvertM m) (ExpressionU m a)
convertLabeled funcS argS exprPl =
    do
        sBinderVar <-
            funcS ^? rBody . _BodyGetVar . _GetBinder & maybeToMPlus
        record <- argS ^? rBody . _BodyRecord & maybeToMPlus
        Lens.has (rTail . _ClosedRecord) record & guard
        guard $ length (record ^. rItems) >= 2
        ctx <- lift ConvertM.readContext
        let var = sBinderVar ^. bvNameRef . nrName & UniqueId.identifierOfUUID & V.Var
        unless (Lens.has (Lens.at var . Lens._Just)
            (Infer.scopeToTypeMap (exprPl ^. Input.inferred . Infer.plScope))) $
            do
                defArgs <-
                    ctx ^? ConvertM.scFrozenDeps . Property.pVal
                        . Infer.depsGlobalTypes . Lens.at var . Lens._Just
                        . schemeType . T._TFun . _1 . T._TRecord
                    & maybeToMPlus
                let flatArgs = FlatComposite.fromComposite defArgs
                flatArgs ^? FlatComposite.extension . Lens._Nothing & maybeToMPlus
                let sFields =
                        record ^.. rItems . traverse . ciTag . tagInfo . tagVal & Set.fromList
                guard $ Map.keysSet (flatArgs ^. FlatComposite.fields) == sFields
        let getArg field =
                AnnotatedArg
                    { _aaTag = field ^. ciTag . tagInfo
                    , _aaName = field ^. ciTag . tagName
                    , _aaExpr = field ^. ciExpr
                    }
        let args = map getArg $ record ^. rItems
        let tags = args ^.. Lens.traversed . aaTag . tagVal
        unless (noRepetitions tags) $ error "Repetitions should not type-check"
        BodyLabeledApply LabeledApply
            { _aFunc = sBinderVar
            , _aSpecialArgs = NoSpecialArgs
            , _aAnnotatedArgs = args
            , _aRelayedArgs =
                -- Hidden args must be determined along with the special args.
                -- One never wants to hide an infix operator's args.
                []
            }
            & lift . addActions exprPl

convertPrefix ::
    Monad m =>
    ExpressionU m a -> ExpressionU m a -> Input.Payload m a ->
    ConvertM m (ExpressionU m a)
convertPrefix funcS argS applyPl =
    do
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        let setToFunc =
                protectedSetToVal (applyPl ^. Input.stored)
                (funcS ^. rPayload . plData . pStored & Property.value)
                <&> EntityId.ofValI
        BodySimpleApply Apply
            { _applyFunc = funcS
            , _applyArg =
                argS & rBody . _BodyHole . holeActions . holeMDelete .~ Just setToFunc
            }
            & addActions applyPl
