{-# LANGUAGE TypeFamilies #-}

module Lamdu.Sugar.Convert.Apply
    ( convert
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Once (Typeable)
import           Control.Monad.Trans.Except.Extended (runMatcherT, justToLeft)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Data.Map as Map
import           Data.Maybe.Extended (maybeToMPlus)
import qualified Data.Property as Property
import qualified Data.Set as Set
import           Hyper
import           Hyper.Syntax (funcIn)
import           Hyper.Syntax.Row (freExtends, freRest)
import           Hyper.Syntax.Scheme (sTyp)
import           Lamdu.Calc.Definition (Deps, depsGlobalTypes)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Sugar.Config as Config
import           Lamdu.Sugar.Convert.Expression.Actions (addActions, addActionsWith, subexprPayloads)
import           Lamdu.Sugar.Convert.Fragment (convertAppliedHole)
import           Lamdu.Sugar.Convert.GetField (convertGetFieldParam)
import           Lamdu.Sugar.Convert.IfElse (convertIfElse)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Lens (childPayloads, getVarName)
import qualified Lamdu.Sugar.PresentationModes as PresentationModes
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

convert ::
    (Monad m, Typeable m, Monoid a) =>
    V.App V.Term # Ann (Input.Payload m a) ->
    Input.Payload m a # V.Term ->
    ConvertM m (ExpressionU EvalPrep m a)
convert app@(V.App funcI argI) exprPl =
    runMatcherT $
    do
        convertGetFieldParam app exprPl & MaybeT & justToLeft
        (funcS, argS) <-
            do
                argS <- ConvertM.convertSubexpression argI & lift
                convertAppliedHole app exprPl argS & justToLeft
                funcS <- ConvertM.convertSubexpression funcI & lift
                protectedSetToVal <- lift ConvertM.typeProtectedSetToVal
                pure
                    ( if Lens.has (hVal . _BodyLeaf . _LeafHole) argS
                      then
                          let dst = argI ^. hAnn . Input.stored . ExprIRef.iref
                              deleteAction =
                                  EntityId.ofValI dst <$
                                  protectedSetToVal (exprPl ^. Input.stored) dst
                          in  funcS
                              & annotation . pActions . delete .~ SetToHole deleteAction
                      else funcS
                    , argS
                    )
        convertEmptyInject app funcS argS exprPl & justToLeft
        convertPostfix app funcS argS exprPl & justToLeft
        convertLabeled app funcS argS exprPl & justToLeft
        convertPrefix app funcS argS exprPl & lift

defParamsMatchArgs ::
    V.Var ->
    Composite v InternalName i o # Ann a ->
    Deps ->
    Bool
defParamsMatchArgs var record frozenDeps =
    do
        defArgs <-
            frozenDeps ^?
                depsGlobalTypes . Lens.at var . Lens._Just
                . _Pure . sTyp . _Pure . T._TFun . funcIn
                . _Pure . T._TRecord . T.flatRow
        defArgs ^? freRest . _Pure . T._REmpty
        let sFields =
                record ^..
                ( cList . tlItems . traverse . tiTag . tagRefTag . tagVal
                    <> cPunnedItems . traverse . pvVar . hVal . Lens._Wrapped . getVarName . inTag
                ) & Set.fromList
        guard (sFields == Map.keysSet (defArgs ^. freExtends))
    & Lens.has Lens._Just

convertEmptyInject ::
    (Monad m, Monoid a, Recursively HFoldable h) =>
    h # Ann (Input.Payload m a) ->
    ExpressionU v m a -> ExpressionU v m a -> Input.Payload m a # V.Term ->
    MaybeT (ConvertM m) (ExpressionU v m a)
convertEmptyInject subexprs funcS argS applyPl =
    do
        inject <- annValue (^? _BodyLeaf . _LeafInject . Lens._Unwrapped) funcS & maybeToMPlus
        r <- annValue (^? _BodyRecord) argS & maybeToMPlus
        r ^. hVal . cList . tlItems & null & guard
        r ^. hVal . cPunnedItems & null & guard
        Lens.has (hVal . cTail . _ClosedComposite) r & guard
        r & annValue %~ (^. cList . tlAddFirst . Lens._Unwrapped)
            & NullaryInject inject & BodyNullaryInject
            & addActions subexprs applyPl & lift

convertPostfix ::
    (Monad m, Monoid a, Recursively HFoldable h) =>
    h # Ann (Input.Payload m a) ->
    ExpressionU v m a -> ExpressionU v m a -> Input.Payload m a # V.Term ->
    MaybeT (ConvertM m) (ExpressionU v m a)
convertPostfix subexprs funcS argS applyPl =
    do
        postfixFunc <- annValue (^? _BodyPostfixFunc) funcS & maybeToMPlus
        del <- makeDel applyPl & lift
        let postfix =
                PostfixApply
                { _pArg = argS & annotation . pActions . delete . Lens.filteredBy _CannotDelete .~ del funcS
                , _pFunc = postfixFunc
                }
        setTo <- lift ConvertM.typeProtectedSetToVal ?? applyPl ^. Input.stored
        ifSugar <- Lens.view (ConvertM.scConfig . Config.sugarsEnabled . Config.ifExpression)
        guard ifSugar *> convertIfElse setTo postfix
            & maybe (BodyPostfixApply postfix) BodyIfElse
            & addActions subexprs applyPl & lift

convertLabeled ::
    (Monad m, Monoid a, Recursively HFoldable h) =>
    h # Ann (Input.Payload m a) ->
    ExpressionU v m a -> ExpressionU v m a -> Input.Payload m a # V.Term ->
    MaybeT (ConvertM m) (ExpressionU v m a)
convertLabeled subexprs funcS argS exprPl =
    do
        Lens.view (ConvertM.scConfig . Config.sugarsEnabled . Config.labeledApply) >>= guard
        -- Make sure it's a not a param, get the var
        -- Make sure it is not a "let" but a "def" (recursive or external)
        funcVar <-
            annValue
            ( ^? _BodyLeaf . _LeafGetVar . _GetBinder
                . Lens.filteredBy (bvForm . _GetDefinition)
                . Lens._Unwrapped
            ) funcS
            & maybeToMPlus
        -- Make sure the argument is a record
        record <- argS ^? hVal . _BodyRecord & maybeToMPlus
        -- that is closed
        Lens.has (cTail . _ClosedComposite) record & guard
        -- with at least 2 fields
        length (record ^. cList . tlItems) + length (record ^. cPunnedItems) >= 2 & guard
        frozenDeps <- Lens.view ConvertM.scFrozenDeps <&> Property.value
        let var = funcVar ^. hVal . Lens._Wrapped . bvVar
        -- If it is an external (non-recursive) def (i.e: not in
        -- scope), make sure the def (frozen) type is inferred to have
        -- closed record of same parameters
        recursiveRef <- Lens.view (ConvertM.scScopeInfo . ConvertM.siRecursiveRef)
        Just var == (recursiveRef <&> (^. ConvertM.rrDefI) <&> ExprIRef.globalId)
            || defParamsMatchArgs var record frozenDeps & guard
        let getArg field =
                AnnotatedArg
                    { _aaTag = field ^. tiTag . tagRefTag
                    , _aaExpr = field ^. tiValue
                    }
        bod <-
            PresentationModes.makeLabeledApply
            funcVar (record ^. cList . tlItems <&> getArg) (record ^. cPunnedItems) exprPl
            <&> BodyLabeledApply & lift
        let userPayload =
                subexprPayloads subexprs (bod ^.. childPayloads)
                & mconcat
        addActionsWith userPayload exprPl bod & lift

convertPrefix ::
    (Monad m, Monoid a) =>
    V.App V.Term # Ann (Input.Payload m a) ->
    ExpressionU v m a -> ExpressionU v m a -> Input.Payload m a # V.Term ->
    ConvertM m (ExpressionU v m a)
convertPrefix subexprs funcS argS applyPl =
    do
        del <- makeDel applyPl
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        let injectToNullary x
                | Lens.has (hVal . _BodyLeaf . _LeafInject) funcS =
                    do
                        ExprIRef.writeValI argIref (V.BLeaf V.LRecEmpty)
                        protectedSetToVal (applyPl ^. Input.stored) (applyPl ^. Input.stored . ExprIRef.iref)
                    <&> EntityId.ofValI
                | otherwise = x
                where
                    argIref = subexprs ^. V.appArg . hAnn . Input.stored . ExprIRef.iref
        BodySimpleApply App
            { _appFunc = funcS & annotation . pActions . delete .~ del argS
            , _appArg =
                argS
                & annotation . pActions . delete %~ (_Delete %~ injectToNullary) . (Lens.filteredBy _CannotDelete .~ del funcS)
            } & addActions subexprs applyPl

makeDel ::
    Monad m =>
    Input.Payload m a # V.Term ->
    ConvertM m ((Annotated (ConvertPayload m a2) # h) -> Delete (Transaction m))
makeDel applyPl =
    ConvertM.typeProtectedSetToVal <&>
    \protectedSetToVal remain ->
    protectedSetToVal (applyPl ^. Input.stored)
    (remain ^. annotation . pInput . Input.stored . ExprIRef.iref)
    <&> EntityId.ofValI
    & Delete
