{-# LANGUAGE TypeFamilies, TypeApplications #-}

module Lamdu.Sugar.Convert.Apply
    ( convert
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Once (OnceT, Typeable)
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
import           Lamdu.Sugar.Convert.Binder.Inline (inlineVar)
import           Lamdu.Sugar.Convert.Binder.Float (makeFloatLetToOuterScope)
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
import           Lamdu.Sugar.Convert.Fragment (convertAppliedHole)
import           Lamdu.Sugar.Convert.GetField (convertGetFieldParam)
import           Lamdu.Sugar.Convert.IfElse (convertIfElse)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Lens (taggedListItems)
import qualified Lamdu.Sugar.PresentationModes as PresentationModes
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

convert ::
    (Monad m, Typeable m) =>
    V.App V.Term # Ann (Input.Payload m) ->
    Input.Payload m # V.Term ->
    ConvertM m (ExpressionU EvalPrep m)
convert app exprPl =
    runMatcherT $
    do
        convertGetFieldParam app >>= Lens._Just addAct & MaybeT & justToLeft
        convertAppliedHole app exprPl & justToLeft
        appS <- convertSimpleApply app exprPl & lift
        convertEmptyInject appS >>= lift . addAct & justToLeft
        convertPostfix appS exprPl >>= lift . addAct & justToLeft
        convertLabeled appS exprPl >>= lift . addAct & justToLeft
        convertPrefix appS exprPl >>= addAct & lift
    where
        addAct = addActions expr
        expr = Ann exprPl (V.BApp app)

convertSimpleApply ::
    Monad m =>
    App V.Term # Ann (Input.Payload m) -> Input.Payload m # V.Term ->
    ConvertM m (AppS EvalPrep m)
convertSimpleApply app@(V.App funcI argI) exprPl =
    do
        mFloat <-
            case funcI ^. hVal of
            V.BLam lam ->
                makeFloatLetToOuterScope (exprPl ^. Input.stored . ExprIRef.setIref)
                (lam & hmap (Proxy @(Recursively HFunctor) #> hflipped %~ hmap (const (^. Input.stored))))
                argI
                <&> Just
            _ -> pure Nothing
        argS <-
            ConvertM.convertSubexpression argI <&>
            case mFloat of
            Nothing -> id
            Just float -> annotation . pActions . extract .~ float
        let scopeUpdates =
                ( case inlineVar expr of
                    Nothing -> id
                    Just (v, mkInline) -> ConvertM.siLetItems . Lens.at v ?~ mkInline
                ) .
                case mFloat of
                Nothing -> id
                Just{} -> (ConvertM.siExtractPos ?~ pos) . (ConvertM.siFloatPos ?~ pos)
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        let dst = argI ^. hAnn . Input.stored . ExprIRef.iref
        let fixDel d
                | Lens.has (hVal . _BodyLeaf . _LeafHole) argS =
                    EntityId.ofValI dst <$ protectedSetToVal (exprPl ^. Input.stored) dst
                    & SetToHole
                | otherwise = d
        let singleApply =  hVal . _BodyLam . lamApplyLimit .~ AtMostOneFuncApply
        let onEachAppliedAlt x =
                x
                & cList . Lens.mapped %~ singleApply
                & cTail . _OpenCompositeTail %~ singleApply
        funcS <-
            ConvertM.convertSubexpression funcI
            & local (ConvertM.scScopeInfo %~ scopeUpdates)
            <&> annotation . pActions . delete %~ fixDel
            <&> hVal . _BodyLam . lamApplyLimit .~ AtMostOneFuncApply
            <&> hVal . _BodyPostfixFunc . _PfCase %~ onEachAppliedAlt
        App funcS argS & pure
    where
        expr = Ann exprPl (V.BApp app)
        pos =
            ConvertM.OuterScopeInfo
            { ConvertM._osiPos = exprPl ^. Input.stored
            , ConvertM._osiScope = exprPl ^. Input.inferScope
            }

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
                ( cList . taggedListItems . tiTag . tagRefTag . tagVal
                    <> cPunnedItems . traverse . pvVar . hVal . Lens._Wrapped . vNameRef . nrName . inTag
                ) & Set.fromList
        guard (sFields == Map.keysSet (defArgs ^. freExtends))
    & Lens.has Lens._Just

type AppS v m =
    App (Term v InternalName (OnceT (T m)) (T m)) #
    Annotated (ConvertPayload m)

convertEmptyInject :: Monad m => AppS v m -> MaybeT (ConvertM m) (BodyU v m)
convertEmptyInject (App funcS argS) =
    do
        Lens.view (ConvertM.scSugars . Config.nullaryInject) >>= guard
        inject <- annValue (^? _BodyLeaf . _LeafInject . Lens._Unwrapped) funcS & maybeToMPlus
        r <- annValue (^? _BodyRecord) argS & maybeToMPlus
        r ^. hVal . cList . tlItems & null & guard
        r ^. hVal . cPunnedItems & null & guard
        Lens.has (hVal . cTail . _ClosedCompositeTail) r & guard
        r & annValue %~ (^. cList . tlAddFirst . Lens._Unwrapped)
            & NullaryInject inject & BodyNullaryInject
            & pure

convertPostfix :: Monad m => AppS v m -> Input.Payload m # V.Term -> MaybeT (ConvertM m) (BodyU v m)
convertPostfix (App funcS argS) applyPl =
    do
        postfixFunc <- annValue (^? _BodyPostfixFunc) funcS & maybeToMPlus
        del <- makeDel applyPl & lift
        let postfix =
                PostfixApply
                { _pArg = argS & annotation . pActions . delete . Lens.filteredBy _CannotDelete .~ del funcS
                , _pFunc = postfixFunc
                }
        setTo <- lift ConvertM.typeProtectedSetToVal ?? applyPl ^. Input.stored
        ifSugar <- Lens.view (ConvertM.scSugars . Config.ifExpression)
        guard ifSugar *> convertIfElse setTo postfix
            & maybe (BodyPostfixApply postfix) BodyIfElse
            & pure

convertLabeled ::
    Monad m =>
    AppS v m -> Input.Payload m # V.Term ->
    MaybeT (ConvertM m) (BodyU v m)
convertLabeled (App funcS argS) exprPl =
    do
        Lens.view (ConvertM.scSugars . Config.labeledApply) >>= guard
        -- Make sure it's a not a param, get the var
        -- Make sure it is not a "let" but a "def" (recursive or external)
        funcVar <-
            annValue
            ( ^? _BodyLeaf . _LeafGetVar
                . Lens.filteredBy (vForm . _GetDefinition)
                . Lens._Unwrapped
            ) funcS
            & maybeToMPlus
        -- Make sure the argument is a record
        record <- argS ^? hVal . _BodyRecord & maybeToMPlus
        -- that is closed
        Lens.has (cTail . _ClosedCompositeTail) record & guard
        -- with at least 2 fields
        length (record ^.. cList . taggedListItems) + length (record ^. cPunnedItems) >= 2 & guard
        frozenDeps <- Lens.view ConvertM.scFrozenDeps <&> Property.value
        let var = funcVar ^. hVal . Lens._Wrapped . vVar
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
        PresentationModes.makeLabeledApply
            funcVar
            (record ^.. cList . taggedListItems <&> getArg)
            (record ^. cPunnedItems) exprPl
            & lift
    <&> BodyLabeledApply

convertPrefix :: Monad m => AppS v m -> Input.Payload m # V.Term -> ConvertM m (BodyU v m)
convertPrefix (App funcS argS) applyPl =
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
                    argIref = argS ^. annotation . pStored . ExprIRef.iref
        BodySimpleApply App
            { _appFunc = funcS & annotation . pActions . delete .~ del argS
            , _appArg =
                argS
                & annotation . pActions . delete %~ (_Delete %~ injectToNullary) . (Lens.filteredBy _CannotDelete .~ del funcS)
            } & pure

makeDel ::
    Monad m =>
    Input.Payload m # V.Term ->
    ConvertM m ((Annotated (ConvertPayload m) # h) -> Delete (T m))
makeDel applyPl =
    ConvertM.typeProtectedSetToVal <&>
    \protectedSetToVal remain ->
    protectedSetToVal (applyPl ^. Input.stored)
    (remain ^. annotation . pStored . ExprIRef.iref)
    <&> EntityId.ofValI
    & Delete
