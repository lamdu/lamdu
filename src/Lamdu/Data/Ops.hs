{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, LambdaCase #-}
module Lamdu.Data.Ops
    ( newHole, applyHoleTo, setToAppliedHole
    , replace, replaceWithHole, setToHole, lambdaWrap, redexWrap
    , redexWrapWithGivenParam
    , CompositeExtendResult(..)
    , recExtend
    , case_
    , genNewTag
    , newPublicDefinitionWithPane
    , newPublicDefinitionToIRef
    , savePreJumpPosition, jumpBack
    , newPane
    , newIdentityLambda
    ) where

import qualified Data.Set as Set
import qualified GUI.Momentu.Widget.Id as WidgetId
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Calc.Val as V
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Data.Definition (Definition(..))
import           Lamdu.Data.Meta (SpecialArgs(..), PresentationMode)
import qualified Lamdu.Expr.GenIds as GenIds
import           Lamdu.Expr.IRef (DefI, ValIProperty, ValI)
import qualified Lamdu.Expr.IRef as ExprIRef
import           Revision.Deltum.Property (Property(..))
import qualified Revision.Deltum.Property as Property
import           Revision.Deltum.Transaction (Transaction, getP, setP, modP)
import qualified Revision.Deltum.Transaction as Transaction
import qualified System.Random.Utils as RandomUtils

import           Lamdu.Prelude

type T = Transaction

setToAppliedHole :: Monad m => ValI m -> ValIProperty m -> T m (ValI m)
setToAppliedHole innerI destP =
    do
        newFuncI <- newHole
        resI <- ExprIRef.newValBody . V.BApp $ V.Apply newFuncI innerI
        Property.set destP resI
        pure resI

applyHoleTo :: Monad m => ValIProperty m -> T m (ValI m)
applyHoleTo exprP =
    do
        newFuncI <- newHole
        applyI <- ExprIRef.newValBody . V.BApp . V.Apply newFuncI $ Property.value exprP
        Property.set exprP applyI
        pure applyI

newHole :: Monad m => T m (ValI m)
newHole = ExprIRef.newValBody $ V.BLeaf V.LHole

replace :: Monad m => ValIProperty m -> ValI m -> T m (ValI m)
replace exprP newExprI = newExprI <$ Property.set exprP newExprI

replaceWithHole :: Monad m => ValIProperty m -> T m (ValI m)
replaceWithHole exprP = replace exprP =<< newHole

setToHole :: Monad m => ValIProperty m -> T m (ValI m)
setToHole exprP =
    exprI <$ ExprIRef.writeValBody exprI hole
    where
        hole = V.BLeaf V.LHole
        exprI = Property.value exprP

lambdaWrap :: Monad m => ValIProperty m -> T m (V.Var, ValI m)
lambdaWrap exprP =
    do
        newParam <- ExprIRef.newVar
        newExprI <-
            Property.value exprP & V.Lam newParam & V.BLam
            & ExprIRef.newValBody
        Property.set exprP newExprI
        pure (newParam, newExprI)

redexWrapWithGivenParam :: Monad m => V.Var -> ValI m -> ValIProperty m -> T m (ValIProperty m)
redexWrapWithGivenParam param newValueI exprP =
    do
        newLambdaI <- ExprIRef.newValBody $ mkLam $ Property.value exprP
        newApplyI <- ExprIRef.newValBody . V.BApp $ V.Apply newLambdaI newValueI
        Property.set exprP newApplyI
        Property (Property.value exprP)
            (ExprIRef.writeValBody newLambdaI . mkLam)
            & pure
    where
        mkLam = V.BLam . V.Lam param

redexWrap :: Monad m => ValIProperty m -> T m V.Var
redexWrap exprP =
    do
        newValueI <- newHole
        newParam <- ExprIRef.newVar
        _ <- redexWrapWithGivenParam newParam newValueI exprP
        pure newParam

data CompositeExtendResult m = CompositeExtendResult
    { cerNewVal :: ValI m
    , cerResult :: ValI m
    }

genNewTag :: Monad m => T m T.Tag
genNewTag = Transaction.newKey <&> fst . GenIds.randomTag . RandomUtils.genFromHashable

recExtend :: Monad m => T.Tag -> ValI m -> T m (CompositeExtendResult m)
recExtend tag valI =
    do
        newValueI <- newHole
        V.RecExtend tag newValueI valI & V.BRecExtend & ExprIRef.newValBody
            <&> CompositeExtendResult newValueI

case_ :: Monad m => T.Tag -> ValI m -> T m (CompositeExtendResult m)
case_ tag tailI =
    do
        newValueI <- newHole
        V.Case tag newValueI tailI & V.BCase & ExprIRef.newValBody
            <&> CompositeExtendResult newValueI

newPane :: Monad m => Anchors.CodeAnchors m -> DefI m -> T m ()
newPane codeAnchors defI =
    do
        let panesProp = Anchors.panes codeAnchors
        panes <- getP panesProp
        when (defI `notElem` map Anchors.paneDef panes) $
            setP panesProp $ panes ++ [Anchors.Pane defI]

savePreJumpPosition :: Monad m => Anchors.CodeAnchors m -> WidgetId.Id -> T m ()
savePreJumpPosition codeAnchors pos = modP (Anchors.preJumps codeAnchors) $ (pos :) . take 19

jumpBack :: Monad m => Anchors.CodeAnchors m -> T m (Maybe (T m WidgetId.Id))
jumpBack codeAnchors =
    getP (Anchors.preJumps codeAnchors)
    <&> \case
    [] -> Nothing
    (j:js) -> j <$ setP (Anchors.preJumps codeAnchors) js & Just

newDefinition ::
    Monad m => Text -> PresentationMode -> Definition (ValI m) () -> T m (DefI m)
newDefinition name presentationMode def =
    do
        newDef <- Transaction.newIRef def
        setP (Anchors.assocNameRef newDef) name
        let defVar = ExprIRef.globalId newDef
        setP (Anchors.assocPresentationMode defVar) presentationMode
        pure newDef

-- Used when writing a definition into an identifier which was a variable.
-- Used in float.
newPublicDefinitionToIRef ::
    Monad m => Anchors.CodeAnchors m -> Definition (ValI m) () -> DefI m -> T m ()
newPublicDefinitionToIRef codeAnchors def defI =
    do
        Transaction.writeIRef defI def
        modP (Anchors.globals codeAnchors) (Set.insert defI)
        newPane codeAnchors defI

newPublicDefinitionWithPane ::
    Monad m =>
    Anchors.CodeAnchors m -> Definition (ValI m) () -> T m (DefI m)
newPublicDefinitionWithPane codeAnchors def =
    do
        defI <- newDefinition "" Verbose def
        modP (Anchors.globals codeAnchors) (Set.insert defI)
        newPane codeAnchors defI
        pure defI

newIdentityLambda :: Monad m => T m (V.Var, ValI m)
newIdentityLambda =
    do
        paramId <- ExprIRef.newVar
        getVar <- V.LVar paramId & V.BLeaf & ExprIRef.newValBody
        lamI <- V.Lam paramId getVar & V.BLam & ExprIRef.newValBody
        pure (paramId, lamI)
