module Lamdu.Data.Ops
    ( newHole, applyHoleTo, setToAppliedHole
    , replace, replaceWithHole, setToHole, lambdaWrap, redexWrap
    , redexWrapWithGivenParam
    , CompositeExtendResult(..)
    , recExtend
    , case_
    , genNewTag, assocPublishedTagName
    , newPublicDefinitionWithPane
    , newPublicDefinitionToIRef
    , newPane
    , newIdentityLambda
    ) where

import qualified Control.Lens as Lens
import           Data.Property (MkProperty', Property(..))
import qualified Data.Property as Property
import qualified Data.Set as Set
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Data.Definition (Definition(..))
import           Lamdu.Data.Meta (SpecialArgs(..), PresentationMode)
import qualified Lamdu.Expr.GenIds as GenIds
import           Lamdu.Expr.IRef (DefI, ValP, ValI)
import qualified Lamdu.Expr.IRef as ExprIRef
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction

import           Lamdu.Prelude

type T = Transaction

setToAppliedHole :: Monad m => ValI m -> ValP m -> T m (ValI m)
setToAppliedHole innerI destP =
    do
        newFuncI <- newHole
        resI <- Transaction.newIRef . V.BApp $ V.Apply newFuncI innerI
        (destP ^. Property.pSet) resI
        pure resI

applyHoleTo :: Monad m => ValP m -> T m (ValI m)
applyHoleTo exprP = setToAppliedHole (exprP ^. Property.pVal) exprP

newHole :: Monad m => T m (ValI m)
newHole = Transaction.newIRef $ V.BLeaf V.LHole

replace :: Monad m => ValP m -> ValI m -> T m (ValI m)
replace exprP newExprI = newExprI <$ Property.set exprP newExprI

replaceWithHole :: Monad m => ValP m -> T m (ValI m)
replaceWithHole exprP = replace exprP =<< newHole

setToHole :: Monad m => ValP m -> T m (ValI m)
setToHole exprP =
    exprI <$ Transaction.writeIRef exprI hole
    where
        hole = V.BLeaf V.LHole
        exprI = Property.value exprP

lambdaWrap :: Monad m => ValP m -> T m (V.Var, ValP m)
lambdaWrap exprP =
    do
        newParam <- ExprIRef.newVar
        newExprI <-
            Property.value exprP & V.Lam newParam & V.BLam
            & Transaction.newIRef
        Property.set exprP newExprI
            <&> (,) newParam

redexWrapWithGivenParam :: Monad m => V.Var -> ValI m -> ValP m -> T m (ValP m)
redexWrapWithGivenParam param newValueI exprP =
    do
        newLambdaI <- Transaction.newIRef $ mkLam $ Property.value exprP
        newApplyI <- Transaction.newIRef . V.BApp $ V.Apply newLambdaI newValueI
        (exprP ^. Property.pSet) newApplyI
        Property (Property.value exprP)
            (Transaction.writeIRef newLambdaI . mkLam)
            & pure
    where
        mkLam = V.BLam . V.Lam param

redexWrap :: Monad m => ValP m -> T m (ValI m)
redexWrap exprP =
    do
        newValueI <- newHole
        newParam <- ExprIRef.newVar
        _ <- redexWrapWithGivenParam newParam newValueI exprP
        pure newValueI

data CompositeExtendResult m = CompositeExtendResult
    { cerNewVal :: ValI m
    , cerResult :: ValI m
    }

genNewTag :: Monad m => T m T.Tag
genNewTag = GenIds.transaction GenIds.randomTag

recExtend :: Monad m => T.Tag -> ValI m -> T m (CompositeExtendResult m)
recExtend tag valI =
    do
        newValueI <- newHole
        V.RecExtend tag newValueI valI & V.BRecExtend & Transaction.newIRef
            <&> CompositeExtendResult newValueI

case_ :: Monad m => T.Tag -> ValI m -> T m (CompositeExtendResult m)
case_ tag tailI =
    do
        newValueI <- newHole
        V.Case tag newValueI tailI & V.BCase & Transaction.newIRef
            <&> CompositeExtendResult newValueI

-- | assocTagNameRef that publishes/unpublishes upon setting a
-- non-empty/empty name
assocPublishedTagName ::
    Monad m => MkProperty' (T m) (Set T.Tag) -> T.Tag -> MkProperty' (T m) Text
assocPublishedTagName publishedTagsProp tag =
    Anchors.assocTagNameRef tag
    & Property.prop . Property.pSet .>
    Lens.imapped %@~ \i -> (<* publish i)
    where
        publish newName =
            Property.modP publishedTagsProp (Lens.contains tag .~ (newName /= ""))

newPane :: Monad m => Anchors.CodeAnchors m -> DefI m -> T m ()
newPane codeAnchors defI =
    do
        let panesProp = Anchors.panes codeAnchors
        panes <- Property.getP panesProp
        when (defI `notElem` map Anchors.paneDef panes) $
            Property.setP panesProp $ panes ++ [Anchors.Pane defI]

newDefinition :: Monad m => PresentationMode -> Definition (ValI m) () -> T m (DefI m)
newDefinition presentationMode def =
    do
        newDef <- Transaction.newIRef def
        let defVar = ExprIRef.globalId newDef
        Property.setP (Anchors.assocPresentationMode defVar) presentationMode
        pure newDef

-- Used when writing a definition into an identifier which was a variable.
-- Used in float.
newPublicDefinitionToIRef ::
    Monad m => Anchors.CodeAnchors m -> Definition (ValI m) () -> DefI m -> T m ()
newPublicDefinitionToIRef codeAnchors def defI =
    do
        Transaction.writeIRef defI def
        Property.modP (Anchors.globals codeAnchors) (Set.insert defI)
        newPane codeAnchors defI

newPublicDefinitionWithPane ::
    Monad m =>
    Anchors.CodeAnchors m -> Definition (ValI m) () -> T m (DefI m)
newPublicDefinitionWithPane codeAnchors def =
    do
        defI <- newDefinition Verbose def
        Property.modP (Anchors.globals codeAnchors) (Set.insert defI)
        newPane codeAnchors defI
        pure defI

newIdentityLambda :: Monad m => T m (V.Var, ValI m)
newIdentityLambda =
    do
        paramId <- ExprIRef.newVar
        getVar <- V.LVar paramId & V.BLeaf & Transaction.newIRef
        lamI <- V.Lam paramId getVar & V.BLam & Transaction.newIRef
        pure (paramId, lamI)
