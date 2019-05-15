{-# LANGUAGE FlexibleContexts #-}
module Lamdu.Data.Ops
    ( newHole, applyHoleTo, setToAppliedHole
    , replace, replaceWithHole, setToHole, lambdaWrap, redexWrap
    , redexWrapWithGivenParam
    , CompositeExtendResult(..)
    , recExtend
    , case_
    , genNewTag, assocTagName
    , newPublicDefinitionWithPane
    , newPublicDefinitionToIRef
    , newPane
    , newIdentityLambda
    , setTagOrder
    ) where

import           AST.Term.Row (RowExtend(..))
import qualified Control.Lens as Lens
import           Data.Property (MkProperty', Property(..))
import qualified Data.Property as Property
import qualified Data.Set as Set
import qualified GUI.Momentu.Direction as Dir
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.CharClassification as Chars
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Data.Definition (Definition(..))
import           Lamdu.Data.Meta (SpecialArgs(..), PresentationMode)
import           Lamdu.Data.Tag (OpName(..), tagOrder, tagNames, tagOpName, getTagName)
import qualified Lamdu.Expr.GenIds as GenIds
import           Lamdu.Expr.IRef (DefI, ValP, ValI)
import qualified Lamdu.Expr.IRef as ExprIRef
import           Lamdu.I18N.LangId (LangId)
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction

import           Lamdu.Prelude

type T = Transaction

setToAppliedHole :: Monad m => ValI m -> ValP m -> T m (ValI m)
setToAppliedHole innerI destP =
    do
        newFuncI <- newHole
        resI <- ExprIRef.newValI . V.BApp $ V.Apply newFuncI innerI
        (destP ^. Property.pSet) resI
        pure resI

applyHoleTo :: Monad m => ValP m -> T m (ValI m)
applyHoleTo exprP = setToAppliedHole (exprP ^. Property.pVal) exprP

newHole :: Monad m => T m (ValI m)
newHole = ExprIRef.newValI $ V.BLeaf V.LHole

replace :: Monad m => ValP m -> ValI m -> T m (ValI m)
replace exprP newExprI = newExprI <$ Property.set exprP newExprI

replaceWithHole :: Monad m => ValP m -> T m (ValI m)
replaceWithHole exprP = replace exprP =<< newHole

setToHole :: Monad m => ValP m -> T m (ValI m)
setToHole exprP =
    exprI <$ ExprIRef.writeValI exprI hole
    where
        hole = V.BLeaf V.LHole
        exprI = Property.value exprP

lambdaWrap :: Monad m => ValP m -> T m (V.Var, ValP m)
lambdaWrap exprP =
    do
        newParam <- ExprIRef.newVar
        newExprI <-
            Property.value exprP & V.Lam newParam & V.BLam
            & ExprIRef.newValI
        Property.set exprP newExprI
            <&> (,) newParam

redexWrapWithGivenParam :: Monad m => V.Var -> ValI m -> ValP m -> T m (ValP m)
redexWrapWithGivenParam param newValueI exprP =
    do
        newLambdaI <- ExprIRef.newValI $ mkLam $ Property.value exprP
        newApplyI <- ExprIRef.newValI . V.BApp $ V.Apply newLambdaI newValueI
        (exprP ^. Property.pSet) newApplyI
        Property (Property.value exprP)
            (ExprIRef.writeValI newLambdaI . mkLam)
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
        RowExtend tag newValueI valI & V.BRecExtend & ExprIRef.newValI
            <&> CompositeExtendResult newValueI

case_ :: Monad m => T.Tag -> ValI m -> T m (CompositeExtendResult m)
case_ tag tailI =
    do
        newValueI <- newHole
        RowExtend tag newValueI tailI & V.BCase & ExprIRef.newValI
            <&> CompositeExtendResult newValueI

assocTagName ::
    (Monad m, Has LangId env, Has Dir.Layout env) =>
    env -> T.Tag -> MkProperty' (T m) Text
assocTagName env tag =
    ExprIRef.readTagInfo tag
    <&> result
    & Property.MkProperty
    where
        lang = env ^. has
        result info =
            Property (getTagName env info)
            (Transaction.writeIRef (ExprIRef.tagI tag) . setName)
            where
                setName name
                    | name == mempty =
                        info
                        & tagOpName .~ NotAnOp
                        & tagNames . Lens.at lang .~ Nothing
                    | isOperator name =
                        info
                        & tagOpName .~ OpUni name
                        & tagNames . Lens.at lang .~ Nothing
                    | otherwise =
                        info
                        & tagOpName .~ NotAnOp
                        & tagNames . Lens.at lang ?~ name
        isOperator = Lens.allOf Lens.each (`elem` Chars.operator)

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
        getVar <- V.LVar paramId & V.BLeaf & ExprIRef.newValI
        lamI <- V.Lam paramId getVar & V.BLam & ExprIRef.newValI
        pure (paramId, lamI)

setTagOrder :: Monad m => T.Tag -> Int -> T m ()
setTagOrder tag order =
    ExprIRef.readTagInfo tag
    <&> tagOrder .~ order
    >>= Transaction.writeIRef (ExprIRef.tagI tag)
