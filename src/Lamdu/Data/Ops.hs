{-# LANGUAGE TypeFamilies #-}
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

import qualified Control.Lens as Lens
import qualified Data.ByteString.Extended as BS
import           Data.Property (MkProperty', Property(..))
import qualified Data.Property as Property
import qualified Data.Set as Set
import qualified Data.UUID as UUID
import qualified GUI.Momentu.Direction as Dir
import           Hyper (_HCompose)
import           Hyper.Type.AST.Row (RowExtend(..))
import           Hyper.Type.Prune (Prune(..))
import           Lamdu.Calc.Identifier (Identifier(..))
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.CharClassification as Chars
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Data.Definition (Definition(..))
import           Lamdu.Data.Meta (SpecialArgs(..), PresentationMode)
import           Lamdu.Data.Tag (Symbol(..), TextsInLang(..), tagOrder, tagTexts, tagSymbol, getTagName, name)
import           Lamdu.Expr.IRef (DefI, HRef, ValI)
import qualified Lamdu.Expr.IRef as ExprIRef
import           Lamdu.I18N.LangId (LangId)
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction

import           Lamdu.Prelude

type T = Transaction

setToAppliedHole :: Monad m => ValI m -> HRef m # V.Term -> T m (ValI m)
setToAppliedHole innerI destP =
    do
        newFuncI <- newHole
        resI <- ExprIRef.newValI . V.BApp $ V.App newFuncI innerI
        (destP ^. ExprIRef.setIref) resI
        pure resI

applyHoleTo :: Monad m => HRef m # V.Term -> T m (ValI m)
applyHoleTo exprP = setToAppliedHole (exprP ^. ExprIRef.iref) exprP

newHole :: Monad m => T m (ValI m)
newHole = ExprIRef.newValI $ V.BLeaf V.LHole

replace :: Monad m => HRef m # V.Term -> ValI m -> T m (ValI m)
replace exprP newExprI = newExprI <$ (exprP ^. ExprIRef.setIref) newExprI

replaceWithHole :: Monad m => HRef m # V.Term -> T m (ValI m)
replaceWithHole exprP = replace exprP =<< newHole

setToHole :: Monad m => HRef m # V.Term -> T m (ValI m)
setToHole exprP =
    exprI <$ ExprIRef.writeValI exprI hole
    where
        hole = V.BLeaf V.LHole
        exprI = exprP ^. ExprIRef.iref

lambdaWrap :: Monad m => HRef m # V.Term -> T m (V.Var, HRef m # V.Term)
lambdaWrap exprP =
    do
        newParam <- ExprIRef.newVar
        t <- _HCompose # Pruned & ExprIRef.newValI
        newExprI <-
            exprP ^. ExprIRef.iref & V.TypedLam newParam t & V.BLam
            & ExprIRef.newValI
        (newParam, exprP & ExprIRef.iref .~ newExprI) <$
            (exprP ^. ExprIRef.setIref) newExprI

redexWrapWithGivenParam ::
    Monad m =>
    V.Var -> ValI m -> HRef m # V.Term -> T m (HRef m # V.Term)
redexWrapWithGivenParam param newValueI exprP =
    do
        newLambdaI <- exprP ^. ExprIRef.iref & mkLam >>= ExprIRef.newValI
        newApplyI <- ExprIRef.newValI . V.BApp $ V.App newLambdaI newValueI
        let newSet v = mkLam v >>= ExprIRef.writeValI newLambdaI
        (exprP & ExprIRef.setIref .~ newSet) <$ (exprP ^. ExprIRef.setIref) newApplyI
    where
        mkLam b =
            _HCompose # Pruned & ExprIRef.newValI
            <&> (V.TypedLam param ?? b) <&> V.BLam

redexWrap :: Monad m => HRef m # V.Term -> T m (ValI m)
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
genNewTag = Transaction.newKey <&> T.Tag . Identifier . BS.strictify . UUID.toByteString

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
    (Monad f, MonadReader env m, Has LangId env, Has Dir.Layout env) =>
    m (T.Tag -> MkProperty' (T f) Text)
assocTagName =
    Lens.view id <&>
    \env tag ->
    let lang = env ^. has
        result info =
            Property (getTagName env info ^. _2 . name)
            (Transaction.writeIRef (ExprIRef.tagI tag) . setName)
            where
                setName x
                    | x == mempty =
                        info
                        & tagSymbol .~ NoSymbol
                        & tagTexts . Lens.at lang .~ Nothing
                    | isOperator x =
                        info
                        & tagSymbol .~ UniversalSymbol x
                        & tagTexts . Lens.at lang .~ Nothing
                    | otherwise =
                        info
                        & tagSymbol .~ NoSymbol
                        & tagTexts . Lens.at lang ?~ TextsInLang x Nothing Nothing
        isOperator = Lens.allOf Lens.each (`elem` Chars.operator)
    in  ExprIRef.readTagData tag
        <&> result
        & Property.MkProperty

newPane :: Monad m => Anchors.CodeAnchors m -> Anchors.Pane m -> T m ()
newPane codeAnchors pane =
    do
        Property panes setPanes <-
            Anchors.panes codeAnchors ^. Property.mkProperty
        panes ++ [pane] & setPanes & when (pane `notElem` panes)

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
        newPane codeAnchors (Anchors.PaneDefinition defI)

newPublicDefinitionWithPane ::
    Monad m =>
    Anchors.CodeAnchors m -> Definition (ValI m) () -> T m (DefI m)
newPublicDefinitionWithPane codeAnchors def =
    do
        defI <- newDefinition Verbose def
        Property.modP (Anchors.globals codeAnchors) (Set.insert defI)
        newPane codeAnchors (Anchors.PaneDefinition defI)
        pure defI

newIdentityLambda :: Monad m => T m (V.Var, ValI m)
newIdentityLambda =
    do
        paramId <- ExprIRef.newVar
        getVar <- V.LVar paramId & V.BLeaf & ExprIRef.newValI
        paramType <- _HCompose # Pruned & ExprIRef.newValI
        lamI <- V.TypedLam paramId paramType getVar & V.BLam & ExprIRef.newValI
        pure (paramId, lamI)

setTagOrder :: Monad m => T.Tag -> Int -> T m ()
setTagOrder tag order =
    ExprIRef.readTagData tag
    <&> tagOrder .~ order
    >>= Transaction.writeIRef (ExprIRef.tagI tag)
