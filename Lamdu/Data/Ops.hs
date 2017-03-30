{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Data.Ops
    ( newHole, wrap, setToWrapper
    , replace, replaceWithHole, setToHole, lambdaWrap, redexWrap
    , redexWrapWithGivenParam
    , recExtend, RecExtendResult(..)
    , case_, CaseResult(..)
    , newPublicDefinitionWithPane
    , newPublicDefinitionToIRef
    , savePreJumpPosition, jumpBack
    , newPane
    , isInfix
    , newIdentityLambda
    ) where

import qualified Data.Set as Set
import           Data.Store.Property (Property(..))
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction, getP, setP, modP)
import qualified Data.Store.Transaction as Transaction
import qualified Data.Text as Text
import qualified Graphics.UI.Bottle.WidgetId as WidgetId
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Calc.Val as V
import           Lamdu.CharClassification (operatorChars)
import           Lamdu.Data.Anchors (PresentationMode(..))
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Data.Definition (Definition(..))
import qualified Lamdu.Expr.GenIds as GenIds
import           Lamdu.Expr.IRef (DefI, ValIProperty, ValI)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified System.Random.Utils as RandomUtils

import           Lamdu.Prelude

type T = Transaction

setToWrapper :: Monad m => ValI m -> ValIProperty m -> T m (ValI m)
setToWrapper wrappedI destP =
    do
        newFuncI <- newHole
        resI <- ExprIRef.newValBody . V.BApp $ V.Apply newFuncI wrappedI
        Property.set destP resI
        return resI

wrap :: Monad m => ValIProperty m -> T m (ValI m)
wrap exprP =
    do
        newFuncI <- newHole
        applyI <- ExprIRef.newValBody . V.BApp . V.Apply newFuncI $ Property.value exprP
        Property.set exprP applyI
        return applyI

newHole :: Monad m => T m (ValI m)
newHole = ExprIRef.newValBody $ V.BLeaf V.LHole

replace :: Monad m => ValIProperty m -> ValI m -> T m (ValI m)
replace exprP newExprI =
    do
        Property.set exprP newExprI
        return newExprI

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
        return (newParam, newExprI)

redexWrapWithGivenParam :: Monad m => V.Var -> ValI m -> ValIProperty m -> T m (ValIProperty m)
redexWrapWithGivenParam param newValueI exprP =
    do
        newLambdaI <- ExprIRef.newValBody $ mkLam $ Property.value exprP
        newApplyI <- ExprIRef.newValBody . V.BApp $ V.Apply newLambdaI newValueI
        Property.set exprP newApplyI
        Property (Property.value exprP)
            (ExprIRef.writeValBody newLambdaI . mkLam)
            & return
    where
        mkLam = V.BLam . V.Lam param

redexWrap :: Monad m => ValIProperty m -> T m V.Var
redexWrap exprP =
    do
        newValueI <- newHole
        newParam <- ExprIRef.newVar
        _ <- redexWrapWithGivenParam newParam newValueI exprP
        return newParam

data RecExtendResult m = RecExtendResult
    { rerNewTag :: T.Tag
    , rerNewVal :: ValI m
    , rerResult :: ValI m
    }

recExtend :: Monad m => ValI m -> T m (RecExtendResult m)
recExtend valI =
    do
        tag <- fst . GenIds.randomTag . RandomUtils.genFromHashable <$> Transaction.newKey
        newValueI <- newHole
        V.RecExtend tag newValueI valI & V.BRecExtend & ExprIRef.newValBody
            <&> RecExtendResult tag newValueI

data CaseResult m = CaseResult
    { crNewTag :: T.Tag
    , crNewVal :: ValI m
    , crResult :: ValI m
    }

case_ :: Monad m => ValI m -> T m (CaseResult m)
case_ tailI =
    do
        tag <- fst . GenIds.randomTag . RandomUtils.genFromHashable <$> Transaction.newKey
        newValueI <- newHole
        V.Case tag newValueI tailI & V.BCase & ExprIRef.newValBody
            <&> CaseResult tag newValueI

newPane :: Monad m => Anchors.CodeProps m -> DefI m -> T m ()
newPane codeProps defI =
    do
        let panesProp = Anchors.panes codeProps
        panes <- getP panesProp
        when (defI `notElem` Set.map Anchors.paneDef panes) $
            setP panesProp $ Set.insert (Anchors.Pane defI) panes

savePreJumpPosition :: Monad m => Anchors.CodeProps m -> WidgetId.Id -> T m ()
savePreJumpPosition codeProps pos = modP (Anchors.preJumps codeProps) $ (pos :) . take 19

jumpBack :: Monad m => Anchors.CodeProps m -> T m (Maybe (T m WidgetId.Id))
jumpBack codeProps =
    do
        preJumps <- getP (Anchors.preJumps codeProps)
        return $
            case preJumps of
            [] -> Nothing
            (j:js) ->
                Just $ do
                    setP (Anchors.preJumps codeProps) js
                    return j

isInfix :: Text -> Bool
isInfix x = not (Text.null x) && Text.all (`elem` operatorChars) x

presentationModeOfName :: Text -> PresentationMode
presentationModeOfName x
    | isInfix x = Infix
    | otherwise = Verbose

newDefinition ::
    Monad m => Text -> PresentationMode -> Definition (ValI m) () -> T m (DefI m)
newDefinition name presentationMode def =
    do
        newDef <- Transaction.newIRef def
        setP (Anchors.assocNameRef newDef) name
        setP (Anchors.assocPresentationMode newDef) presentationMode
        return newDef

-- Used when writing a definition into an identifier which was a variable.
-- Used in float.
newPublicDefinitionToIRef ::
    Monad m => Anchors.CodeProps m -> Definition (ValI m) () -> DefI m -> T m ()
newPublicDefinitionToIRef codeProps def defI =
    do
        Transaction.writeIRef defI def
        getP (Anchors.assocNameRef defI)
            <&> presentationModeOfName
            >>= setP (Anchors.assocPresentationMode defI)
        modP (Anchors.globals codeProps) (Set.insert defI)
        newPane codeProps defI

newPublicDefinitionWithPane ::
    Monad m =>
    Text -> Anchors.CodeProps m -> Definition (ValI m) () -> T m (DefI m)
newPublicDefinitionWithPane name codeProps def =
    do
        defI <- newDefinition name (presentationModeOfName name) def
        modP (Anchors.globals codeProps) (Set.insert defI)
        newPane codeProps defI
        return defI

newIdentityLambda :: Monad m => T m (V.Var, ValI m)
newIdentityLambda =
    do
        paramId <- ExprIRef.newVar
        getVar <- V.LVar paramId & V.BLeaf & ExprIRef.newValBody
        lamI <- V.Lam paramId getVar & V.BLam & ExprIRef.newValBody
        return (paramId, lamI)
