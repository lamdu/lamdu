{-# LANGUAGE NoImplicitPrelude, RecordWildCards #-}
module Lamdu.Data.Ops
    ( newHole, wrap, setToWrapper
    , replace, replaceWithHole, setToHole, lambdaWrap, redexWrap
    , redexWrapWithGivenParam
    , recExtend, RecExtendResult(..)
    , case_, CaseResult(..)
    , addListItem
    , newPublicDefinitionWithPane
    , newDefinition
    , savePreJumpPosition, jumpBack
    , newPane
    , makeNewTag, makeNewPublicTag
    , isInfix
    ) where

import           Prelude.Compat

import           Control.Lens.Operators
import           Control.Monad (when)
import           Control.MonadA (MonadA)
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction, getP, setP, modP)
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.WidgetId as WidgetId
import qualified Lamdu.Builtins.Anchors as Builtins
import           Lamdu.CharClassification (operatorChars)
import           Lamdu.Data.Anchors (PresentationMode(..))
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Expr.GenIds as GenIds
import           Lamdu.Expr.IRef (DefI, ValTree(..))
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.UniqueId as UniqueId
import qualified Lamdu.Expr.Val as V
import qualified System.Random.Utils as RandomUtils

type T = Transaction

setToWrapper ::
    MonadA m => ExprIRef.ValI m -> ExprIRef.ValIProperty m -> T m (ExprIRef.ValI m)
setToWrapper wrappedI destP =
    do
        newFuncI <- newHole
        resI <- ExprIRef.newValBody . V.BApp $ V.Apply newFuncI wrappedI
        Property.set destP resI
        return resI

wrap ::
    MonadA m =>
    ExprIRef.ValIProperty m ->
    T m (ExprIRef.ValI m)
wrap exprP =
    do
        newFuncI <- newHole
        applyI <- ExprIRef.newValBody . V.BApp . V.Apply newFuncI $ Property.value exprP
        Property.set exprP applyI
        return applyI

newHole :: MonadA m => T m (ExprIRef.ValI m)
newHole = ExprIRef.newValBody $ V.BLeaf V.LHole

replace ::
    MonadA m =>
    ExprIRef.ValIProperty m ->
    ExprIRef.ValI m ->
    T m (ExprIRef.ValI m)
replace exprP newExprI =
    do
        Property.set exprP newExprI
        return newExprI

replaceWithHole :: MonadA m => ExprIRef.ValIProperty m -> T m (ExprIRef.ValI m)
replaceWithHole exprP = replace exprP =<< newHole

setToHole :: MonadA m => ExprIRef.ValIProperty m -> T m (ExprIRef.ValI m)
setToHole exprP =
    exprI <$ ExprIRef.writeValBody exprI hole
    where
        hole = V.BLeaf V.LHole
        exprI = Property.value exprP

lambdaWrap :: MonadA m => ExprIRef.ValIProperty m -> T m (V.Var, ExprIRef.ValI m)
lambdaWrap exprP =
    do
        (newParam, newExprI) <- ExprIRef.newLambda $ Property.value exprP
        Property.set exprP newExprI
        return (newParam, newExprI)

redexWrapWithGivenParam ::
    MonadA m =>
    V.Var -> ExprIRef.ValI m -> ExprIRef.ValIProperty m ->
    T m (ExprIRef.ValI m)
redexWrapWithGivenParam param newValueI exprP =
    do
        newLambdaI <-
            ExprIRef.newValBody $ V.BAbs $ V.Lam param $ Property.value exprP
        newApplyI <- ExprIRef.newValBody . V.BApp $ V.Apply newLambdaI newValueI
        Property.set exprP newApplyI
        return newLambdaI

redexWrap :: MonadA m => ExprIRef.ValIProperty m -> T m (V.Var, ExprIRef.ValI m)
redexWrap exprP =
    do
        newValueI <- newHole
        newParam <- ExprIRef.newVar
        newLambdaI <- redexWrapWithGivenParam newParam newValueI exprP
        return (newParam, newLambdaI)

data RecExtendResult m = RecExtendResult
    { rerNewTag :: T.Tag
    , rerNewVal :: ExprIRef.ValI m
    , rerResult :: ExprIRef.ValI m
    }

recExtend :: MonadA m => ExprIRef.ValIProperty m -> T m (RecExtendResult m)
recExtend valP =
    do
        tag <- fst . GenIds.randomTag . RandomUtils.genFromHashable <$> Transaction.newKey
        newValueI <- newHole
        resultI <-
            ExprIRef.newValBody . V.BRecExtend $
            V.RecExtend tag newValueI $ Property.value valP
        Property.set valP resultI
        return $ RecExtendResult tag newValueI resultI

data CaseResult m = CaseResult
    { crNewTag :: T.Tag
    , crNewVal :: ExprIRef.ValI m
    , crResult :: ExprIRef.ValI m
    }

case_ :: MonadA m => ExprIRef.ValIProperty m -> T m (CaseResult m)
case_ valP =
    do
        tag <- fst . GenIds.randomTag . RandomUtils.genFromHashable <$> Transaction.newKey
        newValueI <- newHole
        resultI <-
            ExprIRef.newValBody . V.BCase $
            V.Case tag newValueI $ Property.value valP
        Property.set valP resultI
        return $ CaseResult tag newValueI resultI

addListItem ::
    MonadA m =>
    ExprIRef.ValIProperty m ->
    T m (ExprIRef.ValI m, ExprIRef.ValI m)
addListItem exprP =
    do
        newItemI <- newHole
        newListI <-
            ExprIRef.writeValTree $
            v $ V.BToNom $ V.Nom Builtins.listTid $
            v $ V.BInject $ V.Inject Builtins.consTag $
            recEx Builtins.headTag (ValTreeLeaf newItemI) $
            recEx Builtins.tailTag (ValTreeLeaf (Property.value exprP))
            recEmpty
        Property.set exprP newListI
        return (newListI, newItemI)
    where
        v = ValTreeNode
        recEx tag val rest = v $ V.BRecExtend $ V.RecExtend tag val rest
        recEmpty           = v $ V.BLeaf V.LRecEmpty

newPane :: MonadA m => Anchors.CodeProps m -> DefI m -> T m ()
newPane codeProps defI =
    do
        let panesProp = Anchors.panes codeProps
        panes <- getP panesProp
        when (defI `notElem` panes) $
            setP panesProp $ Anchors.makePane defI : panes

savePreJumpPosition :: MonadA m => Anchors.CodeProps m -> WidgetId.Id -> T m ()
savePreJumpPosition codeProps pos = modP (Anchors.preJumps codeProps) $ (pos :) . take 19

jumpBack :: MonadA m => Anchors.CodeProps m -> T m (Maybe (T m WidgetId.Id))
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

isInfix :: String -> Bool
isInfix x = not (null x) && all (`elem` operatorChars) x

presentationModeOfName :: String -> PresentationMode
presentationModeOfName x
    | isInfix x = Infix 5
    | otherwise = OO

newDefinition ::
    MonadA m => String -> PresentationMode ->
    Definition.Body (ExprIRef.ValI m) -> T m (DefI m)
newDefinition name presentationMode defBody =
    do
        newDef <- Transaction.newIRef defBody
        setP (Anchors.assocNameRef newDef) name
        setP (Anchors.assocPresentationMode newDef) presentationMode
        return newDef

newPublicDefinition ::
    MonadA m => Anchors.CodeProps m -> ExprIRef.ValI m -> String -> T m (DefI m)
newPublicDefinition codeProps bodyI name =
    do
        defI <-
            Definition.Expr bodyI Definition.NoExportedType
            & Definition.BodyExpr
            & newDefinition name (presentationModeOfName name)
        modP (Anchors.globals codeProps) (defI :)
        return defI

newPublicDefinitionWithPane ::
    MonadA m => Anchors.CodeProps m -> ExprIRef.ValI m -> T m (DefI m)
newPublicDefinitionWithPane codeProps bodyI =
    do
        defI <- newPublicDefinition codeProps bodyI ""
        newPane codeProps defI
        return defI

makeNewTag :: MonadA m => String -> T m T.Tag
makeNewTag name =
    do
        tag <- UniqueId.new
        setP (Anchors.assocNameRef tag) name
        return tag

makeNewPublicTag :: MonadA m => Anchors.CodeProps m -> String -> T m T.Tag
makeNewPublicTag codeProps name =
    do
        tag <- makeNewTag name
        modP (Anchors.tags codeProps) (tag :)
        return tag
