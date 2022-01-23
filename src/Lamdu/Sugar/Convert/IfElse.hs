-- | "if" sugar/guards conversion
{-# LANGUAGE TypeFamilies #-}

module Lamdu.Sugar.Convert.IfElse (convertIfElse) where

import qualified Control.Lens as Lens
import           Control.Monad.Once (OnceT)
import           Lamdu.Builtins.Anchors (boolTid, trueTag, falseTag)
import qualified Lamdu.Data.Ops as DataOps
import           Lamdu.Expr.IRef (ValI, iref)
import           Lamdu.Expr.UniqueId (ToUUID(..))
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

convertIfElse ::
    Monad m =>
    (ValI m -> T m (ValI m)) ->
    PostfixApply v InternalName (OnceT (T m)) (T m) # Annotated (ConvertPayload m) ->
    Maybe (IfElse v InternalName (OnceT (T m)) (T m) # Annotated (ConvertPayload m))
convertIfElse setToVal postApp =
    do
        cond <-
            postApp ^?
            pArg . hVal . _BodyPostfixApply .
            Lens.filteredBy (pFunc . hVal . _PfFromNom . tidTId . Lens.only boolTid) .
            pArg
        case postApp ^? pFunc . hVal . _PfCase . cList . tlItems . Lens._Just of
            Just (TaggedListBody alt0 [TaggedSwappableItem alt1 _])
                | tagOf alt0 == trueTag && tagOf alt1 == falseTag -> convIfElse cond alt0 alt1
                | tagOf alt1 == trueTag && tagOf alt0 == falseTag -> convIfElse cond alt1 alt0
            _ -> Nothing
    where
        tagOf alt = alt ^. tiTag . tagRefTag . tagVal
        convIfElse cond altTrue altFalse =
            Just IfElse
            { _iIf = setDelClause cond
            , _iThen = altTrue ^. tiValue & setDelClause
            , _iElse =
                case altFalse ^@?
                     tiValue . hVal . _BodyLam . lamFunc .
                     fBody . Lens.filteredBy hAnn <. hVal . bBody . _BinderTerm . _BodyIfElse
                of
                Just (Const innerPl, innerIfElse) ->
                    Ann
                    { _hVal =
                        ElseIf ElseIfBody
                        { _eAddLet = DataOps.redexWrap (innerPl ^. pStored) <&> EntityId.ofValI
                        , _eIfElse = innerIfElse
                        }
                    , _hAnn =
                        innerPl
                        & pLambdas .~ [altFalse ^. tiValue . hAnn . Lens._Wrapped . pStored . iref & toUUID]
                        & Const
                    }
                Nothing ->
                    altFalse ^. tiValue . hVal
                    & _BodyLam . lamFunc . fBody . annotation . pActions . delete %~ mkElseDel
                    & SimpleElse
                    & Ann (Const (altFalse ^. tiValue . annotation & pActions . delete %~ mkElseDel))
            }
            where
                setDelClause =
                    Lens.filteredBy (hVal . _BodyLeaf . _LeafHole) . annotation . pActions . delete .~ Delete delClause
                delClause =
                    fromMaybe (altFalse ^. tiValue . annotation)
                    (altFalse ^? tiValue . hVal . _BodyLam . lamFunc . fBody . annotation)
                    ^. pStored . iref
                    & setToVal <&> EntityId.ofValI
                mkElseDel CannotDelete =
                    thenBody ^. pStored . iref & setToVal
                    <&> EntityId.ofValI & Delete
                mkElseDel x = x
                thenBody =
                    altTrue ^? tiValue . hVal . _BodyLam . lamFunc . fBody
                    . Lens.filteredBy (hVal . bBody . _BinderTerm) . annotation
                    & fromMaybe (altTrue ^. tiValue . annotation)
