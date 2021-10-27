-- | "if" sugar/guards conversion
{-# LANGUAGE TypeFamilies #-}

module Lamdu.Sugar.Convert.IfElse (convertIfElse) where

import qualified Control.Lens as Lens
import           Control.Monad.Once (OnceT)
import           Lamdu.Builtins.Anchors (boolTid, trueTag, falseTag)
import qualified Lamdu.Data.Ops as DataOps
import           Lamdu.Expr.IRef (ValI, iref)
import           Lamdu.Expr.UniqueId (ToUUID(..))
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

convertIfElse ::
    Monad m =>
    (ValI m -> T m (ValI m)) ->
    PostfixApply v InternalName (OnceT (T m)) (T m) # Annotated (ConvertPayload m a) ->
    Maybe (IfElse v InternalName (OnceT (T m)) (T m) # Annotated (ConvertPayload m a))
convertIfElse setToVal postApp =
    do
        cond <-
            postApp ^?
            pArg . hVal . _BodyPostfixApply .
            Lens.filteredBy (pFunc . hVal . _PfFromNom . tidTId . Lens.only boolTid) .
            pArg
            <&> Lens.filteredBy (hVal . _BodyLeaf . _LeafHole) . annotation .
                pActions . delete .~ deleteWholeIf
        case postApp ^. pFunc . hVal . _PfCase . cList . tlItems of
            [alt0, alt1]
                | tagOf alt0 == trueTag && tagOf alt1 == falseTag -> convIfElse cond alt0 alt1
                | tagOf alt1 == trueTag && tagOf alt0 == falseTag -> convIfElse cond alt1 alt0
            _ -> Nothing
    where
        deleteWholeIf = DataOps.newHole >>= setToVal <&> EntityId.ofValI & Delete
        tagOf alt = alt ^. tiTag . tagRefTag . tagVal
        convIfElse cond altTrue altFalse =
            Just IfElse
            { _iIf = cond
            , _iThen = altTrue ^. tiValue
            , _iElse =
                case altFalse ^?
                     tiValue . hVal . _BodyLam . lamFunc .
                     fBody . hVal . _BinderTerm . _BodyIfElse
                of
                Just innerIfElse ->
                    Ann
                    { _hVal = ElseIf innerIfElse
                    , _hAnn =
                        altFalse ^. tiValue . annotation
                        & pLambdas .~ [altFalse ^. tiValue . hAnn . Lens._Wrapped . pInput . Input.stored . iref & toUUID]
                        & Const
                    }
                Nothing ->
                    altFalse ^. tiValue . hVal
                    & _BodyLam . lamFunc . fBody . annotation . pActions . delete %~ mkElseDel
                    & SimpleElse
                    & Ann (Const (altFalse ^. tiValue . annotation & pActions . delete %~ mkElseDel))
            }
            where
                mkElseDel CannotDelete =
                    thenBody ^. pInput . Input.stored . iref & setToVal
                    <&> EntityId.ofValI & Delete
                mkElseDel x = x
                thenBody =
                    altTrue ^? tiValue . hVal . _BodyLam . lamFunc . fBody
                    . Lens.filteredBy (hVal . _BinderTerm) . annotation
                    & fromMaybe (altTrue ^. tiValue . annotation)
