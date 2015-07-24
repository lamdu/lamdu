{-# LANGUAGE NoImplicitPrelude, RecordWildCards, OverloadedStrings, ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
module Lamdu.Data.ExampleDB
    ( initDB, createPublics
    , withDB
    ) where

import           Prelude.Compat

import           Control.Lens.Operators
import           Control.Monad (unless, void)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Writer (WriterT)
import qualified Control.Monad.Trans.Writer as Writer
import           Control.MonadA (MonadA)
import           Data.Foldable (traverse_)
import           Data.List.Split (splitOn)
import qualified Data.Map as Map
import           Data.Store.Db (Db)
import qualified Data.Store.Db as Db
import           Data.Store.Rev.Branch (Branch)
import qualified Data.Store.Rev.Branch as Branch
import           Data.Store.Rev.Version (Version)
import qualified Data.Store.Rev.Version as Version
import qualified Data.Store.Rev.View as View
import           Data.Store.Transaction (Transaction, setP)
import qualified Data.Store.Transaction as Transaction
import           Data.String (IsString(..))
import qualified Lamdu.Builtins.Anchors as Builtins
import           Lamdu.Data.Anchors (assocTagOrder, ParamList, assocFieldParamList, PresentationMode(..))
import qualified Lamdu.Data.DbLayout as Db
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Ops as DataOps
import           Lamdu.Expr.IRef (DefI, ValI)
import qualified Lamdu.Expr.IRef as ExprIRef
import           Lamdu.Expr.Nominal (Nominal(..))
import           Lamdu.Expr.Pure (($$))
import qualified Lamdu.Expr.Pure as Pure
import           Lamdu.Expr.Scheme (Scheme(..))
import qualified Lamdu.Expr.Scheme as Scheme
import           Lamdu.Expr.Type (Type, (~>))
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.TypeVars as TV
import qualified Lamdu.Expr.UniqueId as UniqueId
import           Lamdu.Expr.Val (Val)
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.GUI.WidgetIdIRef as WidgetIdIRef
import qualified System.Directory as Directory
import           System.FilePath ((</>))

type T = Transaction

setName :: (MonadA m, UniqueId.ToGuid a) => a -> String -> T m ()
setName x = setP . Db.assocNameRef $ x

setTagOrder :: MonadA m => T.Tag -> Builtins.Order -> T m ()
setTagOrder tag order = Transaction.setP (assocTagOrder tag) order

namedId ::
    forall a m. (MonadA m, IsString a, UniqueId.ToGuid a) => String -> T m a
namedId name =
    do
        setName tag name
        return tag
    where
        tag :: a
        tag = fromString name

forAll :: forall a. TV.VarKind a => Int -> ([a] -> Type) -> Scheme
forAll count f =
    Scheme
    { _schemeForAll = mconcat $ map TV.singleton typeVars
    , _schemeConstraints = mempty
    , _schemeType = f $ map TV.lift typeVars
    }
    where
        typeVars :: [T.Var a]
        typeVars = take count $ map (fromString . (:[])) ['a'..'z']

recordType :: [(T.Tag, Type)] -> Type
recordType = T.TRecord . foldr (uncurry T.CExtend) T.CEmpty

sumType :: [(T.Tag, Type)] -> Type
sumType = T.TSum . foldr (uncurry T.CExtend) T.CEmpty

data Public m = Public
    { publicDefs :: [DefI m]
    , publicTags :: [T.Tag]
    , publicTIds :: [T.Id]
    }

instance Monoid (Public m) where
    mempty = Public mempty mempty mempty
    mappend (Public x0 y0 z0) (Public x1 y1 z1) =
        Public (mappend x0 x1) (mappend y0 y1) (mappend z0 z1)

publicize :: (Monad m, Monoid s) => m b -> (b -> s) -> WriterT s m b
publicize act g =
    do
        x <- lift act
        Writer.tell $ g x
        return x

type M m = WriterT (Public m) (T m)

blessAnchors :: MonadA m => M m ()
blessAnchors =
    do
        mapM_ describeAnchorTag Builtins.anchorTags
        lift $ setName Builtins.listTid "List"
        Writer.tell $ mempty { publicTIds = [Builtins.listTid] }
    where
        describeAnchorTag (order, tag, name) =
            do
                lift $ setName tag name
                lift $ setTagOrder tag order
                Writer.tell $ mempty { publicTags = [tag] }

newTag :: MonadA m => Int -> String -> M m T.Tag
newTag order n =
    do
        tag <- publicize (namedId n) $ \x -> mempty { publicTags = [x] }
        lift $ setTagOrder tag order
        return tag

newTId :: MonadA m => String -> M m T.Id
newTId n = publicize (namedId n) $ \x -> mempty { publicTIds = [x] }

newPublicDef ::
    Monad m => m (DefI n) -> WriterT (Public n) m (DefI n)
newPublicDef act = publicize act $ \x -> mempty { publicDefs = [x] }

type TypeCtor = [Type] -> Type

newNominal ::
    MonadA m =>
    T.Id -> [(T.ParamId, T.TypeVar)] ->
    ({-fixpoint:-}TypeCtor -> Scheme) ->
    Transaction m TypeCtor
newNominal tid params body =
    do
        Transaction.writeIRef (ExprIRef.nominalI tid) $
            Nominal (Map.fromList params) scheme
        return tinst
    where
        tinst typeParams =
            T.TInst tid $ Map.fromList $ zip (map fst params) typeParams
        scheme = body tinst

newPublicDefVal ::
    MonadA m => String -> PresentationMode -> ValI m -> Scheme ->
    WriterT (Public m) (Transaction m) (DefI m)
newPublicDefVal name presentationMode valI typ =
    newPublicDef $
    DataOps.newDefinition name presentationMode .
    Definition.BodyExpr $ Definition.Expr valI $
    Definition.ExportedType $ typ

data CtorInfo = CtorInfo
    { ctorTag :: T.Tag
    , _ctorPresentationMode :: PresentationMode
    , _ctorScheme :: Scheme
    }

data Ctor
    = Nullary CtorInfo
    | Normal Type CtorInfo

adt ::
    MonadA m => T.Id -> [(T.ParamId, T.TypeVar)] -> (TypeCtor -> [Ctor]) ->
    M m TypeCtor
adt tid params ctors =
    lift $ newNominal tid params $
    \t ->
        ctors t
        <&> onCtor
        & sumType
        & Scheme.mono
    where
        onCtor (Nullary info) = (ctorTag info, recordType [])
        onCtor (Normal typ info) = (ctorTag info, typ)

createList :: MonadA m => T.ParamId -> M m TypeCtor
createList valTParamId =
    adt Builtins.listTid [(valTParamId, valT)] $ \list ->
    [ Nullary $ CtorInfo Builtins.nilTag Verbose $ forAll 1 $ \[a] -> list [a]
    , let consType =
              recordType
              [ (Builtins.headTag, T.TVar valT)
              , (Builtins.tailTag, list [T.TVar valT])
              ]
      in  Normal consType $
          CtorInfo Builtins.consTag Infix $
          forAll 1 $ \ [a] -> consType ~> list [a]
    ]
    where
        valT = "a"

createMaybe ::
    MonadA m => T.ParamId -> M m TypeCtor
createMaybe valTParamId =
    do
        tid <- newTId "Maybe"
        adt tid [(valTParamId, valT)] $ \maybe_ ->
            [ Nullary $ CtorInfo Builtins.nothingTag Verbose $
              forAll 1 $ \[a] -> maybe_ [a]
            , Normal (T.TVar valT) $ CtorInfo Builtins.justTag Verbose $
              forAll 1 $ \[a] -> a ~> maybe_ [a]
            ]
    where
        valT = "a"

newtype BoolNames = BoolNames
    { bnTid :: T.Id
    }

createBool :: MonadA m => M m (Type, BoolNames)
createBool =
    do
        tid <- newTId "Bool"
        tyCon <-
            adt tid [] $ \boolTCons ->
            [ Nullary $ CtorInfo Builtins.trueTag Verbose $
              Scheme.mono $ boolTCons []
            , Nullary $ CtorInfo Builtins.falseTag Verbose $
              Scheme.mono $ boolTCons []
            ]
        return
            ( tyCon []
            , BoolNames { bnTid = tid }
            )

caseBool :: BoolNames -> V.Var -> V.Var -> Val () -> Val () -> Val () -> Val ()
caseBool boolNames v1 v2 cond then_ else_ =
    cases $$ Pure.fromNom (bnTid boolNames) cond
    where
        cases =
            Pure._case Builtins.trueTag (Pure.abs v1 then_) $
            Pure._case Builtins.falseTag (Pure.abs v2 else_) $
            Pure.absurd

setParamList :: MonadA m => ValI m -> ParamList -> M m ()
setParamList lambdaI tags =
    lift $ Transaction.setP (assocFieldParamList lambdaI) $ Just tags

newPublicFunc ::
    MonadA m => String -> PresentationMode -> [T.Tag] ->
    ([Val ()] -> Val ()) -> Scheme -> M m (DefI m)
newPublicFunc name presentationMode tags mkBody scheme =
    do
        v <- lift ExprIRef.newVar
        lambdaI <-
            lift $ ExprIRef.newVal $ Pure.lambdaRecord v tags mkBody
        setParamList lambdaI tags
        newPublicDefVal name presentationMode lambdaI scheme

createIf :: MonadA m => Type -> BoolNames -> M m (DefI m)
createIf bool boolNames =
    do
        condTag <- newTag 0 "condition"
        thenTag <- newTag 1 "then"
        elseTag <- newTag 2 "else"
        v1 <- lift ExprIRef.newVar
        v2 <- lift ExprIRef.newVar
        newPublicFunc "if" OO
            [condTag, thenTag, elseTag]
            (\[cond, then_ , else_] -> caseBool boolNames v1 v2 cond then_ else_) $
            forAll 1 $ \[a] -> recordType
            [ (condTag, bool)
            , (thenTag, a)
            , (elseTag, a)
            ] ~> a

createPublics :: MonadA m => T m (Public m)
createPublics =
    do
        blessAnchors

        valTParamId <- lift $ namedId "val"

        _ <- createList valTParamId
        _ <- createMaybe valTParamId
        (bool, boolNames) <- createBool

        _ <- createIf bool boolNames

        let infixType lType rType resType =
                recordType [ (Builtins.infixlTag, lType)
                                      , (Builtins.infixrTag, rType)
                                      ] ~> resType

        traverse_
            ((`newPublicBuiltinQualified_` Scheme.mono (infixType T.TInt T.TInt T.TInt)) .
              ("Prelude." ++))
            ["+", "-", "*", "^"]
        newPublicBuiltin_ "%" Infix ["Prelude"] "mod" $ Scheme.mono $ infixType T.TInt T.TInt T.TInt
        newPublicBuiltin_ "//" Infix ["Prelude"] "div" $ Scheme.mono $ infixType T.TInt T.TInt T.TInt
        newPublicBuiltinQualified_ "Prelude.negate" $ Scheme.mono $ T.TInt ~> T.TInt
        newPublicBuiltinQualified_ "Prelude.sqrt" $ Scheme.mono $ T.TInt ~> T.TInt

        let aToAToBool = forAll 1 $ \[a] -> infixType a a bool
        traverse_ ((`newPublicBuiltinQualified_` aToAToBool) . ("Prelude." ++))
            ["==", "/=", "<=", ">=", "<", ">"]
    & Writer.runWriterT <&> snd
    where
        newPublicBuiltin_ name presentationMode ffiPath ffiName typ =
            void $ newPublicBuiltin name presentationMode ffiPath ffiName typ
        newPublicBuiltin name presentationMode ffiPath ffiName typ =
            newPublicDef $
            DataOps.newDefinition name presentationMode .
            Definition.BodyBuiltin $ Definition.Builtin (Definition.FFIName ffiPath ffiName) typ
        newPublicBuiltinQualified fullyQualifiedName =
            newPublicBuiltin name (DataOps.presentationModeOfName name) path name
            where
                path = init fqPath
                name = last fqPath
                fqPath = splitOn "." fullyQualifiedName
        newPublicBuiltinQualified_ fullyQualifiedName typ =
            void $ newPublicBuiltinQualified fullyQualifiedName typ

newBranch :: MonadA m => String -> Version m -> T m (Branch m)
newBranch name ver =
    do
        branch <- Branch.new ver
        setName (Branch.guid branch) name
        return branch

initDB :: Db -> IO ()
initDB db =
    Db.runDbTransaction db $
    do
        exists <- Transaction.irefExists $ Db.branches Db.revisionIRefs
        unless exists $
            do
                emptyVersion <- Version.makeInitialVersion []
                master <- newBranch "master" emptyVersion
                view <- View.new master
                let writeRevAnchor f = Transaction.writeIRef (f Db.revisionIRefs)
                writeRevAnchor Db.view view
                writeRevAnchor Db.branches [master]
                writeRevAnchor Db.currentBranch master
                writeRevAnchor Db.redos []
                let paneWId = WidgetIdIRef.fromIRef $ Db.panes Db.codeIRefs
                writeRevAnchor Db.cursor paneWId
                Db.runViewTransaction view $
                    do
                        public <- createPublics
                        let writeCodeAnchor f = Transaction.writeIRef (f Db.codeIRefs)
                        writeCodeAnchor Db.globals (publicDefs public)
                        writeCodeAnchor Db.panes []
                        writeCodeAnchor Db.preJumps []
                        writeCodeAnchor Db.preCursor paneWId
                        writeCodeAnchor Db.postCursor paneWId
                        writeCodeAnchor Db.tids (publicTIds public)
                        writeCodeAnchor Db.tags (publicTags public)
                -- Prevent undo into the invalid empty revision
                newVer <- Branch.curVersion master
                Version.preventUndo newVer

withDB :: FilePath -> (Db -> IO a) -> IO a
withDB lamduDir body =
    do
        Directory.createDirectoryIfMissing False lamduDir
        Db.withDb (lamduDir </> "codeedit.db") $ \db ->
            do
                initDB db
                body db
