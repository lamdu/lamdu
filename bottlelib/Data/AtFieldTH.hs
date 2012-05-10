{-# LANGUAGE CPP, TemplateHaskell #-}
module Data.AtFieldTH (make) where

import Control.Arrow (second)
import qualified Data.Char as Char
import qualified Data.Function as Function
import qualified Data.List as List
import Language.Haskell.TH.Syntax

make :: Name -> Q [Dec]
make typeName = do
    TyConI typeCons <- reify typeName
    return $ makePure typeName typeCons

makePure :: Name -> Dec -> [Dec]
makePure typeName (NewtypeD _ _ typeVars ctor _) =
    constructorAtFuncs (makeAtNameForNewtype typeName) typeName typeVars [ctor]
makePure typeName (DataD _ _ typeVars ctors _) =
    constructorAtFuncs makeAtNameForDataField typeName typeVars ctors
makePure _ typeCons = error $ show typeCons ++ " not supported!"

-- A sortOn for when you lack an Ord instance.
-- An Eq instance is required.
-- Equal values are garaunteed to be grouped together. Other than that the order is arbitrary.
-- O(n^2) complexity.
sillySortOn :: Eq b => (a -> b) -> [a] -> [a]
sillySortOn _ [] = []
sillySortOn f (x:xs) =
    x : eq ++ sillySortOn f neq
    where
        (eq, neq) = List.partition ((== f x) . f) xs

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn = List.groupBy . Function.on (==)

groups :: Eq a => [(a, b)] -> [(a, [b])]
groups =
    map f . groupOn fst . sillySortOn fst
    where
        f xs = (fst (head xs), map snd xs)

constructorAtFuncs :: (Name -> Name) -> Name -> [TyVarBndr] -> [Con] -> [Dec]
constructorAtFuncs makeAtName typeName typeVars ctors =
    concatMap (fieldAtFunc makeAtName typeName typeVars isFreeVar) fields
    where
        fields =
            (map . second) checkAllCtors . groups .
            filter (not . ("_" `List.isPrefixOf`) . nameBase . fst . fst) $
            concatMap constructorFields ctors
        checkAllCtors xs
            | length xs == length ctors = Nothing
            | otherwise = Just xs
        constructorVars = concatMap (List.nub . varsOfType . snd . fst) fields
        isFreeVar v = 1 == countElemRepetitions v constructorVars

constructorFields :: Con -> [((Name, Type), Name)]
constructorFields (RecC conName fields) =
    map f fields
    where
        f (name, _, t) = ((name, t), conName)
constructorFields (NormalC _ []) = []
constructorFields (NormalC name _) = error $ "Constructor " ++ show name ++ " contains non-record fields. This is not supported by Data.AtFieldTH.make"
constructorFields x = error $ "unsupported constructor " ++ show x ++ " supplied to Data.AtFieldTH.make"

countElemRepetitions :: Eq a => a -> [a] -> Int
countElemRepetitions x = length . filter (== x)

-- When field name is *Typename, such as unTypename, or getTypename,
-- As is common convention for 'conceptual' newtype wrappers, we name the accessor atTypename.
-- Otherwise we name it as we do normally with Data constructors.
makeAtNameForNewtype :: Name -> Name -> Name
makeAtNameForNewtype newTypeName fieldName
    | typeNameStr `List.isSuffixOf` nameBase fieldName = mkName $ "at" ++ typeNameStr
    | otherwise = makeAtNameForDataField fieldName
    where
        typeNameStr = nameBase newTypeName

makeAtNameForDataField :: Name -> Name
makeAtNameForDataField fieldName =
    mkName $ "at" ++ (Char.toUpper fieldNameHead : fieldNameTail)
    where
        fieldNameHead : fieldNameTail = nameBase fieldName

unusedWarnPragma :: Name -> [Dec]
unusedWarnPragma name =
    [ SigD extraName $ ConT ''Int
    , FunD extraName [ Clause [] clause [] ]
    ]
    where
        extraName = mkName $ "_unused_pragma_" ++ nameBase name
        clause = NormalB $ foldl AppE (VarE 'const) [LitE (IntegerL 9), VarE name]

fieldAtFunc :: (Name -> Name) -> Name -> [TyVarBndr] -> (Name -> Bool) -> ((Name, Type), Maybe [Name]) -> [Dec]
fieldAtFunc makeAtName typeName typeVars isFreeVar ((fieldName, fieldType), mCtors) =
    [ SigD resName . ForallT resultTypeVars [] $ foldr1 arrow
        [ arrow (sideType fieldType "Src") (sideType fieldType "Dst")
        , input
        , output
        ]
    , FunD resName $ case mCtors of
        Nothing -> [mkClause (VarP valName)]
        Just ctors ->
            map (mkClause . AsP valName . (`RecP` [])) ctors ++
            [ Clause [WildP, VarP valName] (NormalB (VarE valName)) [] ]
    ] ++ unusedWarnPragma resName
    where
        arrow = AppT . AppT ArrowT
        funcName = mkName "func"
        valName = mkName "val"
        resName = makeAtName fieldName
        mkClause valPat = Clause [VarP funcName, valPat] clause []
        clause = NormalB $ RecUpdE (VarE valName) [(fieldName, applyExp)]
        applyExp = AppE (VarE funcName) . AppE (VarE fieldName) $ VarE valName
        valType = foldl AppT (ConT typeName) $ map (VarT . tyVarBndrName) typeVars
        sideType t suffix = mapTypeVarNames (sideTypeVar suffix) t
        fieldVars = List.nub (varsOfType fieldType)
        sideTypeVar suffix name =
            if isFreeVar name && elem name fieldVars
            then mkName (nameBase name ++ suffix) else name
        input = sideType valType "Src"
        output = sideType valType "Dst"
        resultTypeVars = map PlainTV . List.nub $ concatMap varsOfType [input, output]

tyVarBndrName :: TyVarBndr -> Name
tyVarBndrName (PlainTV name) = name
tyVarBndrName (KindedTV name _) = name

-- TODO [BP]: can boilerplate be reduced with SYB/uniplate ?
varsOfType :: Type -> [Name]
varsOfType (ForallT _ _ t) = varsOfType t -- TODO: dwa need todo something wrt the predicates?
varsOfType (VarT x) = [x]
varsOfType (ConT _) = []
varsOfType (TupleT _) = []
#if __GLASGOW_HASKELL__ >= 704
varsOfType (UnboxedTupleT _) = []
#endif
varsOfType ArrowT = []
varsOfType ListT = []
varsOfType (AppT x y) = varsOfType x ++ varsOfType y
varsOfType (SigT x _) = varsOfType x

-- TODO BP
mapTypeVarNames :: (Name -> Name) -> Type -> Type
mapTypeVarNames func (ForallT vars cxt t) =
    ForallT vars (map (mapPredVarNames func) cxt) (mapTypeVarNames func t)
mapTypeVarNames func (VarT x) = VarT (func x)
mapTypeVarNames _ (ConT x) = ConT x
mapTypeVarNames _ (TupleT x) = TupleT x
#if __GLASGOW_HASKELL__ >= 704
mapTypeVarNames _ (UnboxedTupleT x) = UnboxedTupleT x
#endif
mapTypeVarNames _ ArrowT = ArrowT
mapTypeVarNames _ ListT = ListT
mapTypeVarNames func (AppT x y) = AppT (mapTypeVarNames func x) (mapTypeVarNames func y)
mapTypeVarNames func (SigT t kind) = SigT (mapTypeVarNames func t) kind

-- TODO BP
mapPredVarNames :: (Name -> Name) -> Pred -> Pred
mapPredVarNames func (ClassP name types) = ClassP name $ map (mapTypeVarNames func) types
mapPredVarNames func (EqualP x y) = EqualP (mapTypeVarNames func x) (mapTypeVarNames func y)
