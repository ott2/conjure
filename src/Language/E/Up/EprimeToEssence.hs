{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings  #-}
module Language.E.Up.EprimeToEssence(
    mainPure,
    mainPure',
    -- for debuging
    introduceTypes,
    introduceIndexRange,
    convertUnamed,
    combineInfos,
    convertRep,
    onlyNeeded,
    makeTuplesOfMatrixesMap

) where

import Language.E

import Language.E.Up.Common(unwrapExpr,wrapInTuple,unwrapTuple)
import Language.E.Up.Data
import Language.E.Up.Debug
import Language.E.Up.GatherInfomation(getVariables,getEnumMapping,getEnumsAndUnamed,getSolVariables,getEssenceVariables)
import Language.E.Up.RepresentationTree(createVarTree)
import Language.E.Up.EvaluateTree2(evalTree)
import Language.E.Up.GatherIndexRanges(gatherIndexRanges)

import Data.Char(isSpace)
import Data.Map(Map)

import qualified Data.Map as M
import qualified Data.Text as T

type Logs = [Text]

mainPure :: (Spec, Spec, Spec, Spec,Logs) -> [E]
mainPure = mainPure' True

mainPure' :: Bool -> (Spec, Spec, Spec, Spec,Logs) -> [E]
mainPure' addIndexRange (spec,sol,org,_,logs) =

    let tuplesOfMatrixes =  makeTuplesOfMatrixesMap logs
        varInfo1 = getVariables spec

        enumMapping1         = getEnumMapping org
        enums1               = getEnumsAndUnamed org
        (enumMapping, enums) = convertUnamed enumMapping1 enums1

        orgInfo  = getEssenceVariables enumMapping org
        solInfo1 = getSolVariables sol
        -- I don't think I need the aux variables
        solInfo2 = M.filterWithKey (\a _ ->  not $ isPrefixOf "v__" a) solInfo1
        solInfo  = M.filterWithKey (\a _ ->  not $ isPrefixOf "aux__" a) solInfo2
        varInfo2 = M.filterWithKey (\a _ ->  not $ isPrefixOf "v__" a) varInfo1
        varInfo  = M.filterWithKey (\a _ ->  not $ isPrefixOf "aux__" a) varInfo2

        varTrees = createVarTree varInfo
        varMap1  = combineInfos varInfo solInfo
        varMap   = M.map (\vd@VarData{vEssence=e} -> vd{vEssence=unwrapExpr e} ) varMap1

        varResults = map (\t -> evalTree (onlyNeeded varMap t ) tuplesOfMatrixes t ) varTrees

        wrap :: String -> E -> E
        wrap name value =
            [xMake| topLevel.letting.expr := [value]
                  | topLevel.letting.name.reference := [Prim (S (T.pack name))] |]

        indexrangeMapping = gatherIndexRanges org


        lookUp m = fromMaybe (error "fromMaybe EprimeToEssence: lookUpType")  . flip M.lookup m
        eval (s,e) =
            let orgType     = lookUp orgInfo s
                indext      = lookUp indexrangeMapping s
                res        = introduceTypes enumMapping orgType e
                withIndexes = introduceIndexRange indext res
            in wrap s $
               if addIndexRange then withIndexes else res

        resultEssence   = map eval varResults

    in enums ++ resultEssence 

makeTuplesOfMatrixesMap :: [Text] -> Map [String] Int
makeTuplesOfMatrixesMap =
      M.fromList
    . map  ( second ( textToInt .  tailTemp  )
           . first  (splitOn "_" .  dropWhile isSpace . T.unpack) 
           )
    . map (T.break (== '∑') . T.replace "~" "")
    . nub
    . mapMaybe (T.stripPrefix "[matrixToTuple]")

    where

    tailTemp t | T.length t == 0  = "1"  
    tailTemp t = T.tail t

    textToInt :: Text -> Int
    textToInt t = case reads (T.unpack t) of 
        [(i,_)] -> i
        _       -> error $ "Logs: [matrixToTuple] Unable to parse a int from " ++ show t


onlyNeeded :: M.Map String VarData -> Tree String ->  M.Map String VarData
onlyNeeded mapping (Branch s _) = M.filterWithKey (\k _ -> s `isPrefixOf` k ) mapping
onlyNeeded mapping (Leaf s ) = M.filterWithKey (\k _ -> s `isPrefixOf` k ) mapping
onlyNeeded mapping _ = mapping

combineInfos ::  M.Map String VarInfo -> M.Map String [E] -> M.Map String VarData
combineInfos varInfo solInfo =
   combineInfo (M.toAscList varInfo) (M.elems solInfo)

    where

    combineInfo :: [(String,VarInfo)] ->  [[E]] -> M.Map String VarData
    combineInfo  vs es = M.fromList $  zipWith combine' vs es

    combine' :: (String,VarInfo) -> [E] -> (String,VarData)
    combine' (s, VarInfo{indexes = ix, bounds = b}) es =
        (s,VarData{vIndexes=ix, vBounds=b, vEssence= es !! 1})


convertRep :: [TagT] -> (Bool,[TagT])
convertRep arr =
  let (bs,res) =  unzip $ map convertRep' arr
  in  (or bs, res)

convertRep' :: TagT -> (Bool,TagT)
convertRep' (TagSingle t) =  (b', TagSingle res)
    where (b',res) = convertTag t

convertRep' (TagEnum _  ) =  (True, TagSingle "int")
convertRep' (TagUnamed _) =  (True, TagSingle "int")

convertRep' (TagTuple ts) = (or b', TagTuple res)
    where (b',res) =  unzip $ map convertRep ts

convertRep' (TagFunc ins tos) = ( b1 || b2, TagFunc ins' tos')
    where
        (b1,ins') =  convertRep ins
        (b2,tos') =  convertRep tos

convertRep'  t = (False,t)

convertTag :: Tag -> (Bool,Tag)
convertTag "set"  = (True, "matrix")
convertTag "mset" = (True, "matrix")
convertTag "bool" = (True, "int")
convertTag t      = (False, t)

introduceTypes ::  M.Map String [E] -> [TagT] -> E -> E

introduceTypes emap ts [xMatch| [val] := expr |] =
    let res = introduceTypes emap ts val
    in  [xMake| expr := [res] |]

introduceTypes emap [TagEnum name] [xMatch| [Prim (I num)] := value.literal |] =
    let res = fromMaybe (_bugg "fromMaybe enums") $ M.lookup name emap
        selected = res !! fromInteger (num - 1)
    in  selected

introduceTypes emap [TagUnamed kind] e@[xMatch| [Prim (I _)] := value.literal |] =
    introduceTypes emap [TagEnum ("__named_" ++ kind)] e

introduceTypes _ [TagSingle "bool"] [xMatch| [Prim (I num)] := value.literal |] =
    let bool = num == 1
    in  [xMake| value.literal := [Prim (B bool)] |]

introduceTypes emap (TagSingle "matrix":ts) [xMatch| vs := value.matrix.values |] =
    let res = map (introduceTypes emap ts) vs
    in  [xMake| value.matrix.values := res |]

introduceTypes emap (TagSingle "set":ts) [xMatch| vs := value.matrix.values |] =
    let res = map (introduceTypes emap ts) vs
    in  [xMake| value.set.values := res |]

introduceTypes emap (TagSingle "mset":ts) [xMatch| vs := value.matrix.values |] =
    let res = map (introduceTypes emap ts) vs
    in  [xMake| value.mset.values := res |]

introduceTypes emap [TagTuple ts] [xMatch| vs := value.tuple.values |] =
    let res = zipWith (introduceTypes emap) ts vs
    in  [xMake| value.tuple.values := res |]

introduceTypes emap [TagRel ts] [xMatch| vs := value.relation.values |] =
    let res = map (zipWith (introduceTypes emap) ts . unwrapTuple ) vs
    in  [xMake| value.relation.values := (map wrapInTuple res)   |]

introduceTypes emap [TagFunc ins tos] [xMatch| arr := value.function.values |] =
   let mappings =  map (func ins tos) arr
   in  [xMake| value.function.values := mappings |]

    where
    func ins' tos' [xMatch| [a,b] := mapping |] =
       let a' = introduceTypes emap ins' a
           b' = introduceTypes emap tos' b
       in   [xMake| mapping := [a',b'] |]
    func _ _ _  = _bugg "EprimeToEssence: introduceTypes function"

introduceTypes emap [TagPar ts] [xMatch| partsArr := value.partition.values |] =
   let parts =  map par partsArr
   in  [xMake| value.partition.values := parts |]

    where
    par [xMatch| ps := part |] =
       let res = map (introduceTypes emap ts) ps
       in  [xMake| part := res |]
    par _ = _bugg "EprimeToEssence: introduceTypes partition"



introduceTypes _ [TagSingle _] e = e
{-introduceTypes _ ts e = e-}
introduceTypes _ ts e = _bugi "introduceTypes not handled" (ts,[e])


introduceIndexRange :: IndexT -> E -> E

introduceIndexRange (IndexMatrix ir it) [xMatch| vs := value.matrix.values |] =
    let vs' = map (introduceIndexRange it) vs
    in  [xMake| value.matrix.values     := vs'
              | value.matrix.indexrange := [ir] |]

introduceIndexRange (IndexTuple its) [xMatch| vs := value.tuple.values |] =
    let vs' = zipWith introduceIndexRange its vs
    in  [xMake| value.tuple.values := vs' |]

introduceIndexRange (IndexRel its) [xMatch| vs := value.relation.values |] =
    let vs' = map (zipWith introduceIndexRange  its . unwrapTuple ) vs
    in  [xMake| value.relation.values := (map wrapInTuple vs') |]

introduceIndexRange (IndexFunc ins tos) [xMatch| arr := value.function.values |] =
   let mappings =  map (func ins tos) arr
   in  [xMake| value.function.values := mappings |]

    where
    func ins' tos' [xMatch| [a,b] := mapping |] =
       let a' = introduceIndexRange ins' a
           b' = introduceIndexRange tos' b
       in   [xMake| mapping := [a',b'] |]
    func _ _ _  = _bugg "EprimeToEssence: introduceIndexRange function"

introduceIndexRange (IndexPar it) [xMatch| arr := value.partition.values |] =
   let parts =  map par arr
   in  [xMake| value.partition.values := parts |]

    where
    par [xMatch| vs := part |] =
       let vs' = map (introduceIndexRange it) vs
       in  [xMake| part := vs' |]
    par _  = _bugg "EprimeToEssence: introduceIndexRange partition"

introduceIndexRange (IndexSet it) [xMatch| vs := value.set.values |] =
    let vs' = map (introduceIndexRange it) vs
    in  [xMake| value.set.values := vs' |]

introduceIndexRange (IndexSet it) [xMatch| vs := value.mset.values |] =
    let vs' = map (introduceIndexRange it) vs
    in  [xMake| value.mset.values := vs' |]

introduceIndexRange IndexNone e = e

introduceIndexRange i [xMatch| [v] := expr |] =
    [xMake| expr :=  [introduceIndexRange i v] |]

introduceIndexRange _ e  = e
{-introduceIndexRange p e = error $ "introduceIndexRange" ++ (show p) ++ " " ++  (show  . prettyAsBoth $ e)-}


convertUnamed :: M.Map String [E] -> [E]  -> (M.Map String [E], [E])
convertUnamed m arr =
    let enumsParts = mapMaybe convertU arr
        m'         = M.union m (M.fromList enumsParts)
        enums      = map toEnum' enumsParts
        arr'       = arr ++ enums
    in  (m', arr')

    where toEnum' (name,es) =
            [xMake| topLevel.letting.name.reference  := [Prim (S ref) ]
                  | topLevel.letting.typeEnum.values := es |]

            where ref = T.pack  name


convertU :: E -> Maybe (String, [E])
convertU [xMatch| [Prim (S name)] := topLevel.letting.name.reference
                | [Prim (I num)]  := topLevel.letting.typeUnnamed.value.literal  |] =
    let vals = map (createEnum name ) [1..num]
    in Just ("__named_" ++ T.unpack name, vals)

    where
    createEnum :: Text -> Integer -> E
    createEnum name1 num1 = [xMake| reference := [Prim (S ref )] |]

        where ref = T.concat [ "_", name1, "_", (T.pack . show) num1]

convertU _ = Nothing

_bug :: String -> [E] -> t
_bug  s = upBug  ("EprimeToEssence: " ++ s)
_bugi :: (Show a) => String -> (a, [E]) -> t
_bugi s = upBugi ("EprimeToEssence: " ++ s )
_bugg :: String -> t
_bugg s = _bug s []
