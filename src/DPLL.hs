module DPLL where

import Logic
import Heuristics
import Preprocessing

import Data.List
import qualified Data.Set as S

dPLL f = do
    kb <- dimacs f
    let sol = dpll identity kb
    return sol

dpll :: Heuristic -> KB -> Bool
dpll _ [] = True
dpll h kb | elem [] kb = False
          | (not . null) vs1 = dpll h $ dpllAux1 vs1 kb
          | (not . null) vs' = dpll h $ dpllAux2 vs' kb
          | otherwise = dpll' kb (h kb vs) h
            where vs1 = [v|[v]<-kb]
                  ls  = literalsKB kb
                  vs  = S.elems $ S.map (var) ls
                  vs' = [v|v<-vs,S.notMember (-v) ls]

dpll' kb (v:vs) h = (dpll h ((map (delete v) kb1) ++ kb3)) ||
                    (dpll h ((map (delete (-v)) kb2) ++ kb3))
                      where (kb1,kbAux) = partitionL kb v
                            (kb2,kb3)   = partitionL kbAux (-v)

dpllAux1 ls kb = foldr (\l acc -> [delete (-l) c|c <- acc, notElem l c]) kb ls
-- dpllAux1' ls kb h = dpll h $ [delete (-l) c|c <- kb, notElem l c])

dpllAux2 ls kb = foldr (\l acc -> [c|c<-acc, notElem l c]) kb ls

partitionL kb l = foldr (\c (acc1,acc2) -> if elem l c then (c:acc1,acc2) else (acc1,c:acc2)) ([],[]) kb

literalsKB :: KB -> S.Set Literal
literalsKB kb = foldr (\c acc -> literals c acc) S.empty kb

literals :: Clause -> S.Set Literal -> S.Set Literal
literals c acc = foldr (\v acc2 -> S.insert v acc2) acc c

varsKB :: KB -> S.Set Variable
varsKB kb = foldr (\c acc -> vars c acc) S.empty kb

vars :: Clause -> S.Set Variable -> S.Set Variable
vars c acc = foldr (\v acc2 -> S.insert (var v) acc2) acc c 