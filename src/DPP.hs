module DPP where

import Logic
import Heuristics
import Preprocessing

import Data.List
import qualified Data.Set as S

dPP f = do
    kb <- dimacs f
    let sol = dpp identity kb
    return sol

dpp :: Heuristic -> KB -> Bool
dpp _ [] = True
dpp h kb | elem [] kb = False
         | (not . null) vs1 = dpp h $ dppAux1 vs1 kb
         | (not . null) vs' = dpp h $ dppAux2 vs' kb
         | otherwise = dpp' kb (h kb vs) h
           where vs1 = [v|[v]<-kb]
                 ls  = literalsKB kb
                 vs  = S.elems $ S.map (var) ls
                 vs' = [v|v<-vs,S.notMember (-v) ls]

dpp' kb (v:vs) h = dpp h ([(delete v c1) `union` (delete (-v) c2)|c1<-kb1,c2<-kb2] ++ kb3)
             where (kb1,kbAux) = partitionL kb v
                   (kb2,kb3)   = partitionL kbAux (-v)

dppAux1 ls kb = foldr (\l acc -> [delete (-l) c|c <- acc, notElem l c]) kb ls
-- dppAux1' ls kb h = dpp h $ [delete (-l) c|c <- kb, notElem l c])

dppAux2 ls kb = foldr (\l acc -> [c|c<-acc, notElem l c]) kb ls

partitionL kb l = foldr (\c (acc1,acc2) -> if elem l c then (c:acc1,acc2) else (acc1,c:acc2)) ([],[]) kb

literalsKB :: KB -> S.Set Literal
literalsKB kb = foldr (\c acc -> literals c acc) S.empty kb

literals :: Clause -> S.Set Literal -> S.Set Literal
literals c acc = foldr (\v acc2 -> S.insert v acc2) acc c

varsKB :: KB -> S.Set Variable
varsKB kb = foldr (\c acc -> vars c acc) S.empty kb

vars :: Clause -> S.Set Variable -> S.Set Variable
vars c acc = foldr (\v acc2 -> S.insert (var v) acc2) acc c 