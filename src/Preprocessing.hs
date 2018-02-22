module Preprocessing where

import Logic

import Data.List (foldl')

-- | __(dimacs f)__ is the pair (/ps/,/vs/) where ps is the set of formulas
-- wich corresponds to the formula in DIMACS format writed in the file /f/ and
-- /vs/ is the list of variables wich occurs in any propositional formula.
dimacs f = do
  s0 <- readFile f
  return $ aux2 $ foldl' (\acc l -> (map aux ((aux3 . words) l)):acc) [] $ lines $ s0
     where aux3 (c:cs) | c == "c" || c == "p" = ["0"]
           aux3 cs = init cs
           aux xs  = (read xs)::Int
           aux2 xs = (xs::KB)
