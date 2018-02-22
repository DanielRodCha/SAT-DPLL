module Heuristics where

import Logic

type Heuristic = KB -> [Variable] -> [Variable]

identity :: Heuristic
identity kb vs = vs