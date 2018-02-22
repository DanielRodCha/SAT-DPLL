module Logic where 

import qualified Data.Set as S

type Variable = Int
type Literal  = Variable

var :: Literal -> Variable
var = abs

sign :: Literal -> Int
sign = signum

type Clause = [Literal]
type KB = [Clause]