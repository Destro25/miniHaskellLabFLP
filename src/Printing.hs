
module Printing (showExp) where

import Exp
import Data.List (intercalate)

showVar :: Var -> String
showVar v = show v

showExp :: ComplexExp -> String
showExp (CX v) = showVar v
showExp (Nat n) = show n
