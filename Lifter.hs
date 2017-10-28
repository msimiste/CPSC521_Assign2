module Main where

import LambdaLift
import CallGraph
import AlphaRename
import AST
import Data.List
import Data.Maybe


main = do
    
    prog1 <- alphaRename fun2
    return prog1
    
    



fun2 = (Prog 
             [Fun ("main",["x","y"],(ADD (VAR "x") (VAR "y")))
             ,Fun ("f",["x"], (LET 
                   [Fun ("g",["y"],ADD (APP "main" [VAR "x"]) (VAR "x"))
                   ,Fun ("h",["x","y"], ADD (APP "main" [VAR "x"])(VAR "y"))]
                     (ADD (APP "g" [VAR "x"])
                     (APP "h" [VAR "x"])) ))])
