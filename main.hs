module Main where

import CallGraph
import AlphaRename
import LambdaLift
import ModdedAST
import AST
import Data.List
import Data.Maybe
import TestFiles
import ParseProg
import System.Environment

main = do
    args <- getArgs
    let fname = args !! 0
    test <- readFile fname
    putStrLn (show_prog( modAST (progToAST test)))
