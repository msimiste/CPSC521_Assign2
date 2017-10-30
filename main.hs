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
    (Prog prog) <- parseFile fname
    moddedProg <- modAST (Prog prog)
    putStr $ show_prog moddedProg
    --putStrLn $ show_prog moddedProg
    --let tokens = myLexer fconts
    --let ptree = pProg tokens
    --case ptree of
    --    Ok tree -> do
    --        let astree = transProg tree
    --        let symbT = beginProcess astree
    --        let iRep = transProgIR astree
    --        putStrLn $ (ppShow) symbT
    --        putStrLn $ (ppShow) astree
    --        putStrLn $ (ppShow) iRep
    --    Bad emgs -> putStrLn emgs
    --
