import ParseProg
import AST
import Data.List

data Mexp a b = BExp a b | Exp a b deriving Show


type AItem = (String, Int)

--data AList a = Alist ([AItem],Int) deriving Show
type AList = ([AItem],Int)

type ST = ([AItem],[AItem], Int, Int)
    
varList:: (Printer a, Printer b) => (Prog a b) -> ST
varList (Prog a) = ourList where 
        ourList = foldl(\(sTable,funList) func funList ->  parseFun func (sTable,funList) )  (([],[],0,0),[]) funcs 
        funcs = funList (Prog a)

funList:: (Prog a b) -> [Fun a b]
funList (Prog funcs) = funcs


parseFun:: (Printer a, Printer b) => (Fun a b) -> (ST,[Fun a b]) -> (ST,[Fun a b])
parseFun (Fun a) (symTable, funList)  = case (Fun a) of
    Fun (name, args, exp) -> (table, funs) where
        (symTable1,funs1) = rename (Fun a)
        (symTable2,funs1) =  parseFuncArgs args (symTable1,funList)
        (table,funs) = parseExp exp (symTable2, funs1)
  
-- check if (current arg , "any num") is an element of vlist (((printer arg),_) `elem` (vars)) 
parseFuncArgs::(Printer b) =>  [b] -> (ST, [Fun a b]) -> (ST, [Fun a b])
parseFuncArgs [] (symTable,funList) = (symTable,funList)
parseFuncArgs args (symTable,funList) = foldl(\((vars,funcs,vnum,fnum),funs) arg -> rename arg ((vars,funcs,vnum,fnum),funs)) (symTable,funList) args 

parseAItem:: (Printer b) =>  b -> ST -> AItem
parseAItem s (_, _, vnum,_) = (printer s, vnum)

parseExp:: (Printer a, Printer b) => (Exp a b) -> (ST,[Fun a b]) -> (ST, [Fun a b])
parseExp exp ((vlist, flist, vnum, fnum), funList) = case exp of
    VAR exp -> case ((printer exp) `elem` (listOfVars vlist)) of 
            True -> ((vlist, flist, vnum, fnum),funList)
            False -> renameVar z9((parseAItem exp sTable):vlist,flist,(vnum+1),fnum),funList) where
                sTable = (vlist, flist, vnum, fnum) 
    ADD exp1 exp2 -> list where
            (symTable1,funs1) = parseExp exp1 (vlist, flist, vnum, fnum)
            list = parseExp exp2 (symTable1,funs1)
    SUB exp1 exp2 -> list where    
            (symTable1,funs1) = parseExp exp1 (vlist, flist, vnum, fnum)
            list = parseExp exp2 (symTable1,funs1)
    MUL exp1 exp2 -> list where    
            (symTable1,funs1) = parseExp exp1 (vlist, flist, vnum, fnum)
            list = parseExp exp2 (symTable1,funs1)
    DIV exp1 exp2 -> list where    
            (symTable1,funs1) = parseExp exp1 (vlist, flist, vnum, fnum)
            list = parseExp exp2 (symTable1,funs1)
    NEG exp1 -> parseExp exp1 ((vlist, flist, vnum, fnum),funList)
    CONST exp -> ((("",exp):vlist, flist, vnum, fnum),funList) where
        sTable = (vlist, flist, vnum, fnum)
    COND bexp1 exp1 exp2 -> list where
            (symTable1,funs1) = parseBexp bexp1 ((vlist, flist, vnum, fnum), funList)
            (symTable2,funs2) = parseExp exp1 (symTable1,funs1)
            list = parseExp exp2 (symTable2,funs2)
    APP exp exps -> list where
        (symTable,funs1) = case (printer exp `elem` (map fst flist)) of 
            True -> ((vlist,flist,vnum,fnum), funList)
            False -> renameFun ((vlist,(printer exp,fnum):flist,vnum,(fnum+1)),funs) 
        list = foldl(\(sTable,funcs) exps -> parseExp exps (sTable,funcs)) (symTable,funs1) exps --fix app of fcn name
    LET funcs exp2 -> list where
        (symTable1,funs1) =  foldl(\(sTable,funs) func -> parseFun func (sTable,funs)) ((vlist, flist, vnum, fnum),funList) funcs  
        list = parseExp exp2 (symTable1,funs1)
 
 
--parseExp exp symtable
-- Var exp -> case () of 
--      True -> symtable
--      False -> ((parseAItem exp symtable):vlist, flist, (vnum+1), fnum) where
--      (vlist,flist,vnum,fnum) = symtable
--       
parseBexp:: (Printer a, Printer b) => (BExp a b) -> (ST,[Fun a b] -> (ST, [Fun a b])
parseBexp exp (symTable, funList) = case exp of
    Lt exp1 exp2 -> list where
        (symTable1,funs1) = parseExp exp1 (symTable, funList)
        list = parseExp exp2 (symTable1,funs1)     
    Gt exp1 exp2 -> list where
        (symTable1,funs1) = parseExp exp1 (symTable, funList)
        list = parseExp exp2 (symTable1,funs1)      
    Eq exp1 exp2 -> list where
        (symTable1,funs1) = parseExp exp1 (symTable, funList)
        list = parseExp exp2 (symTable1,funs1)         
    AND bexp1 bexp2 -> list where
        (symTable1,funs1) = parseExp exp1 (symTable, funList)
        list = parseExp exp2 (symTable1,funs1)       
    OR  bexp1 bexp2 -> list where
        (symTable1,funs1) = parseExp exp1 (symTable, funList)
        list = parseExp exp2 (symTable1,funs1)  
    NOT bexp1 -> parseBexp bexp1 (symTable, funList)   
    _ -> (symTable,funList)
    


listOfVars::  [AItem] -> [String]
listOfVars aList = foldr(\(x,y) acc -> x:acc) [] aList

fun1 = (Prog [Fun ("main",["x","y"],(ADD (VAR "x") (VAR "y")))])
      
test3 = (Prog [Fun ("main",[],(ADD (VAR "x") (VAR "y")))
             ,Fun ("f",["z"], (LET 
                   [Fun ("g",["a"],MUL (VAR "b") (VAR "c"))
                   ,Fun ("h",["d","e"], DIV (VAR "m") (VAR "j"))]
                     (ADD (APP "t" [VAR "k"])
                     (APP "h" [VAR "l",CONST 7])) ))])
test5 = (Prog 
             [Fun ("main",[],(ADD (VAR "x") (VAR "y")))
             ,Fun ("f",["x"], (LET 
                   [Fun ("g",["y"],MUL (VAR "y") (VAR "x"))
                   ,Fun ("h",["x","y"], DIV (VAR "x") (VAR "y"))]
                     (ADD (APP "g" [VAR "x"])
                     (APP "h" [VAR "x",CONST 7])) ))])



rename:: x -> (ST,[Fun a b]) -> (ST,[Fun a b])
rename x ((vList, fList, vNum, fNum),funList)  = case x of 
    (Fun (name, args, exp)) -> (table, funs) where
        (symTable1,funs1) = case (printer name `elem` (map fst fList)) of 
            True -> ((vLIst, fList, vNum, fNum), func:funList) where
                index = elemIndex (printer name) (map fst vList)
                (str, num) = fList !! index
                func = (Fun ("f" ++ (show num),args exp))
            False -> (sTable, funs) where
                (sTable, funs) = ((vList, (nameString, fNum):fList, vNum, (fNum + 1)), func:funList) where -- need a renameFun
                nameString = printer name  
                func = "f" ++ (show fNum)
        
    (Exp a b) -> 
    b -> case ((printer b) `elem` (map fst vList)) of 
    True -> ((vars,funcs,vnum,fnum),funs)
    False -> renameArg ((((printer arg),vnum):vars,funcs,(vnum+1),fnum),funs)

--renameExp:: (Exp a b) -> ST -> (Exp a b)

--renameFun:: (Fun a b) -> ST -> (Fun a b)

--renameArg:: ()
-

--[(name,[fcns],[Vargs],[Vfrees])]
