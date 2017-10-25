import ParseProg
import AST
import Data.List
import Data.Maybe -- prove this


data Mexp a b = BExp a b | Exp a b deriving Show


type AItem = (String, Int)

--data AList a = Alist ([AItem],Int) deriving Show
type AList = ([AItem],Int)

type ST = ([AItem],[AItem], Int, Int)
    
varList:: (Printer a, Printer b, RevPrinter a) => (Prog a b) -> ST
varList (Prog a) = ourList where 
        (ourList,functions) = foldl(\(sTable,funcs1) func ->  parseFun func (sTable,funcs1) )  (([],[],0,0),funcs) funcs 
        funcs = funList (Prog a)

funList:: (Prog a b) -> [Fun a b]
funList (Prog funcs) = funcs


parseFun:: (Printer a, Printer b, RevPrinter a) => (Fun a b) -> (ST,[Fun a b]) -> (ST,[Fun a b])
parseFun func (symTable, funList)  = case func of
    Fun (name, args, exp) -> (table, funs) where
        (symTable1,funs1) = rename func (symTable, funList)
        --(symTable2,funs2) =  parseFuncArgs func (symTable1,funs1)
        (table,funs) = parseExp exp (symTable1, funs1)
  
-- check if (current arg , "any num") is an element of vlist (((printer arg),_) `elem` (vars)) 
parseFuncArgs::(Printer b) =>  (Fun a b) -> (ST, [Fun a b]) -> (ST, [Fun a b])
parseFuncArgs (Fun (name, [], exp)) (symTable,funList) = (symTable,(Fun (name, [], exp)):funList)
parseFuncArgs func (symTable,funList) = rename_args func (symTable, funList)


parseAItem:: (Printer b) =>  b -> ST -> AItem
parseAItem s (_, _, vnum,_) = (printer s, vnum)

parseExp:: (Printer a, Printer b, RevPrinter a) => (Exp a b) -> (ST,[Fun a b]) -> (ST, [Fun a b])
parseExp exp (symTable, funList) = case exp of
    VAR exp -> rename_var (VAR exp) (symTable, funList)     
    ADD exp1 exp2 -> (tbl,list) where
            (symTable1,funs1) = parseExp exp1 (symTable, funList)
            (tbl, list1) = parseExp exp2 (symTable1, funs1)
            list = list1                
    SUB exp1 exp2 -> (tbl,list) where    
            (symTable1,funs1) = parseExp exp1 (symTable, funList)
            (tbl, list1) = parseExp exp2 (symTable1, funs1)
            list = list1
    MUL exp1 exp2 -> (tbl,list) where    
            (symTable1,funs1) = parseExp exp1 (symTable, funList)
            (tbl, list1) = parseExp exp2 (symTable1, funs1)
            list = list1
    DIV exp1 exp2 -> (tbl,list) where    
            (symTable1,funs1) = parseExp exp1 (symTable, funList)
            (tbl, list1) = parseExp exp2 (symTable1, funs1)
            list = list1
    NEG exp1 -> parseExp exp1 (symTable,funList)
    CONST exp -> ((("",exp):vlist, flist, vnum, fnum), funList) where
        (vlist, flist, vnum, fnum) = symTable
    COND bexp1 exp1 exp2 -> (tbl,list) where
            (symTable1,funs1) = parseBexp bexp1 (symTable, funList)
            (symTable2,funs2) = parseExp exp1 (symTable1,funs1)
            (tbl,funs3) = parseExp exp2 (symTable2,funs2)
            list = funs3
    APP exp exps -> (tbl,list) where
        (symTable,funs1) = rename_funName exp (symTable,funList)
        (tbl, funs2) = foldl(\(sTable,funcs) exps -> parseExp exps (sTable,funcs)) (symTable,funs1) exps
        list = funs2 --fix app of fcn name
    LET funcs exp2 -> (tbl,list) where
        (symTable1,funs1) =  foldl(\(sTable,funs) func -> parseFun func (sTable,funs)) (symTable,funList) funcs  
        (tbl,funs2) = parseExp exp2 (symTable1,funs1)
        list = funs2
 
--parseExp exp symtable
-- Var exp -> case () of 
--      True -> symtable
--      False -> ((parseAItem exp symtable):vlist, flist, (vnum+1), fnum) where
--      (vlist,flist,vnum,fnum) = symtable
--       
parseBexp:: (Printer a, Printer b, RevPrinter a) => (BExp a b) -> (ST,[Fun a b]) -> (ST, [Fun a b])
parseBexp exp (symTable, funList) = case exp of
    Lt exp1 exp2 -> (tbl,list) where
        (symTable1,funs1) = parseExp exp1 (symTable, funList)
        (tbl,funs2) = parseExp exp2 (symTable1,funs1)
        list = funs2     
    Gt exp1 exp2 -> (tbl,list) where
        (symTable1,funs1) = parseExp exp1 (symTable, funList)
        (tbl,funs2) = parseExp exp2 (symTable1,funs1)
        list = funs2      
    Eq exp1 exp2 -> (tbl,list) where
        (symTable1,funs1) = parseExp exp1 (symTable, funList)
        (tbl,funs2) = parseExp exp2 (symTable1,funs1)
        list = funs2         
    AND bexp1 bexp2 -> (tbl,list) where
        (symTable1,funs1) = parseBexp bexp1 (symTable, funList)
        (tbl,funs2) = parseBexp bexp2 (symTable1,funs1)
        list = funs2       
    OR  bexp1 bexp2 -> (tbl,list) where
        (symTable1,funs1) = parseBexp bexp1 (symTable, funList)
        (tbl,funs2) = parseBexp bexp2 (symTable1,funs1)
        list = funs2  
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


rename:: (Printer a, Printer b, RevPrinter a) => (Fun a b) -> (ST,[Fun a b]) -> (ST,[Fun a b])
rename function ((vList, fList, vNum, fNum),funList)  = case function of 
    (Fun (name, args, exp)) -> case ((printer name) `elem` (map fst fList)) of 
            True -> ((vList, fList, vNum, fNum), funList1) where
                index = fromJust (elemIndex (printer name) (map fst fList))
                (str, num) = fList !! index
                (table, funs) = rename_args  (Fun ((revPrinter name1), args, exp)) ((vList, fList, vNum,fNum),funList)
                name1 = "f" ++ (show num)
                funList1 = funs
                   
            False -> (sTable, functions) where
                sTable1 = (vList, (nameString, fNum):fList, vNum, (fNum + 1)) 
                nameString = printer name 
                (sTable, functions) = rename_args (Fun ((revPrinter name1), args, exp)) (sTable1,funList) 
                name1 = "f" ++ (show fNum) 
                              
rename_var:: (Exp a b) -> (ST, [Fun a b]) -> (ST, [Fun a b])
rename_var eXp ((vList, fList, vNum, fNum),funList) =  case eXp of
    (VAR exp) -> case ((printer exp) `elem` (listOfVars vList)) of 
        True -> ((vList, fList, vNum, fNum), funList1) where
            index = fromJust (elemIndex (printer exp) (map fst vList))
            (str,num) = vList !! index
            funList1 = (VAR exp1):funList
            exp1 = (revPrinter "X" ++ show vNum)
        False -> (sTable, funList1) where
            sTable = (((printer exp),vNum):vList,fList,(vNum+1),fNum)
            funList1 = (VAR exp1):funList
            exp1 = (revPrinter "X" ++ show vNum)

rename_args:: (Fun a b) -> (ST, [Fun a b]) -> (ST, [Fun a b])
rename_args (Fun (name, args, exp)) (symTable, funList)  = (table, functions) where
    (table, args1) = foldl(\(sTable,args2) arg -> rename_arg arg sTable) (symtable,[]) args 
    func = (Fun (name, args1, exp))
    functions = func:funList


rename_arg:: b  -> ST -> (ST, b)
rename_arg arg  (vList, fList, vNum, fNum) = case ((printer arg) `elem` (map fst vList)) of 
    True -> (table, argVal) where
            index = (elemIndex (printer b) (map fst vList))
            (str, num) = fromJust vList !! index
            argVal = "X" ++ show vNum
            table = (vList, fList, vNum, fNum)
    
    False -> (table, argVal) where
            table = (((printer arg),vNum):vlist,flist,(vNum+1),fNum)
            argVal = "X" ++ show vNum
                
rename_funName:: a -> (ST, [Fun a b]) -> (ST, [Fun a b])
rename_funName name ((vList, fList, vNum, fNum),funList) =  case ((printer name) `elem` (map fst flist)) of 
    True -> ((vList,fList,vNum,fNum), ("f" ++ show name):funList)
    False -> ((vList,(printer name,fNum):fList,vNum,(fNum+1)),funList)
