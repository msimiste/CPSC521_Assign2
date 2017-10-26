
import ParseProg
import AST
import Data.List
import Data.Maybe -- prove this
import Data.String


--data MExp String String = BExp String String | Exp String String deriving Show



type AItem = (String, Int)

--data AList a = Alist ([AItem],Int) deriving Show
type AList = ([AItem],Int)

type ST = ([AItem],[AItem], Int, Int)
    
varList::  (Prog String String) -> [Fun String String]--ST
varList (Prog prog) = functions where 
        (ourList,functions) = foldl(\(sTable,funcs1) func ->  parseFun func (sTable,funcs1) )  (([],[],0,0),funcs) funcs 
        funcs = funList (Prog prog)

funList:: (Prog String String) -> [Fun String String]
funList (Prog funcs) = funcs


parseFun::  (Fun String String) -> (ST,[Fun String String]) -> (ST,[Fun String String])
parseFun func (symTable, funList)  = case func of
    Fun (name, args, exp) -> (table, funs) where
        (symTable1,funs1) = foldl(\(st,funs) func -> (st, ((snd(replaceFuncName (st, func))):funs))) (symTable, []) funList--rename1 func (symTable, funList)
        --(symTable2,funs2) =  parseFuncArgs func (symTable1,funs1)
        (table,funs) =  parseExp exp (symTable1, funs1)
  
-- check if (current arg , "any num") is an element of vlist (((printer arg),_) `elem` (vars)) 
parseFuncArgs::  (Fun String String) -> (ST, [Fun String String]) -> (ST, [Fun String String])
parseFuncArgs (Fun (name, [], exp)) (symTable,funList) = (symTable,(Fun (name, [], exp)):funList)
parseFuncArgs func (symTable,funList) = rename_args func (symTable, funList)


parseAItem::  String -> ST -> AItem
parseAItem s (_, _, vnum,_) = (printer s, vnum)

parseExp::  (ST, Exp String String) -> (Exp String String) 
parseExp exp (symTable, funList) = case exp of
    VAR exp -> (st,funs) where
        (st, ex) = replaceExpName (symTable, (VAR exp)) 
        funs = ex:funList
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
 
      
parseBexp::  (BExp String String) -> (ST,[Fun String String]) -> (ST, [Fun String String])
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

testTable = ([("y",1),("x",0)],[("main",9)],2,1)::ST

--rename:: (ST, [Fun String String]) -> (ST, [Fun String String])
--rename (table, funclist) = map (\(Fun (name,args,exp)) -> (Fun ((replaceName name), (replaceArgs args), (replaceExps exp))) ) funcList
--rename (table, funclist) = fold(\(st,funs) fun -> )(table,funclist) funclist

replaceFuncName:: (ST,(Fun String String)) -> (ST,(Fun String String))
replaceFuncName ((vList,fList,vNum,fNum),(Fun (name, args,exp))) = case (checkInList name fList) of 
    True -> ((vList, fList, vNum, fNum),retFunction) where
        index = getIndex name fList
        (str, num) = fList !! index
        retFunction = (Fun ("f" ++ show num,args,exp)) 
    False -> ((vList,(name,fNum):fList, vNum, (fNum+1)),retFunction) where
        retFunction = (Fun ("f" ++ show fNum,args,exp))

replaceExpName:: (ST, (Exp String String)) -> (ST, (Exp String String))
replaceExpName ((vList, fList, vNum, fNum), exp) = case exp of
    (VAR exp) -> case (checkInList  exp vList) of 
        True -> ((vList, fList, vNum, fNum), VAR exp1) where
            index = (getIndex exp vList)
            (str,num) = vList !! index
            funList1 = funList
            exp1 = ("X" ++ show num)
        False -> (((exp,vNum):vList, fList, (vNum+1), fNum), VAR exp1) where
            exp1 = ("X" ++ show vNum)
    
        
        



checkInList:: String -> [AItem] -> Bool
checkInList item list = (item `elem` (map fst list))
 
getIndex:: String -> [AItem] -> Int
getIndex item list = fromJust (elemIndex item (map fst list))
 
               -- (table, funs) = rename_args  (Fun ((revPrinter name1), args, exp)) ((vList, fList, vNum,fNum),funList)
               -- name1 = "f" ++ (show num)



rename1:: (Fun String String) -> (ST,[Fun String String]) -> (ST,[Fun String String])
rename1 function ((vList, fList, vNum, fNum),funList)  = case function of 
    (Fun (name, args, exp)) -> case (checkInList name fList) of 
            True -> ((vList, fList, vNum, fNum), funList1) where
                index = fromJust (elemIndex (printer name) (map fst fList))
                (str, num) = fList !! index
                (table, funs) = rename_args  (Fun (name1, args, exp)) ((vList, fList, vNum,fNum),funList)
                name1 = "f" ++ (show num)
                funList1 = funs
                   
            False -> (sTable, functions) where
                sTable1 = (vList, (nameString, fNum):fList, vNum, (fNum + 1)) 
                nameString = printer name 
                (sTable, functions) = rename_args (Fun (name1, args, exp)) (sTable1,funList) 
                name1 = "f" ++ (show fNum) 
                              
rename_var:: (Exp String String) -> (ST, [Fun String String]) -> (ST, [Fun String String])
rename_var eXp ((vList, fList, vNum, fNum),funList) =  case eXp of
    (VAR exp) -> case (checkInList exp vList) of 
        True -> ((vList, fList, vNum, fNum), funList1) where
            index = getIndex exp vList
            (str,num) = vList !! index
            funList1 = funList
            --exp = ("X" ++ show vNum)
        False -> (sTable, funList1) where
            sTable = (((printer exp),vNum):vList,fList,(vNum+1),fNum)
            funList1 = funList
            --exp = ("X" ++ show vNum)

--replaceVar:: (Exp String String) -> (Exp String String) -> [Fun String String] -> [Fun String String]

rename_args:: (Fun String String) -> (ST, [Fun String String]) -> (ST, [Fun String String])
rename_args (Fun (name, args, exp)) (symTable, funList)  = (table, functions) where
    (table, args1) = foldl(\(sTable,args2) arg -> (sTable,(snd (rename_arg arg sTable)):args2)) (symTable, []) args 
    func = (Fun (name, args1, exp))
    functions = func:funList


rename_arg:: String -> ST -> (ST, String)
rename_arg arg  (vList, fList, vNum, fNum) = case (checkInList arg vList) of 
    True -> (table, str) where
            index = getIndex arg vList
            (str, num) = vList !! index
            --argVal = "X" ++ show vNum
            table = (vList, fList, vNum, fNum)
    
    False -> (table, arg) where
            table = ((arg,vNum):vList,fList,(vNum+1),fNum)
            --argVal = "X" ++ show vNum
                
rename_funName:: String-> (ST, [Fun String String]) -> (ST, [Fun String String])
rename_funName name ((vList, fList, vNum, fNum),funList) =  case ((printer name) `elem` (map fst fList)) of 
    True -> ((vList,fList,vNum,fNum), funList)
    False -> ((vList,(printer name,fNum):fList,vNum,(fNum+1)),funList)

myMap:: ST -> (ST -> a -> (ST, a)) ->  a -> (ST, a)
myMap table f 


