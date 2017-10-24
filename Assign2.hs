import ParseProg
import AST

data Mexp a b = BExp a b | Exp a b deriving Show


type AItem = (String, Int)

--data AList a = Alist ([AItem],Int) deriving Show
type AList = ([AItem],Int)

type ST = ([AItem],[AItem], Int, Int)
    
varList:: (Printer a, Printer b) => (Prog a b) -> ST
varList (Prog a) = ourList where 
        ourList = foldl(\sTable func ->  parseFun func sTable)  ([],[],0,0) funcs []
        funcs = funList (Prog a)

funList:: (Prog a b) -> [Fun a b]
funList (Prog funcs) = funcs


parseFun:: (Printer a, Printer b) => (Fun a b) -> ST -> ST 
parseFun (Fun a) (vList, fList, vNum, fNum)  = case (Fun a) of
    Fun (name, args, exp) -> table where
        symTable1 = case (printer name `elem` (map fst vList)) of 
            True -> (vList, fList, vNum, fNum) 
            False -> (vList, (nameString, fNum):fList, vNum, (fNum + 1)) where
                nameString = printer name           
        symTable2 =  parseFuncArgs args symTable1
        table = parseExp exp symTable2
  
-- check if (current arg , "any num") is an element of vlist (((printer arg),_) `elem` (vars)) 
parseFuncArgs::(Printer b) =>  [b] -> ST -> ST
parseFuncArgs [] symTable = symTable
parseFuncArgs args symTable = foldl(\(vars,funcs,vnum,fnum) arg  -> case ((printer arg) `elem` (map fst vars)) of 
    True -> (vars,funcs,vnum,fnum)
    False -> ((parseAItem arg (vars,funcs,vnum,fnum)):vars,funcs,(vnum+1),fnum)) symTable args

parseAItem:: (Printer b) =>  b -> ST -> AItem
parseAItem s (_, _, vnum,_) = (printer s, vnum)

parseExp:: (Printer a, Printer b) => (Exp a b) -> ST -> ST
parseExp exp (vlist, flist, vnum, fnum) = case exp of
    VAR exp -> case ((printer exp) `elem` (listOfVars vlist)) of 
            True -> (vlist, flist, vnum, fnum)
            False -> ((parseAItem exp sTable):vlist,flist,(vnum+1),fnum) where
                sTable = (vlist, flist, vnum, fnum) 
    ADD exp1 exp2 -> list where
            symTable1 = parseExp exp1 (vlist, flist, vnum, fnum)
            list = parseExp exp2 symTable1
    SUB exp1 exp2 -> list where    
            symTable1 = parseExp exp1 (vlist, flist, vnum, fnum)
            list = parseExp exp2 symTable1
    MUL exp1 exp2 -> list where    
            symTable1 = parseExp exp1 (vlist, flist, vnum, fnum)
            list = parseExp exp2 symTable1
    DIV exp1 exp2 -> list where    
            symTable1 = parseExp exp1 (vlist, flist, vnum, fnum)
            list = parseExp exp2 symTable1
    NEG exp1 -> parseExp exp1 (vlist, flist, vnum, fnum)
    CONST exp -> (("",exp):vlist, flist, vnum, fnum) where
        sTable = (vlist, flist, vnum, fnum)
    COND bexp1 exp1 exp2 -> list where
            symTable1 = parseBexp bexp1 (vlist, flist, vnum, fnum)
            symTable2 = parseExp exp1 symTable1
            list = parseExp exp2 symTable2
    APP exp exps -> list where
        symTable = case (printer exp `elem` (map fst flist)) of 
            True -> (vlist,flist,vnum,fnum)
            False -> (vlist,(printer exp,fnum):flist,vnum,(fnum+1)) 
        list = foldl(\sTable exps -> parseExp exps sTable) symTable exps --fix app of fcn name
    LET funcs exp2 -> list where
        symTable1 =  foldl(\sTable func -> parseFun func sTable) (vlist, flist, vnum, fnum) funcs  
        list = parseExp exp2 symTable1
 
 
--parseExp exp symtable
-- Var exp -> case () of 
--      True -> symtable
--      False -> ((parseAItem exp symtable):vlist, flist, (vnum+1), fnum) where
--      (vlist,flist,vnum,fnum) = symtable
--       
parseBexp:: (Printer a, Printer b) => (BExp a b) -> ST -> ST
parseBexp exp symTable = case exp of
    Lt exp1 exp2 -> list where
        symTable1 = parseExp exp1 symTable
        list = parseExp exp2 symTable1     
    Gt exp1 exp2 -> list where
        symTable1 = parseExp exp1 symTable
        list = parseExp exp2 symTable1        
    Eq exp1 exp2 -> list where
        symTable1 = parseExp exp1 symTable
        list = parseExp exp1 symTable1         
    AND bexp1 bexp2 -> list where
        symTable1 = parseBexp bexp1 symTable
        list = parseBexp bexp2 symTable1      
    OR  bexp1 bexp2 -> list where
        symTable1 = parseBexp bexp1 symTable
        list = parseBexp bexp2 symTable1        
    NOT bexp1 -> parseBexp bexp1 symTable   
    _ -> symTable
    


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



--rename:: [Fun a b] -> ST -> [Fun a b]

--renameExp:: (Exp a b) -> ST -> (Exp a b)

--renameFun:: (Fun a b) -> ST -> (Fun a b)
-

--[(name,[fcns],[Vargs],[Vfrees])]
