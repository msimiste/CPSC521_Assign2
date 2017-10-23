import ParseProg
import AST

data Mexp a b = BExp a b | Exp a b deriving Show


type AItem = (String, Int)

--data AList a = Alist ([AItem],Int) deriving Show
type AList = ([AItem],Int)
    
varList:: (Printer b) => (Prog a b) -> Int -> AList
varList (Prog a) num = ourList where 
        ourList = foldl(\(acc,y) func ->  parseFun func y (acc,y))  ([],num) funcs 
        funcs = funList (Prog a)

funList:: (Prog a b) -> [Fun a b]
funList (Prog funcs) = funcs


parseFun::(Printer b) => (Fun a b) -> Int -> AList -> AList
parseFun (Fun a) num aList = case (Fun a) of
    Fun (name, args, exp) -> list where
        (flist, num1) =  parseFuncArgs args num aList
        list = parseExp exp num1 (flist, num1) 
                
parseFuncArgs::(Printer b) =>  [b] -> Int -> AList -> AList
parseFuncArgs [] num list = list
parseFuncArgs args num (aItems, aNum) = foldl(\(acc,n) arg  -> case ((printer arg) `elem` (listOfVars (aItems,aNum))) of 
    True -> (acc, n)
    False -> ((parseAItem arg n):acc,n+1)) (aItems, aNum) args

parseAItem:: (Printer b) =>  b -> Int -> AItem
parseAItem s num = (printer s, num)

parseExp:: (Printer b) => (Exp a b) -> Int -> AList -> AList
parseExp exp num (aItems,aNum) = case exp of
    VAR exp -> case ((printer exp) `elem` (listOfVars (aItems,aNum))) of 
            True -> (aItems, aNum)
            False -> ([parseAItem exp num] ++ aItems,num+1)
    ADD exp1 exp2 -> list where
            (flist, num1) = parseExp exp1 num (aItems, aNum)
            list = parseExp exp2 num1  (flist, num1)
    SUB exp1 exp2 -> list where    
            (flist, num1) = parseExp exp1 num (aItems, aNum)
            list = parseExp exp2 num1  (flist, num1)
    MUL exp1 exp2 -> list where    
            (flist, num1) = parseExp exp1 num (aItems, aNum)
            list = parseExp exp2 num1  (flist, num1)
    DIV exp1 exp2 -> list where    
            (flist, num1) = parseExp exp1 num (aItems, aNum)
            list = parseExp exp2 num1 (flist, num1)
    NEG exp1 -> parseExp exp1 num (aItems, aNum)
    CONST exp -> ([parseAItem "V" exp] ++ aItems, aNum)
    COND bexp1 exp1 exp2 -> list where
            (flist, num1) = parseBexp bexp1 num (aItems, aNum)
            (slist, num2) = parseExp exp1 num1 (flist, num1)
            list = parseExp exp2 num2 (slist, num2)
    APP exp exps -> foldl(\(acc,y) exps  -> parseExp exps y (acc,y)) (aItems,aNum) exps
    LET funcs exp2 -> list where
        (flist, num1) =  foldl(\(acc,y) func  -> parseFun func y (acc,y)) (aItems,aNum) funcs  
        list = parseExp exp2 num1 (flist, num1)
       

parseBexp:: (Printer b) => (BExp a b) -> Int -> AList -> AList
parseBexp exp num (aItems, aNum) = case exp of
    Lt exp1 exp2 -> list where
        (flist, num1) = parseExp exp2 num (aItems, aNum)
        list = parseExp exp1  num1 (flist, num1)     
    Gt exp1 exp2 -> list where
        (flist, num1) = parseExp exp2 num (aItems, aNum)
        list = parseExp exp1  num1 (flist, num1)       
    Eq exp1 exp2 -> list where
        (flist, num1) = parseExp exp2 num (aItems, aNum)
        list = parseExp exp1 num1 (flist, num1)        
    AND bexp1 bexp2 -> list where
        (flist, num1) = parseBexp bexp2 num (aItems, aNum)
        list = parseBexp bexp1 num1 (flist, num1)      
    OR  bexp1 bexp2 -> list where
        (flist, num1) = parseBexp bexp2 num (aItems, aNum)
        list = parseBexp bexp1 num1 (flist, num1)       
    NOT bexp1 -> parseBexp bexp1 num (aItems, aNum)    
    _ -> ([], num)
    


listOfVars:: AList -> [String]
listOfVars (aList, num) = foldr(\(x,y) acc -> x:acc) [] aList

fun1 = (Prog [Fun ("main",["x","y"],(ADD (VAR "x") (VAR "y")))])
      
test3 = (Prog [Fun ("main",[],(ADD (VAR "x") (VAR "y")))
             ,Fun ("f",["z"], (LET 
                   [Fun ("g",["a"],MUL (VAR "b") (VAR "c"))
                   ,Fun ("h",["d","e"], DIV (VAR "h") (VAR "j"))]
                     (ADD (APP "g" [VAR "k"])
                     (APP "h" [VAR "l",CONST 7])) ))])
test5 = (Prog 
             [Fun ("main",[],(ADD (VAR "x") (VAR "y")))
             ,Fun ("f",["x"], (LET 
                   [Fun ("g",["y"],MUL (VAR "y") (VAR "x"))
                   ,Fun ("h",["x","y"], DIV (VAR "x") (VAR "y"))]
                     (ADD (APP "g" [VAR "x"])
                     (APP "h" [VAR "x",CONST 7])) ))])

    
