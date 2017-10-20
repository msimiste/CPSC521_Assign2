import ParseProg
import AST

data Mexpression a b = BExp a b | Exp a b


--varList:: Mexpression -> [String]
--varList m = case m of 
--    Exp a b 


--varList:: Fun a b -> ([String], [String])
--varList  Fun (a, b, c) = 

type AItem = (String, Int)

--data AList a = Alist ([AItem],Int) deriving Show
type AList = ([AItem],Int)
    
varList:: (Printer b) => (Prog a b) -> Int -> AList 
varList (Prog a) num = theList where
    --theList = foldr (\funcs acc -> parseFun funcs num) ([],0) nummap (\funcs -> parseFun funcs num) funs where
    funs = funList (Prog a)
    list1 = 
--foldl(\acc func -> parseFun (fst func) acc) 0 ([],0)

--parseFuncVars vars num where
    --((Fun (name, vars, exp)):funcs) = funList (Prog a)

--varList (Prog (Fun (name, vars, exp):funs)) num = parseFuncVars vars num 
    --Prog (f:fs) -> case f of
        --Fun (name,vars, exp) -> case vars  of
           -- True -> parseVars vars num
    --BExp a b -> False:(varList exp)
    --Exp a b -> True:(varList exp) 

funList:: (Prog a b) -> [Fun a b]
funList (Prog funcs) = funcs


parseFun::(Printer b) => (Fun a b) -> Int -> AList 
parseFun (Fun a) num = case a of
    (name, vars, exp) -> parseFuncVars vars num 

parseFuncVars::(Printer b) =>  [b] -> Int -> AList 
parseFuncVars []  num = ([], num)
parseFuncVars (s:str) num = (aItems, number) where
    (aItems,number) = ([(parseAItem s num)] ++ fst (parseFuncVars str (num+1)),num+2)
     
--((map (\x -> parseAItem x (num+1)) str),num)
--((foldr(\x y -> map parseItem y x) num str),)
--((s,num)):(parseVars str num+1)


parseAItem:: (Printer b) =>  b -> Int -> AItem
parseAItem s num = (printer s, num)
--((((printer s),num):(parseVars str (num+1))),num)


fun1 = (Prog [Fun ("main",["x","y"],(ADD (VAR "x") (VAR "y")))])
          
