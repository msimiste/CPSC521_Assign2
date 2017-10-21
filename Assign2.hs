import ParseProg
import AST

data Mexp a b = BExp a b | Exp a b


--varList:: Mexpression -> [String]
--varList m = case m of 
--    Exp a b 


--varList:: Fun a b -> ([String], [String])
--varList  Fun (a, b, c) = 

type AItem = (String, Int)

--data AList a = Alist ([AItem],Int) deriving Show
type AList = ([AItem],Int)
    
varList:: (Printer b) => (Prog a b) -> Int -> AList 
varList (Prog a) num = ourList where
    --ourList = tail almostList where
        ourList = foldr(\func (acc,num) ->  parseFun func num)  ([],0)  funcs where
            funcs = funList (Prog a)

funList:: (Prog a b) -> [Fun a b]
funList (Prog funcs) = funcs


parseFun::(Printer b) => (Fun a b) -> Int -> AList 
parseFun (Fun a) num = case (Fun a) of
    Fun (name, vars, exp) -> parseFuncVars vars num 

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


parseVars:: (Printer b) => (Mexp a b) -> Int -> AList


fun1 = (Prog [Fun ("main",["x","y"],(ADD (VAR "x") (VAR "y")))])
          
