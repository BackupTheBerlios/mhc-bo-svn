
module Scanner where
import Char
import UU.Parsing

--- Definicion de los datas para Simbolos (Tokens) --

data TypeToken = Operator | KeyWord | Space | Integer | Bool | Id deriving (Eq,Ord,Show)

data Simbolo = Simbolo TypeToken String deriving Show

---Definicion de las Instancias de las clases que necesitamos ---

instance Symbol Simbolo

instance Eq Simbolo where
    (Simbolo Operator str ) == (Simbolo Operator str2 ) = str == str2
    (Simbolo KeyWord str ) == (Simbolo KeyWord str2 ) = str == str2
    (Simbolo tok1 _ ) == (Simbolo tok2 _ ) = tok1 == tok2

instance Ord Simbolo where
   compare (Simbolo Operator str) (Simbolo Operator str2) = compare str str2
   compare (Simbolo KeyWord str) (Simbolo KeyWord str2) = compare str str2
   compare (Simbolo tok1 _) (Simbolo tok2 _) = compare tok1 tok2




--- Definicion del Scanner ---
scanner :: String -> [Simbolo]

scanner [] = []
scanner a@(chr:str) | isUpper chr = getKeyWord [chr] str
		  | isLower chr = (Simbolo Id [chr]) : scanner str 
		  | isKeyOp [chr] =  getKeyOp a 
                  | isSpace chr = (Simbolo Space [chr]) : scanner str
		  | isDigit chr = getNumber [chr] str  
		  | otherwise = error ("Simbolo no reconocido " ++ [chr])
		 

getKeyWord str1 [] = if isKeyWord str1 then [(Simbolo KeyWord str1)] else if elem str1 boleanos then [(Simbolo Bool str1)] else error ("Simbolo no reconocido " ++ str1)
getKeyWord str1 a@(chr:str) | isUpper chr = getKeyWord (str1++[chr]) str
			    | isSpace chr = if isKeyWord str1 then (Simbolo KeyWord str1) : (Simbolo Space [chr]) : scanner str else if elem str1 boleanos then (Simbolo Bool str1):(Simbolo Space [chr]):scanner str else error "Simbolo no reconocido"
			    | otherwise = if isKeyWord str1 then (Simbolo KeyWord str1) : scanner str else if elem str1 boleanos then (Simbolo Bool str1): scanner str  else error "Sombolo no reconocido"

getKeyOp [] = []
getKeyOp [a] = [(Simbolo Operator [a])]
getKeyOp (a:b:as) | elem  (a:[b]) keyOps = (Simbolo Operator (a:[b])) : scanner as
		  | otherwise = (Simbolo Operator [a]) : scanner (b:as)

getNumber a [] = [(Simbolo Integer a )] 
getNumber a [b] = if isDigit b then [(Simbolo Integer (a ++ [b]))] else (Simbolo Integer a) : scanner [b]
getNumber a (b:as) | isDigit b = getNumber (a ++ [b]) as
		   | otherwise = (Simbolo Integer a) : scanner (b:as)

isKeyWord str = elem str keyWords 

isKeyOp str = elem str keyOps

boleanos = ["TRUE","FALSE"]
keyWords = ["LET" , "IN" , "IF" , "THEN" , "ELSE"]

keyOps = ["+" , "-" , "*" , "\\" , "/" , "<" , ">" , "=", "," , ";" , ">=" , "<=" , "==", "&&" , "||" , "->" , "(" , ")" ]
