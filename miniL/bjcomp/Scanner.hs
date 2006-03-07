
module Scanner(scanner) where
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

scanner file = scanner1 file 1 

scanner1 :: String -> Int -> [Simbolo]

scanner1 [] _ = []
scanner1 a@(chr:str) fila | isUpper chr = getKeyWord [chr] str fila
		  				  | isLower chr = (Simbolo Id [chr]) : scanner1 str fila
				   		  | isKeyOp [chr] =  getKeyOp a fila
                     	  | isSpace chr = if chr == '\n' then (Simbolo Space [chr]) : scanner1 str (fila+1) else (Simbolo Space [chr] ): scanner1 str fila
		             	  | isDigit chr = getNumber [chr] str fila
		             	  | otherwise = error ("Simbolo no reconocido " ++ [chr])

getKeyWord str1 [] fila = if isKeyWord str1 then [(Simbolo KeyWord str1)] else if elem str1 keyBool then [(Simbolo Bool str1)] else error ("Simbolo no reconocido " ++ str1)
getKeyWord str1 a@(chr:str) fila | isUpper chr = getKeyWord (str1++[chr]) str fila
			    				 | isSpace chr = if isKeyWord str1 then (Simbolo KeyWord str1) : (Simbolo Space [chr]) : scanner1 str fila else if elem str1 keyBool then (Simbolo Bool str1):(Simbolo Space [chr]):scanner1 str fila else error "Simbolo no reconocido"
			 				     | otherwise = if isKeyWord str1 then (Simbolo KeyWord str1) : scanner1 str fila else if elem str1 keyBool then (Simbolo Bool str1): scanner1 str fila else error "Simbolo no reconocido"

getKeyOp [] fila = []
getKeyOp [a] fila = [(Simbolo Operator [a])]
getKeyOp (a:b:as) fila | elem  (a:[b]) keyOps = (Simbolo Operator (a:[b])) : scanner1 as fila
		  		       | otherwise = (Simbolo Operator [a]) : scanner1 (b:as) fila

getNumber a [] fila = [(Simbolo Integer a )] 
getNumber a [b] fila = if isDigit b then [(Simbolo Integer (a ++ [b]))] else (Simbolo Integer a) : scanner1 [b] fila
getNumber a (b:as) fila | isDigit b = getNumber (a ++ [b]) as fila
		   			    | otherwise = (Simbolo Integer a) : scanner1 (b:as) fila

isKeyWord str = elem str keyWords 

isKeyOp str = elem str keyOps

keyBool = ["TRUE","FALSE"]
keyWords = ["LET" , "IN" , "IF" , "THEN" , "ELSE"]
keyOps = ["+" , "-" , "*" , "\\" , "/" , "<" , ">" , "=", "," , ";" , ">=" , "<=" , "==", "&&" , "||" , "->" , "(" , ")" ]
