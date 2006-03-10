module Scanner where

import UU.Parsing
import Char

data Simbolo = Simbolo TokenType String Int Int deriving Show 

data TokenType = Cadena | Caracter | TipoBasico | Entero | Boleano | Reservada | Operador | OperadorEspecial | IdMayor | IdMenor | NuevaLinea | Espacio deriving (Eq,Ord,Show)

instance Symbol Simbolo

instance Eq Simbolo where
    (Simbolo OperadorEspecial str _ _ ) == (Simbolo OperadorEspecial str2 _ _) = str == str2
    (Simbolo Reservada str _ _) == (Simbolo Reservada str2 _ _) = str == str2
    (Simbolo tok1 _ _ _) == (Simbolo tok2 _ _ _) = tok1 == tok2
    
instance Ord Simbolo where
   compare (Simbolo OperadorEspecial str _ _) (Simbolo OperadorEspecial str2 _ _) = compare str str2
   compare (Simbolo Reservada str _ _) (Simbolo Reservada str2 _ _) = compare str str2
   compare (Simbolo tok1 _ _ _) (Simbolo tok2 _ _ _) = compare tok1 tok2
   
	


scanner1 []  _ _ = []
scanner1 (a:as) nf nc | a == '\"' = scannerCadena as nf (nc+1) []
		     | a == '\'' = scannerCaracter as nf (nc+1) 
		     | isDigit a = scannerEntero (a:as) nf (nc) []
		     | isLower a = scannerMenor (a:as) nf (nc) []
		     | isUpper a = scannerMayor (a:as) nf (nc) []
		     | isOp a = scannerOperador (a:as) nf nc
		     | esEspacio a = (scanner1 as nf (nc+1))
		     | isNl a =  (Simbolo NuevaLinea [a] nf nc):(scanner1 as (nf+1) 1)
		     | otherwise = error ("Simbolo "++ [a] ++ " no reconocido " ++ "en la linea " ++ (show nf)++ "y columna "++ show nc)


scannerCadena :: String -> Int -> Int -> String -> [Simbolo]
scannerCadena (a:as) nf nc c | a == '\"' = (Simbolo Cadena c nf nc):(scanner1 as nf (nc+1))
			     | a == '\n' = scannerCadena as (nf+1) (nc+1) (c++[a])
			     | isAscii a = scannerCadena as nf (nc+1) (c++[a])

scannerCaracter :: String -> Int -> Int -> [Simbolo]
scannerCaracter (a:x:as) nc nf | isAlpha a && x == '\'' = (Simbolo Caracter [a] nc nf): scanner1 as nf (nc+1)

scannerEntero :: String -> Int -> Int -> String -> [Simbolo]
scannerEntero []  nf nc c = [Simbolo Entero c nf nc]
scannerEntero (a:as) nf nc c | isDigit a = scannerEntero as nf (nc+1) (c++[a])
			     | otherwise = (Simbolo Entero c nf nc): scanner1 (a:as) nf nc  

scannerMenor :: String -> Int -> Int -> String -> [Simbolo]
scannerMenor []  nf nc c = if perteneceReserv c then [ Simbolo Reservada c nf nc] else [Simbolo IdMenor c nf nc]
scannerMenor (a:as) nf nc c | isAlpha a = scannerMenor as nf (nc+1) (c++[a])
		       	    | isDigit a = scannerMayor as nf (nc+1) (c++[a])
			    | otherwise = (if perteneceReserv c then Simbolo Reservada c nf nc else Simbolo IdMenor c nf nc):scanner1 (a:as) nf nc 

scannerMayor :: String -> Int -> Int -> String -> [Simbolo]
scannerMayor [] nf nc c = if perteneceBasico c then [Simbolo TipoBasico c nf nc] else if esBoleano c then [Simbolo Boleano c nf nc]  else [Simbolo IdMayor c nf nc]
scannerMayor (a:as) nf nc c | isAlpha a = scannerMayor as nf (nc+1) (c++[a])
			| isDigit a = scannerMayor as nf (nc+1) (c++[a])
              	        | otherwise = (if perteneceBasico c then Simbolo TipoBasico c nf nc else if esBoleano c then Simbolo Boleano c nf nc  else Simbolo IdMayor c nf nc):scanner1 (a:as) nf nc 

scannerOperador :: String -> Int -> Int -> [Simbolo]
scannerOperador [a] nf nc | isOp a = [Simbolo Operador [a] nf nc]
		          | otherwise = scanner1 [a] nf nc 
scannerOperador (a:b:as) nf nc | isOp a && isOp b  && a == b && (a == '+' || a == '&' || a == '=' || a == '|') = (Simbolo Operador (a:b:[]) nf nc):(scanner1 as nf (nc+2))
			       | isOp a && isOp b && a == b && a == ':' = (Simbolo OperadorEspecial (a:b:[]) nf nc):(scanner1 as nf (nc+2))
			       | a == '-' && b == '>' = (Simbolo OperadorEspecial (a:b:[]) nf nc):(scanner1 as nf (nc+2))
			       | (a == '>' && b =='=' )||(a == '<' && b =='=') = (Simbolo Operador (a:b:[]) nf nc):(scanner1 as nf (nc+2))
			       | a == '\\' && b == '=' = (Simbolo Operador (a:b:[]) nf nc):(scanner1 as nf (nc+2))
			       | a == '=' || a == '|' || a == '(' || a == ')' = (Simbolo OperadorEspecial (a:[]) nf nc):(scanner1 (b:as) nf (nc+1))
			       | isOp a = (Simbolo Operador (a:[]) nf nc):(scanner1 (b:as) nf nc)
			       | otherwise = scanner1 (a:b:as) nf nc
     			
isOp a = elem a simbolos
       where 
            simbolos = ['=','|','&','*','+','-','/','\\','>','<','(',')', ':']

isNl a = if a == '\n' then True else False


esEspacio a = if a == ' ' then True else False

perteneceBasico a = elem a basicas
		  where 
		  	basicas = ["String","Bool","Int","Char"]

perteneceReserv a  = elem a reservadas 
		   where 
		   	reservadas = ["module","where","let","in","data","case","of","do"]

esBoleano a = elem a ["True","False"]


