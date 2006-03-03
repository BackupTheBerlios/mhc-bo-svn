
module Parser where

import UU.Parsing
import Scanner
import Data.Char

sem_Root :: Int -> IO ()
sem_Root = putStr . show . id
pRoot = sem_Root <$> pAExpr <* pOperator ";" -- <* pSpace 
	<|> sem_Root <$> pAExpr <* pList  pSpace <* pOperator ";"
{--
pPrograma = () <$> pEExpre <* pKeyWord ";"

pEExpre = () <$> pLExpr 
	<|> () <$> pAExpr 
	<|> () <$> pBExpr

pRExpr = () <$> pAExpr
	<|> () <$> pBExpr 
	<|> () <$> pApli

pIExpr = () <$> pAExpr
        <|> () <$> pBExpr 
        <|> () <$> pApli
	<|> () <$> pCExpr
	<|> () <$> pFExpr

-- Application --

pApl = () <$> pId <* pOperator "(" <*> pArgs <* pOperator ")"

pArgs = () <$> pList pArg

pArg = () <$> pBExpr
	<|> () <$> pCExpr

-- Functional expresion --

pFExpr = () <$> pOperator "\\" *> pList pId <* pOpertator "->" <*> pIExpr

-- Boolean expresion --

pBExpr = () <$> pRELExpr 
	<|> () <$> pBCons
	<|> () <$> pId
	<|> () <$> pKeyWord "NOT"  *> pBExpr
	<|> () <$> pBExpr <* pKeyWord "AND" <*> pBExpr
	<|> () <$> pBExpr <*> pKeyWord "OR" <*> pBExpr

-- Relational expresion --

pRELExpr = () <$> pAExpr <* pOperator "==" <*> pAExpr 
	<|>  () <$> pAExpr <* pOPerator "<=" <*> pAExpr

-- Conditional expresion --

pCExpre = () <$ pKewyWord "IF" <*> pBExpr <* pKeyWord "THEN" <*> pIExpr <* pKeyWord "ELSE" <*> pIExpr

-- Let expresion -- 

pLExpr = () <$ pKeyWord "LET" <*> pDecls <* pKeyWord "IN" <*> pRExpr

pDecls = () <$> pList pDecl

pDecl = () <$> pId <* pOperator "=" <*> pIExpr

--}

--Aritmetic expresions --

pAExpr = pTerm 
	<|> (\a c -> (-) a c) <$> pTerm <* pList pSpace <* pOperator "-" <* pList pSpace <*> pAExpr
	<|> (\a c -> (+) a c) <$> pTerm <* pList pSpace <* pOperator "+" <* pList pSpace <*> pAExpr 

pTerm = pFactor
	<|> (\a c -> (*) a c) <$> pFactor <* pList pSpace <* pOperator "*" <* pList pSpace <*> pTerm
	<|> (\a c -> div a c) <$> pFactor <* pList pSpace <* pOperator "/" <* pList pSpace <*> pTerm	

pFactor = pAconst -- <|> pId <|> pApl
	<|> (\a -> a) <$ pOperator "(" <*> pAExpr <* pOperator ")"

pAconst :: Parser Simbolo Int
pAconst =  (\(Simbolo Integer str) -> read str) <$> pSym (Simbolo Integer "")

pBConst :: Parser Simbolo Bool
pBConst = (\(Simbolo Bool str) -> f str) <$> pSym (Simbolo Bool "")
	where f a = if a == "TRUE" then True else False	

pOperator :: String -> Parser Simbolo String
pOperator str2 =  (\(Simbolo Operator str) -> str) <$> pSym (Simbolo Operator str2)

pSpace :: Parser Simbolo String 
pSpace = (\(Simbolo Space str) -> str) <$> pSym (Simbolo Space " ")

pKeyWord :: String -> Parser Simbolo String
pKeyWord str = (\(Simbolo KeyWord str2)-> str2) <$> pSym (Simbolo KeyWord str)

pId :: Parser Simbolo String
pId = (\(Simbolo Id str) -> str) <$> pSym (Simbolo Id "")

---Interprete ---

main = do
	putStr "\nInterprete MiniL  "
	a <- getLine
	f a
f a = if a /= [] then (if  (last a) == ';' then g a else h a) else h a

g a = do 
	out <- parseIO pRoot (scanner a)
	out
	main

h a = do
	b <- getLine 
	f (a++b)

compilar as = do
	archivo <- readFile as
	out <- parseIO pRoot (scanner archivo)
	out
	putStr "\n"
