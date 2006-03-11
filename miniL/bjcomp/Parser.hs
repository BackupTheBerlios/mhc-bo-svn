
module Parser where

import UU.Parsing
import Scanner
import Data.Char

sem_Root :: String -> IO ()
sem_Root = putStr . show . id
pRoot = sem_Root <$> pIExpr <* pOperator ";" -- <* pSpace 
	<|> sem_Root <$> pIExpr <* pList  pSpace <* pOperator ";"
{--
pPrograma = () <$> pEExpre <* pKeyWord ";"

pEExpre = () <$> pLExpr 
	<|> () <$> pAExpr 
	<|> () <$> pBExpr

pRExpr = () <$> pAExpr
	<|> () <$> pBExpr 
	<|> () <$> pApli
--}
pIExpr = (\a -> show a) <$> pAExpr
        <|> (\a -> map toUpper (show a)) <$> pBExpr 
      --  <|> () <$> pApli
		<|> (\a -> show a) <$> pCExpr
		-- <|> (\a -> a) <$> pFExpr
{--
-- Application --

pApl = () <$> pId <* pOperator "(" <*> pArgs <* pOperator ")"

pArgs = () <$> pList pArg

pArg = () <$> pBExpr
	<|> () <$> pCExpr

-- Functional expresion --

pFExpr = () <$> pOperator "\\" *> pList pId <* pOpertator "->" <*> pIExpr
--}
-- Boolean expresion --

pBExpr = (\a -> a) <$> pBTerm
	<|> (\a -> a) <$ pKeyWord "NOT"<* pList pSpace <*> pBFactor
	<|> (\a b -> not a && b) <$ pKeyWord "NOT" <* pList pSpace <*> pBFactor <* pList pSpace <*  pKeyWord "AND" <* pList pSpace <*> pBExpr
    <|> (\a b -> not a || b) <$ pKeyWord "NOT" <* pList pSpace <*> pBFactor <* pList pSpace <*  pKeyWord "OR" <* pList pSpace <*> pBExpr


pBTerm = (\a b -> a && b) <$> pBFactor <* pList pSpace <* pKeyWord "AND" <* pList pSpace <*> pBExpr
	<|>	(\a b -> a || b) <$> pBFactor <* pList pSpace <* pKeyWord "OR" <* pList pSpace <*> pBExpr

pBFactor = pBConst

-- Extra expresion for Relational and Aritmetic expresion --


-- Relational expresion --

pRELExpr = (\a b -> (==) a b) <$> pAExpr <* pList pSpace <* pOperator "==" <* pList pSpace <*> pAExpr 
	<|>  (\a  b -> (<=) a b) <$> pAExpr <* pList pSpace <* pOperator "<=" <* pList pSpace <*> pAExpr

-- Conditional expresion --

pCExpr = (\a b c -> if a then b else c) <$ pKeyWord "IF" <*> pBExpr <* pKeyWord 
										"THEN" <*> pIExpr <* pKeyWord "ELSE" <*> pIExpr

{-- Let expresion -- 

pLExpr = () <$ pKeyWord "LET" <*> pDecls <* pKeyWord "IN" <*> pRExpr

pDecls = () <$> pList pDecl

pDecl = () <$> pId <* pOperator "=" <*> pIExpr
--}

--Aritmetic expresions --

pAExpr = pTerm 
--	<|> (\a c -> (-) a c) <$> pTerm <* pList pSpace <* pOperator "-" <* pList pSpace <*> pAExpr
	<|> (\a c -> (+) a c) <$> pTerm <* pList pSpace <* pOperator "+" <* pList pSpace <*> pAExpr 

pTerm = pFactor
	<|> (\a c -> div a c) <$> pFactor <* pList pSpace <* pOperator "/" <* pList pSpace <*> pTerm	
	<|> (\a c -> (*) a c) <$> pFactor <* pList pSpace <* pOperator "*" <* pList pSpace <*> pTerm

pFactor = pAconst -- <|> pId <|> pApl
	<|> (\a -> a) <$ pOperator "(" <*> pAExpr <* pOperator ")"

pAconst :: Parser Simbolo Int
pAconst =  (\(Simbolo Integr str) -> read str) <$> pSym (Simbolo Integr "")

pBConst :: Parser Simbolo Bool
pBConst = (\(Simbolo Boolean str) -> f str) <$> pSym (Simbolo Boolean "")
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
