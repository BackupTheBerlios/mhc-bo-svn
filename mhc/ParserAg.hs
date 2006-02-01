module Parser where
import UU.Parsing
import Scanner
import Datas
import AST

pRoot   =   sem_Root_Root <$> pModule


pModule = sem_Module_ModuleDB <$>  pModDecl <*> pModBody 
	<|> sem_Module_ModuleB <$> pModBody

pModDecl = sem_ModDecl_ModDecl <$ pReservada "module" <*> pIdMayor <* pReservada "where" <* pNewLine <* pList pNewLine

-- pModBody = ModBodyDB <$> (pFoldr ((:),[])  pDataDecl) <*> (pFoldr ((:),[]) ((<+>) pTypeFunction pFunctionDecl))
pModBody = sem_ModBody_ModBodyDB <$> pDataDeclaration <* pNewLine <* pList pNewLine <*> pModBody
	<|> sem_ModBody_ModBodyFB <$> ((<$>)sem_FuncTyDecl_Tuple  (pTypeFunction <* pNewLine <* pList pNewLine) <*>(pFunctionDeclaration)) <*> pModBody
	<|> pSucceed sem_ModBody_ModBodyS

pDataDeclaration = sem_DataDeclaration_ConsDataDeclaration <$ pReservada "data" <*> pIdMayor <* pOperadorEspecial "=" <*> pIdMayor <*> pFoldr (sem_StringList_Cons,sem_StringList_Nil) (pOperadorEspecial "|"  *> pIdMayor) 

pTypeFunction = sem_TypeFunction_TypeFunction <$> pIdMenor <* pOperadorEspecial "::" <*> pType <*> pFoldr (sem_TypeList_Cons,sem_TypeList_Nil) (pOperadorEspecial "->" *> pType) 

pFunctionDeclaration = sem_FunctionDeclaration_FunctionDeclaration <$> pIdMenor <*> pFoldr (sem_StringList_Cons,sem_StringList_Nil) pIdMenor <* pOperadorEspecial "=" <*> pExpresion 

pExpresion = sem_Expresion_ExpLit <$> pLiteral <* pNewLine  <*  pList pNewLine 	
	<|> sem_Expresion_ExpLambda <$ pOperadorEspecial "(" <* pOperadorEspecial "\\" <*> pIdMenor <*> pFoldr (sem_StringList_Cons,sem_StringList_Nil) pIdMenor <* pOperadorEspecial "->" <*> pExpresion
	<|> sem_Expresion_ExpLet <$ pReservada "let" <*> pFunctionDeclaration <*> pFoldr (sem_FunctionDeclarationList_Cons,sem_FunctionDeclarationList_Nil) pFunctionDeclaration <* pReservada "in" <*> pExpresion
	<|> sem_Expresion_ExpCase <$ pReservada "case" <*> pIdMenor <* pReservada "of" <* pNewLine <*>  ((<$>) sem_LitExp_Tuple  (pLiteral <* pOperadorEspecial "->") <*> (pExpresion <* pNewLine <* pList pNewLine)) <*> pFoldr (sem_LitExpList_Cons,sem_LitExpList_Nil) ((<$>) sem_LitExp_Tuple (pLiteral <* pOperadorEspecial "->") <*>  (pExpresion <* pNewLine <* pList pNewLine))
	<|> sem_Expresion_ExpComp <$> pLiteral <*> pOperator <*> pExpresion
	<|> sem_Expresion_ExpFunction <$> pIdMenor <*> pFoldr (sem_ParamList_Cons,sem_ParamList_Nil) pParam <* pNewLine  <*  pList pNewLine
	
pType = sem_Typee_TypeIMe <$> pIdMenor
	<|> sem_Typee_TypeIMa <$> pIdMayor
	<|> sem_Typee_TypeBas <$> pBasicType

pParam = sem_Param_ParamIM <$> pIdMenor
	<|> sem_Param_ParamLit <$> pLiteral 
	
pLiteral = sem_Literal_LitInt <$> pEntero <|> sem_Literal_LitString <$>  pCadena <|> sem_Literal_LitBool <$> pBoleano <|> sem_Literal_LitChar <$> pCaracter 

pOperator = sem_Operator_Operator  <$> pOperador
	    





--Parsers inyectores--


pNewLine :: Parser Simbolo String
pNewLine = (\(Simbolo NuevaLinea str x y) -> str ) <$> pSym (Simbolo NuevaLinea "" 0 0)
pEntero :: Parser Simbolo String
pEntero = (\(Simbolo Entero str x y) -> str) <$> pSym (Simbolo Entero "" 0 0)
pCadena :: Parser Simbolo String
pCadena = (\(Simbolo Cadena str x y) -> str) <$> pSym (Simbolo Cadena "" 0 0)
pCaracter :: Parser Simbolo String
pCaracter = (\(Simbolo Caracter str x y) -> str) <$> pSym (Simbolo Caracter "" 0 0)
pBasicType :: Parser Simbolo String
pBasicType  = (\(Simbolo TipoBasico str x y) -> str) <$> pSym (Simbolo TipoBasico "" 0 0)
pBoleano :: Parser Simbolo String
pBoleano = (\(Simbolo Boleano str x y) -> str) <$> pSym (Simbolo Boleano "" 0 0)
pReservada str = (\(Simbolo Reservada str2 x y) -> x) <$> pSym (Simbolo Reservada str 0 0)
pOperadorEspecial str = (\(Simbolo OperadorEspecial str2 x y) -> str) <$> pSym (Simbolo OperadorEspecial str 0 0)
pIdMayor :: Parser Simbolo String
pIdMayor = (\(Simbolo IdMayor str x y) -> str) <$> pSym (Simbolo IdMayor "" 0 0)
pIdMenor :: Parser Simbolo String
pIdMenor = (\(Simbolo IdMenor str x y) -> str) <$> pSym (Simbolo IdMenor "" 0 0)
pEspacio :: Parser Simbolo String
pEspacio = (\(Simbolo Espacio str x y) -> str) <$> pSym (Simbolo Espacio "" 0 0)
pOperador :: Parser Simbolo String
pOperador = (\(Simbolo Operador str x y) -> str)<$> pSym(Simbolo Operador "" 0 0)
--many    :: Parser s a  -> Parser s [a]
--many p  =  list <$> p <*> many p <|> pSucceed []
--many1    :: Parser s a -> Parser s [a]
--many1 p  =  list <$> p <*> many p
list x xs  =  x:xs

main = do compilar "P2.hs"

compilar as  = do 
		archivo <- readFile as
		out <- parseIO pRoot (scanner archivo 1 0)
		snd (out [])
