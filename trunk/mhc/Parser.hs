module Parser where
import UU.Parsing
import Scanner
import Datas

sem_Root_Root :: Module  -> IO ()
sem_Root_Root          = putStr . show . id
pRoot   =   sem_Root_Root <$> pModule


pModule = ModuleDB <$>  pModDecl <*> pModBody 
	<|> ModuleB <$> pModBody

pModDecl = ModDecl <$ pReservada "module" <*> pIdMayor <* pReservada "where" <* pNewLine <* pList pNewLine

-- pModBody = ModBodyDB <$> (pFoldr ((:),[])  pDataDecl) <*> (pFoldr ((:),[]) ((<+>) pTypeFunction pFunctionDecl))
pModBody = ModBodyDB <$> pDataDeclaration <* pNewLine <* pList pNewLine <*> pModBody
	<|> ModBodyFB <$> ((<+>) (pTypeFunction <* pNewLine <* pList pNewLine)  (pFunctionDeclaration)) <*> pModBody
	<|> pSucceed ModBodyS

pDataDeclaration = DataDeclaration <$ pReservada "data" <*> pIdMayor <* pOperadorEspecial "=" <*> pIdMayor <*> pList (pOperadorEspecial "|"  *> pIdMayor) 

pTypeFunction = TypeFunction <$> pIdMenor <* pOperadorEspecial "::" <*> pType <*> pFoldr ((:),[]) (pOperadorEspecial "->" *> pType) 

pFunctionDeclaration = FunctionDeclaration <$> pIdMenor <*> pFoldr ((:),[]) pIdMenor <* pOperadorEspecial "=" <*> pExpresion 

pExpresion = ExpLit <$> pLiteral <* pNewLine  <*  pList pNewLine 	
	<|> ExpLambda <$ pOperadorEspecial "(" <* pOperadorEspecial "\\" <*> pIdMenor <*> pList pIdMenor <* pOperadorEspecial "->" <*> pExpresion
	<|> ExpLet <$ pReservada "let" <*> pFunctionDeclaration <*> pList pFunctionDeclaration <* pReservada "in" <*> pExpresion
	<|> ExpCase <$ pReservada "case" <*> pIdMenor <* pReservada "of" <* pNewLine <*>  ((<+>) (pLiteral <* pOperadorEspecial "->")  (pExpresion <* pNewLine <* pList pNewLine)) <*> pList ((<+>) (pLiteral <* pOperadorEspecial "->")  (pExpresion <* pNewLine <* pList pNewLine))
	<|> ExpComp <$> pLiteral <*> pOperator <*> pExpresion
	<|> ExpFunction <$> pIdMenor <*> pList pParam <* pNewLine  <*  pList pNewLine
	
pType = TypeIMe <$> pIdMenor
	<|> TypeIMa <$> pIdMayor
	<|> TypeBas <$> pBasicType

pParam = ParamIM <$> pIdMenor
	<|> ParamLit <$> pLiteral 
	
pLiteral = LitInt <$> pEntero <|> LitString <$>  pCadena <|> LitBool <$> pBoleano <|> LitChar <$> pCaracter 

pOperator = Operator <$> pOperador
	    





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
		out
