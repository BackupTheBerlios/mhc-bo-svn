module Main where
import UU.Parsing
import Scanner
import Datas

---Funciones semanticas --
sem_Module_1 = Mod1
sem_Module_2 = Mod2
sem_Module_Decl =ModDec
sem_Module_Body_1 = ModBod1
sem_Module_Body_2 = ModBod2
sem_Data_Constructor = DatCons
sem_Data_Decl = DatDecl
sem_Type_Function = TyFunc
sem_Basic_Type = BasTyp
sem_Expresion_1 = Exp1 
sem_Expresion_2 = Exp2 
sem_Expresion_3 = Exp3
sem_Expresion_4 = Exp4
sem_Expresion_Lam = Exp5
sem_Expresion_0 = Exp0
sem_Func_Decla = FunDel
sem_Param_May = ParMay
sem_Param_Men = ParMen
sem_Param_Op = ParOpe
sem_Param_Lit = ParLit
sem_Type_Ma = TyMa
sem_Type_Me = TyMe
sem_Type_Ba = TyBa
sem_Operador = Op
sem_Literal = Lite
sem_Params = []
sem_Types = []

-------------------------------------------------
sem_Root_Root :: Module  -> IO ()
sem_Root_Root          = putStr . show . id
pRoot   =   sem_Root_Root <$> pModule

pModule = sem_Module_1 <$> pModuleDeclaration <*> pModuleBody
	<|> sem_Module_2 <$> pModuleBody


pModuleDeclaration = sem_Module_Decl <$ pReservada "module" <*> pIdMayor <* pReservada "where" 

pModuleBody = sem_Module_Body_1 <$> pList pDataDeclaration <*> pTypeFunctions <*> pList pFunctionDeclaration
--	<|> sem_Module_Body_2 <$> pTypeFunctions <*> pFunctionDeclarations <*> pDataDeclarations

pDataDeclaration = sem_Data_Decl <$ pReservada "data" <*> pIdMayor <* pOperadorEspecial "=" <*> pDataConstructor 

pFunctionDeclarations = list <$> pFunctionDeclaration <*> pFunctionDeclarations
	<|> pSucceed []

pFunctionDeclaration = sem_Func_Decla <$> pIdMenor <*> pList pParam<* pOperadorEspecial "=" <*> pExpresion  

pTypeFunctions = list <$> pTypeFunction <*> pTypeFunctions
	<|> pSucceed []

pTypeFunction = sem_Type_Function <$> pIdMenor <* pOperadorEspecial "::" <*> pType <*> pTypeFunctionAux  
pTypeFunctionAux = list <$ pOperadorEspecial "->" <*> pType <*> pTypeFunctionAux 
		<|> pSucceed []

		
pDataConstructor = sem_Data_Constructor <$> pIdMayor <*> pDataConstructorAux 
pDataConstructorAux = list <$ pOperadorEspecial "|" <*>  pIdMayor <*>pDataConstructorAux 
		    <|> pSucceed []

pExpresion = sem_Expresion_1 <$> pLiteral 
--	<|> pLetExpresion
--	<|> sem_Expresion_0 <$> pLiteral <*>pOperador <*> pLiteral
--	<|> sem_Expresion_2 <$> pParam <*> pLiteral <*> pExpresion
	-- <|> sem_Expresion_3 <$ pReservada "let" <*>pFunctionDeclaration <*> pFunctionDeclarations <* pReservada "in" <*> pExpresion
	<|> sem_Expresion_4 <$> pLiteral <*> pOperador <*> pExpresion
-- 	<|> sem_Expresion_Lam <$ pOperadorEspecial "(" <* pOperadorEspecial "\\" <*> pList pParam <* pOperadorEspecial "->" <*> pExpresion <* pOperadorEspecial ")"

pLetExpresion = sem_Expresion_3 <$ pReservada "let" <*> pFunctionDeclaration <*> pFunctionDeclarations <* pReservada "in" <*> pExpresion 


pNuevaLineas = list <$> pNuevaLinea <*> pSucceed []
	<|> list <$> pNuevaLinea <*> pNuevaLineas
--	<|> (\a ->[a]) <$>pNuevaLinea

---Falta Operadores COMPLETAR--

pParam = sem_Param_May <$> pIdMayor 
	<|> sem_Param_Men <$> pIdMenor  

pType = sem_Type_Ma <$> pIdMayor 
	<|> sem_Type_Me <$> pIdMenor 
	<|> sem_Type_Ba <$> pTipoBasico
--	<|> sem_Type_Bool <$> pBoleano
sem_Type_Bool = TyBool

pBasicType = sem_Basic_Type<$> pTipoBasico 
pLiteral :: Parser Simbolo Literal
pLiteral = sem_Literal <$>  (pEntero  <|> pCadena <|> pCaracter)--  <|> pBoleano)

--Parsers inyectores--
--pLiteralAux = (\(Entero str _ _) -> str) <$> pSym (Entero "" 0 0)
--	<|> (\(Cadena str _ _) -> str) <$> pSym (Cadena "" 0 0)
--	<|> (\(Boleano str _ _) -> str) <$> pSym (Boleano "" 0 0)
--	<|> (\(Caracter chr _ _) -> [chr]) <$> pSym (Caracter 'x' 0 0)

pNuevaLinea :: Parser Simbolo String
pNuevaLinea = (\(NuevaLinea str _ _) -> str ) <$> pSym (NuevaLinea "" 0 0)
pEntero :: Parser Simbolo String
pEntero = (\(Entero str _ _) -> str) <$> pSym (Entero "" 0 0)
pCadena :: Parser Simbolo String
pCadena = (\(Cadena str _ _) -> str) <$> pSym (Cadena "" 0 0)
pCaracter :: Parser Simbolo String
pCaracter = (\(Caracter chr _ _) -> [chr]) <$> pSym (Caracter ' ' 0 0)
pTipoBasico :: Parser Simbolo String
pTipoBasico  = (\(TipoBasico str _ _) -> str) <$> pSym (TipoBasico "" 0 0)
pBoleano :: Parser Simbolo String
pBoleano = (\(Boleano str _ _) -> str) <$> pSym (Boleano "" 0 0)
pReservada str = (\(Reservada x _ _) -> x) <$> pSym (Reservada str 0 0)
pOperadorEspecial str = (\(OperadorEspecial x _ _) -> x) <$> pSym (OperadorEspecial str 0 0)
pIdMayor :: Parser Simbolo String
pIdMayor = (\(IdMayor str _ _) -> str) <$> pSym (IdMayor "" 0 0)
pIdMenor :: Parser Simbolo String
pIdMenor = (\(IdMenor str _ _) -> str) <$> pSym (IdMenor "" 0 0)
pEspacio :: Parser Simbolo String
pEspacio = (\(Espacio chr _ _) -> [chr]) <$> pSym (Espacio ' ' 0 0)
pOperador :: Parser Simbolo String
pOperador = (\(Operador x _ _) -> x)<$> pSym(Operador "" 0 0)
--many    :: Parser s a  -> Parser s [a]
--many p  =  list <$> p <*> many p <|> pSucceed []
--many1    :: Parser s a -> Parser s [a]
--many1 p  =  list <$> p <*> many p

pExpresionSeq = (\exprs -> case exprs of
			[e] -> [e])
			
list :: a -> [a] -> [a]
list x xs  =  x:xs

main = do compilar "P2.hs"

compilar as  = do 
		archivo <- readFile as
		out <- parseIO pRoot (scanner archivo 1 0)
		out
