module Datas  where

data Module = ModuleDB ModDecl ModBody | ModuleB ModBody deriving Show
data ModDecl = ModDecl String deriving Show
data ModBody = ModBodyDB DataDeclaration ModBody 
	| ModBodyFB (TypeFunction,FunctionDeclaration) ModBody
	| ModBodyS deriving Show
data DataDeclaration = DataDeclaration String String [String] deriving Show
data TypeFunction = TypeFunction String Type [Type] deriving Show
data FunctionDeclaration = FunctionDeclaration String [String] Expresion deriving Show
data Expresion = ExpLit Literal
	| ExpLambda  String [String] Expresion
	| ExpLet FunctionDeclaration [FunctionDeclaration] Expresion
	| ExpCase String (Literal,Expresion) [(Literal,Expresion)]
	| ExpComp Literal Operator  Expresion
	| ExpFunction String [Param] deriving Show
data Param = ParamIM String
	| ParamLit Literal deriving Show
data Type = TypeIMe String
	| TypeIMa String
	| TypeBas String deriving Show
data Literal = LitString String | LitBool String | LitInt String | LitChar String deriving Show
data Operator = Operator String deriving Show
	




