module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
    If Expr.T Statement Statement|
    Skip|
    Begin [Statement]|
    While Expr.T Statement|
    Read String|
    Write Expr.T|
    Repeat Statement Expr.T
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

ifStatement = (accept "if" -# Expr.parse) # (require "then" -# parse) # (require "else" -# parse) >-> buildIf
buildIf ((b, t), e) = If b t e

skipStatement = accept "skip" # require ";" >-> buildSkip
buildSkip _ = Skip

beginStatement = accept "begin" -# iter parse #- require "end" >-> buildBegin
buildBegin l = Begin l

whileStatement = accept "while" -# Expr.parse # (require "do" -# parse) >-> buildWhile
buildWhile (b, s) = While b s

readStatement = accept "read" -# (word #- require ";") >-> buildRead
buildRead v = Read v

writeStatement = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite e = Write e

repeatStatement = accept "repeat" -# parse # (require "until" -# Expr.parse) #- require ";" >-> buildRepeat
buildRepeat (s, e) = Repeat s e

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []

exec (Assignment id expr: stmts) dict input =
    exec stmts (Dictionary.insert (id, (Expr.value expr dict)) dict) input


exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

exec (Skip: stmts) dict input = 
    exec stmts dict input

exec (While cond stmt: stmts) dict input = 
    if (Expr.value cond dict)>0
    then exec ([stmt] ++ [(While cond stmt)] ++ stmts) dict input
    else exec stmts dict input


exec (Begin bStmts: stmts) dict input = 
    exec (bStmts++stmts) dict input

exec (Read id: stmts) dict (input:inputs) = 
    exec stmts (Dictionary.insert (id, input) dict) inputs

exec (Write expr: stmts) dict input = 
    (Expr.value expr dict) : (exec stmts dict input)

exec (Repeat stmt cond: stmts) dict input =
    exec (stmt : (If cond (Skip) (Repeat stmt cond):stmts)) dict input

instance Parse Statement where
  --parse = error "Statement.parse not implemented"
  parse = assignment ! ifStatement ! skipStatement ! beginStatement ! whileStatement ! readStatement ! writeStatement ! repeatStatement
  toString (Assignment id expr) = id ++ " := " ++ Expr.toString expr ++ ";\n"
  toString (If cond thenStmts elseStmts) = "if " ++ Expr.toString cond ++ " then\n" ++ toString thenStmts ++ "else\n" ++ toString elseStmts
  toString (Skip) = "skip;\n"
  toString (While cond stmt) = "while " ++ Expr.toString cond ++ " do\n" ++ toString stmt
  toString (Begin l) = "begin\n" ++ listToString l ++ "end\n"
  toString (Read id) = "read " ++ id ++ ";\n"
  toString (Write expr) = "write " ++ Expr.toString expr ++ ";\n"
  toString (Repeat stmt cond) = "repeat\n" ++ toString stmt ++ "until " ++ Expr.toString cond ++ ";\n"


listToString :: [Statement] -> String
listToString [] = ""
listToString (x:xs) = toString x ++ listToString xs
