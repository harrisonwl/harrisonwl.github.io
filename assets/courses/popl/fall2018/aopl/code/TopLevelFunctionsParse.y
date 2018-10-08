{
module TopLevelFunctionsParse where
import Prelude hiding (LT, GT, EQ, id)
import Data.Char
import Value
import Operators
import TopLevelFunctions
import Lexer
}

%name parser
%tokentype { Token }

%token 
    fun    { TokenKeyword "function" }
    ','    { Symbol "," }
    '{'    { Symbol "{" }
    '}'    { Symbol "}" }
    if     { TokenKeyword "if" }
    else   { TokenKeyword "else" }
    true   { TokenKeyword "true" }
    false  { TokenKeyword "false" }
    var    { TokenKeyword "var" }
    ';'    { Symbol ";" }
    id     { TokenIdent $$ }
    digits { Digits $$ }
    '='    { Symbol "=" }
    '+'    { Symbol "+" }
    '-'    { Symbol "-" }
    '*'    { Symbol "*" }
    '/'    { Symbol "/" }
    '<'    { Symbol "<" }
    '>'    { Symbol ">" }
    '<='   { Symbol "<=" }
    '>='   { Symbol ">=" }
    '=='   { Symbol "==" }
    '&&'   { Symbol "&&" }
    '!'    { Symbol "!" }
    '||'   { Symbol "||" }
    '('    { Symbol "(" }
    ')'    { Symbol ")" }

%%

Program : Functions Exp      { Program $1 $2 }

Functions : Functions Function   { $1 ++ [$2] }
          |                      { [] }

Function : fun id '(' ids ')' '{' Exp '}'       { ($2, Function $4 $7) }
 
ids : ids ',' id     { $1 ++ [$3] }
    | id               { [$1] }
    |               { [] }
    
 -- all the rest is the same as IntBool.y
 
Exp : var id '=' Exp ';' Exp           { Declare $2 $4 $6 }
    | if '(' Exp ')' Exp else Exp  { If $3 $5 $7 }
    | Or                               { $1 }

Or   : Or '||' And        { Binary Or $1 $3 }
     | And                { $1 }

And   : And '&&' Comp      { Binary And $1 $3 }
     | Comp                { $1 }

Comp : Comp '==' Term     { Binary EQ $1 $3 }
     | Comp '<' Term      { Binary LT $1 $3 }
     | Comp '>' Term      { Binary GT $1 $3 }
     | Comp '<=' Term     { Binary LE $1 $3 }
     | Comp '>=' Term     { Binary GE $1 $3 }
     | Term               { $1 }

Term : Term '+' Factor    { Binary Add $1 $3 }
     | Term '-' Factor    { Binary Sub $1 $3 }
     | Factor             { $1 }

Factor : Factor '*' Primary    { Binary Mul $1 $3 }
       | Factor '/' Primary    { Binary Div $1 $3 }
       | Primary               { $1 }

Primary : digits         { Literal (IntV $1) }
        | true           { Literal (BoolV True) }
        | false          { Literal (BoolV False) }
        | '-' Primary    { Unary Neg $2 }
        | '!' Primary    { Unary Not $2 }
        | id             { Variable $1 }
        | id '(' Exps ')' { Call $1 $3 }
        | '(' Exp ')'    { $2 }

Exps  : Exps ',' Exp     { $1 ++ [$3] }
      | Exp              { [$1] }
      |                  { [] }

{

symbols = ["{", "}", ",", "+", "-", "*", "/", "(", ")", ";", "==", "=", "<=", ">=", "<", ">", "||", "&&", "!"]
keywords = ["function", "var", "if", "else", "true", "false"]
parseExp str = parser (lexer symbols keywords str)

parseInput = do
  input <- getContents
  print (parseExp input)

}
