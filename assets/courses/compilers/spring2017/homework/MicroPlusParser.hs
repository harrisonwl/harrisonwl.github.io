module MicroPlusParser where

import Prelude hiding (EQ,LT,GT)

import MicroPlusAST
import Control.Monad

import System.IO
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

expression :: Parser Exp
expression = buildExpressionParser table term' <?> "expression"
table = [ [Prefix (reservedOp "~" >> return (UnaryOp Not))]
        , [Infix (reservedOp "&" >> return (BinOp And)) AssocLeft]
        , [Infix (reservedOp "==" >> return (BinOp EQ)) AssocLeft]
        , [Infix (reservedOp "*" >> return (BinOp Times)) AssocLeft]
        , [Infix (reservedOp "+" >> return (BinOp Plus)) AssocLeft]
        , [Infix (reservedOp "-" >> return (BinOp Sub)) AssocLeft]
        , [Infix (reservedOp "/" >> return (BinOp Div)) AssocLeft]
        , [Infix (reservedOp ">" >> return (BinOp GT)) AssocLeft]
        , [Infix (reservedOp "<" >> return (BinOp LT)) AssocLeft]
        ]

term' = parens expression
       <|> funcall
       <|> fmap Var identifier
       <|> fmap ICon integer
       <|> fmap FCon float
       <|> (reserved "true" >> return (BCon True))
       <|> (reserved "false" >> return (BCon False))

funcall = do
  reserved "call"
  f <- identifier
  args <- parens $ sepBy expression {-arguments-} comma
  return $ FunCall f args

-- the top level function for Cmd
cmdParser :: Parser Cmd
cmdParser = whiteSpace >> command

command :: Parser Cmd
command   =  parens command <|> sequenceOfCmd

sequenceOfCmd =
  do list <- (sepBy1 command' semi)
     -- If there's only one statement return it without using Seq.
     return $ if length list == 1 then head list else Seq list

command' :: Parser Cmd
command' =     iteCmd
           <|> ifthenCmd
           <|> whileCmd
           <|> skipCmd
           <|> returnCmd
           <|> assignCmd

returnCmd :: Parser Cmd
returnCmd = do
  reserved "return"
  e <- expression
  return (Return e)

iteCmd :: Parser Cmd
iteCmd =
  do reserved "if"
     cond  <- expression
     reserved "then"
     cmd1 <- command
     reserved "else"
     cmd2 <- command
     return $ IfThenElse cond cmd1 cmd2

ifthenCmd :: Parser Cmd
ifthenCmd =
  do reserved "if"
     cond  <- expression
     reserved "then"
     cmd <- command
     return $ IfThen cond cmd

whileCmd :: Parser Cmd
whileCmd =
  do reserved "while"
     cond <- expression
     reserved "do"
     cmd <- command
     return $ While cond cmd

assignCmd :: Parser Cmd
assignCmd =
  do var  <- identifier
     reservedOp ":="
     expr <- expression
     return $ Assign var expr

skipCmd :: Parser Cmd
skipCmd = reserved "skip" >> return Skip

--
-- Variable declarations
--

argdecls = sepBy decl comma
locdecls = do
  reservedOp "["
  ds <- sepBy decl semi
  reservedOp "]"
  return ds

decl = do
  x <- identifier
  reservedOp ":"
  t <- typename
  return $ Decl x t

typename =     (reserved "float" >> return FLOAT)
           <|> (reserved "int" >> return INT)
           <|> (reserved "bool" >> return BOOL)

--
-- Function declarations
--

fundecl = do
  rt <- typename
  f <- identifier
  argdecs <- parens argdecls
  reservedOp "{"
  locdecs <- (locdecls <|> return [])
  case locdecs of
   [] -> do body <- command
            reservedOp "}"
            return $ FunDecl rt f argdecs locdecs body
   _  -> do reservedOp ";"
            body <- command
            reservedOp "}"
            return $ FunDecl rt f argdecs locdecs body

program = do
  fds <- many1 fundecl
  return (Program fds)

---
--- top level parsing functions
---

string2exp :: String -> Exp
string2exp str =
  case parse expression "" str of
    Left e  -> error $ show e
    Right r -> r

string2cmd :: String -> Cmd
string2cmd str =
  case parse cmdParser "" str of
    Left e  -> error $ show e
    Right r -> r

string2fd :: String -> FunDecl
string2fd str =
  case parse fundecl "" str of
    Left e  -> error $ show e
    Right r -> r

string2ad :: String -> [Decl]
string2ad str =
  case parse argdecls "" str of
    Left e  -> error $ show e
    Right r -> r

string2prog :: String -> Program
string2prog str = 
  case parse program "" str of
    Left e  -> error $ show e
    Right r -> r

file2cmd :: String -> IO Cmd
file2cmd file =
  do prog  <- readFile file
     case parse cmdParser "" prog of
       Left e  -> print e >> fail "parse error"
       Right r -> return r

file2prog :: String -> IO Program
file2prog file =
  do prog  <- readFile file
     case parse program "" prog of
       Left e  -> print e >> fail "parse error"
       Right p -> return p

-- Just a test function
string2locdecls str =
  case parse (whiteSpace >> locdecls) "" str of
    Left e  -> error $ show e
    Right r -> r

----------
----------
----------
----------
languageDef =
  emptyDef { Token.commentStart    = "/*"
           , Token.commentEnd      = "*/"
           , Token.commentLine     = "//"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = [ "if"
                                     , "then"
                                     , "else"
                                     , "while"
                                     , "do"
                                     , "skip"
                                     , "true"
                                     , "false"
                                     , "not"
                                     , "and"
                                     , "or"
                                     ]
           , Token.reservedOpNames = ["+", "-", "*", "/", ":="
                                     , "<", ">", "and", "or", "not"
                                     ]
           }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
--                                      parens p
--                                    takes care of the parenthesis and
--                                    uses p to parse what's inside them
integer    = Token.integer    lexer -- parses an integer
float      = Token.float      lexer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace
symbol     = Token.symbol     lexer -- parses symbols
comma      = Token.comma      lexer -- parses symbols
