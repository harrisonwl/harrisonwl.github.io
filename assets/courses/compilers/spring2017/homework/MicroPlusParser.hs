module MicroPlusParser where

import Prelude hiding (EQ,LT,GT)

import MicroPlusAST
import Control.Monad
{-
import System.IO
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
-}
import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token


def = emptyDef{ commentStart = "{-"
              , commentEnd = "-}"
              , identStart = letter
              , identLetter = alphaNum
              , opStart = oneOf "~&*+-/><="
              , opLetter = oneOf "~&*+-/><="
              , reservedOpNames = ["~", "&", "*", "+", "-", "/", ">", "<", "="]
              , reservedNames = ["true", "false", "nop",
                                 "if", "then", "else", "fi",
                                 "while", "do", "od"]
              }

TokenParser{ parens     = m_parens
           , identifier = m_identifier
           , integer    = m_integer
           , float      = m_float
           , reservedOp = m_reservedOp
           , reserved   = m_reserved
           , semiSep1   = m_semiSep1
           , whiteSpace = m_whiteSpace } = makeTokenParser def

expression :: Parser Exp
expression = buildExpressionParser table term' <?> "expression"
table = [ [Prefix (m_reservedOp "~" >> return (UnaryOp Not))]
        , [Infix (m_reservedOp "&" >> return (BinOp And)) AssocLeft]
        , [Infix (m_reservedOp "==" >> return (BinOp EQ)) AssocLeft]
        , [Infix (m_reservedOp "*" >> return (BinOp Times)) AssocLeft]
        , [Infix (m_reservedOp "+" >> return (BinOp Plus)) AssocLeft]
        , [Infix (m_reservedOp "-" >> return (BinOp Sub)) AssocLeft]
        , [Infix (m_reservedOp "/" >> return (BinOp Div)) AssocLeft]
        , [Infix (m_reservedOp ">" >> return (BinOp GT)) AssocLeft]
        , [Infix (m_reservedOp "<" >> return (BinOp LT)) AssocLeft]
        ]

term' = m_parens expression
       <|> fmap Var m_identifier
       <|> fmap ICon m_integer
       <|> fmap FCon m_float
       <|> (m_reserved "true" >> return (BCon True))
       <|> (m_reserved "false" >> return (BCon False))

expr :: String -> IO ()
expr inp = case parse expression "" inp of
             { Left err -> print err
             ; Right ans -> print ans
             }


-----------
{-
operators = [ [Prefix (reservedOp "-"   >> return (UnaryOp Neg             ))          ]
            , [Infix  (reservedOp "*"   >> return (BinOp Times)) AssocLeft]
            , [Infix  (reservedOp "+"   >> return (BinOp Plus     )) AssocLeft]
            , [Infix  (reservedOp "-"   >> return (BinOp Sub)) AssocLeft]
            , [Prefix (reservedOp "not" >> return (UnaryOp Not             ))          ]
            , [Infix  (reservedOp "/"   >> return (BinOp Div  )) AssocLeft]
            , [Infix  (reservedOp ">"   >> return (BinOp GT)) AssocLeft]
            , [Infix  (reservedOp "<"   >> return (BinOp LT)) AssocLeft]
            , [Infix  (reservedOp "=="  >> return (BinOp EQ)) AssocLeft]
            , [Infix  (reservedOp "and" >> return (BinOp And     )) AssocLeft]
            , [Infix  (reservedOp "or"  >> return (BinOp Or      )) AssocLeft]
             ]

expression = buildExpressionParser operators term
-}

funcall = do
  f <- m_identifier
--  m_symbol "("
  args <- parens $ sepBy arguments comma
--  m_symbol ")"
  return $ FunCall f args

arguments =     (liftM Var identifier)
            <|> (liftM ICon integer)
            <|> liftM FCon float
            <|> (m_reserved "true"  >> return (BCon True ))
            <|> (m_reserved "false" >> return (BCon False))

term =  parens expression
     <|> funcall
     <|> liftM Var identifier
     <|> liftM ICon integer
     <|> liftM FCon float
     <|> (m_reserved "true"  >> return (BCon True ))
     <|> (m_reserved "false" >> return (BCon False))

-- the top level function for Cmd
--cmdParser :: Parser Cmd
cmdParser = whiteSpace >> command

--command :: Parser Cmd
command   =  parens command <|> sequenceOfCmd

sequenceOfCmd = undefined
{-
sequenceOfCmd =
  do list <- (sepBy1 command' semi)
     -- If there's only one statement return it without using Seq.
     return $ if length list == 1 then head list else Seq list
-}

command' :: Parser Cmd
command' =   iteCmd
           <|> ifthenCmd
           <|> whileCmd
           <|> skipCmd
           <|> returnCmd
           <|> assignCmd

returnCmd :: Parser Cmd
returnCmd = do
  m_reserved "return"
  e <- expression
  return (Return e)

iteCmd :: Parser Cmd
iteCmd =
  do m_reserved "if"
     cond  <- expression
     m_reserved "then"
     cmd1 <- command
     m_reserved "else"
     cmd2 <- command
     return $ IfThenElse cond cmd1 cmd2

ifthenCmd :: Parser Cmd
ifthenCmd =
  do m_reserved "if"
     cond  <- expression
     m_reserved "then"
     cmd <- command
     return $ IfThen cond cmd

whileCmd :: Parser Cmd
whileCmd =
  do m_reserved "while"
     cond <- expression
     m_reserved "do"
     cmd <- command
     return $ While cond cmd

assignCmd :: Parser Cmd
assignCmd =
  do var  <- m_identifier
     m_reservedOp ":="
     expr <- expression
     return $ Assign var expr

skipCmd :: Parser Cmd
skipCmd = m_reserved "skip" >> return Skip

--
-- Variable declarations
--

argdecls = undefined
locdecls = undefined
{-
argdecls = sepBy decl comma
locdecls = do
  m_reservedOp "["
  ds <- sepBy decl semi
  m_reservedOp "]"
  return ds
-}

decl = do
  x <- m_identifier
  m_reservedOp ":"
  t <- typename
  return $ Decl x t

typename =     (m_reserved "float" >> return FLOAT)
           <|> (m_reserved "int" >> return INT)
           <|> (m_reserved "bool" >> return BOOL)

--
-- Function declarations
--
fundecl = undefined

{-
fundecl = do
  rt <- typename
  f <- identifier
--  symbol "("
  argdecs <- parens argdecls
--  symbol ")"
  m_reservedOp "{"
  locdecs <- locdecls
  case locdecs of
   [] -> do body <- command
            m_reservedOp "}"
            return $ FunDecl rt f argdecs locdecs body
   _  -> do m_reservedOp ";"
            body <- command
            m_reservedOp "}"
            return $ FunDecl rt f argdecs locdecs body
-}

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

-- Just a test function
file2cmd :: String -> IO Cmd
file2cmd file =
  do program  <- readFile file
     case parse cmdParser "" program of
       Left e  -> print e >> fail "parse error"
       Right r -> return r

{-
-- Just a test function
string2locdecls str =
  case parse (whiteSpace >> locdecls) "" str of
    Left e  -> error $ show e
    Right r -> r
-}

----------
----------
----------
----------
{-
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
-}

--lexer = Token.makeTokenParser languageDef

--identifier = Token.identifier lexer -- parses an identifier
--reserved   = Token.reserved   lexer -- parses a reserved name
--reservedOp = Token.reservedOp lexer -- parses an operator
--parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
--integer    = Token.integer    lexer -- parses an integer
--float      = Token.float      lexer
--semi       = Token.semi       lexer -- parses a semicolon
--whiteSpace = Token.whiteSpace lexer -- parses whitespace
--symbol     = Token.symbol     lexer -- parses symbols
--comma      = Token.comma      lexer -- parses symbols
