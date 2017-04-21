module MicroPlusParser where

import Prelude hiding (EQ,LT,GT)

import MicroPlusAST
-- import System.IO
-- import Control.Monad
-- import Text.ParserCombinators.Parsec
-- import Text.ParserCombinators.Parsec.Expr
-- import Text.ParserCombinators.Parsec.Language
-- import qualified Text.ParserCombinators.Parsec.Token as Token

import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language



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

exprparser :: Parser Exp
exprparser = buildExpressionParser table term <?> "expression"
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

term = m_parens exprparser
       <|> fmap Var m_identifier
       <|> fmap ICon m_integer
       <|> fmap FCon m_float
       <|> (m_reserved "true" >> return (BCon True))
       <|> (m_reserved "false" >> return (BCon False))

expr :: String -> IO ()
expr inp = case parse exprparser "" inp of
             { Left err -> print err
             ; Right ans -> print ans
             }

cmdParser = whiteSpace >> command

--command :: Parser Cmd
--command   =  parens command <|> sequenceOfCmd

sequenceOfCmd = undefined

{-

exprparser :: Parser Exp
exprparser = buildExpressionParser table term' <?> "expression"
table = [ [Prefix (m_reservedOp "~" >> return (UnaryOp Neg))]
          , [Infix  (m_reservedOp "/"   >> return (BinOp Div  )) AssocLeft]
          , [Infix  (m_reservedOp ">"   >> return (BinOp GT)) AssocLeft]
          , [Infix  (m_reservedOp "<"   >> return (BinOp LT)) AssocLeft]
          , [Infix  (m_reservedOp "=="  >> return (BinOp EQ)) AssocLeft]
          , [Infix  (m_reservedOp "and" >> return (BinOp And     )) AssocLeft]
          , [Infix  (m_reservedOp "or"  >> return (BinOp Or      )) AssocLeft]
        ]

term' = parens exprparser
       <|> fmap Var identifier
       <|> (m_reserved "true" >> return (BCon True))
       <|> (m_reserved "false" >> return (BCon False))

expr :: String -> IO ()
expr inp = case parse exprparser "" inp of
             { Left err -> print err
             ; Right ans -> print ans
             }

-}

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

lexer = Token.makeTokenParser languageDef
-}
