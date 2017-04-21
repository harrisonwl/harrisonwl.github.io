import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
{-
== Sample grammar ==

Here is the expression grammar:

 ''expr''  ::= ''var'' | ''const'' | ( ''expr'' ) | ''unop'' ''expr'' | ''expr'' ''duop'' ''expr''
 ''var''   ::= ''letter'' { ''letter'' | ''digit'' }<sup>*</sup>
 ''const'' ::= true | false
 ''unop''  ::= ~
 ''duop''  ::= &amp; | =

Operator precedence from high to low: <tt>~</tt>, <tt>&amp;</tt>, <tt>=</tt>. Both binary operators are left-associative.

Here is the statement grammar:

 ''stmt'' ::= nop | ''var'' := ''expr'' | if ''expr'' then ''stmt'' else ''stmt'' fi | while ''expr'' do ''stmt'' od
        | ''stmt'' { ; ''stmt'' }<sup>+</sup>

In addition to these grammars, we will also allow block comments like <tt>{- this is a comment -}</tt>.

We will parse these grammars into an internal representation (abstract syntax tree). (In some applications you skip the internal representation and go straight to evaluation or code generation or... but let's fix just one goal here.) Here is an internal representation:
-}

data Expr = Var String | Con Bool | Uno Unop Expr | Duo Duop Expr Expr
    deriving Show
data Unop = Not deriving Show
data Duop = And | Iff deriving Show
data Stmt = Nop | String := Expr | If Expr Stmt Stmt | While Expr Stmt
          | Seq [Stmt]
    deriving Show

{-
== Sample parser ==

Here is the plan. Fill in a record to specify comments, roles of characters (what goes into identifiers, what goes into operators), reserved words, reserved operators. Give this record to <hask>makeTokenParser</hask> and get a record of utility parsers that enable us to work at the token level (rather than the character level). The expression parser is obtained with the help of <hask>buildExpressionParser</hask>. The statement parser is written as a recursive descent parser.

=== Define symbols ===

<hask>LanguageDef</hask> is the name of the record type we have to fill in. Many presets are provided so that we can pick one and just customize a few fields. A minimalist preset is <hask>emptyDef</hask> and we change it with:
-}

def = emptyDef{ commentStart = "{-"
              , commentEnd = "-}"
              , identStart = letter
              , identLetter = alphaNum
              , opStart = oneOf "~&=:"
              , opLetter = oneOf "~&=:"
              , reservedOpNames = ["~", "&", "=", ":="]
              , reservedNames = ["true", "false", "nop",
                                 "if", "then", "else", "fi",
                                 "while", "do", "od"]
              }

{-
=== Make token parser ===

When we pass the above record to <hask>makeTokenParser</hask>, the return value is a record of type <hask>TokenParser</hask>. Its fields are little parsers that we can use. They parse and return various tokens (identifiers, operators, reserved things, all sorts of brackets) and skip comments as we have specified. They also eat whitespaces after the tokens. Using them, our expression parser and statement parser can be written at the token level and without worrying about whitespaces. The only caveat: they don't eat whitespaces at the very, very, very beginning, and we have to do that ourselves, but even that is made easy.

There are only a few of the many provided parsers we will use here. Using record pattern matching, we call <hask>makeTokenParser def</hask> and pick out just those we need:

-}

TokenParser{ parens = m_parens
           , identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , semiSep1 = m_semiSep1
           , whiteSpace = m_whiteSpace } = makeTokenParser def

{-
Here is an overview of what these parsers do:

; <tt>m_parens</tt> : <hask>m_parens p</hask> parses an open parenthesis, then runs <hask>p</hask>, then parses a close parenthesis, and returns what <hask>p</hask> returns.
; <tt>m_identifier</tt> : parses and returns an identifier, checking that it does not clash with a reserved word.
; <tt>m_reservedOp</tt> : by example: <hask>m_reservedOp ":="</hask> checks that the next token is the <hask>:=</hask> reserved operator.
; <tt>m_reserved</tt> : by example: <hask>m_reserved "od"</hask> checks that the next token is the <hask>od</hask> reserved word.
; <tt>m_semiSep1</tt> : <hask>m_semiSep1 p</hask> parses and returns a semicolon-separated sequence of one or more <hask>p</hask>'s.
; <tt>m_whiteSpace</tt> : eats whitespaces.

You are encouraged to explore these and other parsers provided in the record. They are all very handy.

=== Expression parser ===

The expression parser is obtained from <hask>buildExpressionParser</hask>. We do not need to write our own recursive descent parser or perform left/right-factoring.
-}

exprparser :: Parser Expr
exprparser = buildExpressionParser table term <?> "expression"
table = [ [Prefix (m_reservedOp "~" >> return (Uno Not))]
        , [Infix (m_reservedOp "&" >> return (Duo And)) AssocLeft]
        , [Infix (m_reservedOp "=" >> return (Duo Iff)) AssocLeft]
        ]
term = m_parens exprparser
       <|> fmap Var m_identifier
       <|> (m_reserved "true" >> return (Con True))
       <|> (m_reserved "false" >> return (Con False))

expr :: String -> IO ()
expr inp = case parse exprparser "" inp of
             { Left err -> print err
             ; Right ans -> print ans
             }

{-

We give operator precedence (by ordering them in the table), association, "semantic action", and how to parse an "atomic term" including the parenthesized case (the only place we do a recursive descent, and it's trivial). It is possible to place several operators at the same precedence, though not shown here. The general format for the table is hard to explain but easy to illustrate and copy.

=== Statement parser ===

The statement parser is best done by recursive descent, with special treatment to the semicolon-separated list (easy with <hask>m_semiSep1</hask>), utilizing the handy token parsers and the expression parser. We also remember to skip whitespaces just once at the very, very, very beginning:
-}

mainparser :: Parser Stmt
mainparser = m_whiteSpace >> stmtparser <* eof
    where
      stmtparser :: Parser Stmt
      stmtparser = fmap Seq (m_semiSep1 stmt1)
      stmt1 = (m_reserved "nop" >> return Nop)
              <|> do { v <- m_identifier
                     ; m_reservedOp ":="
                     ; e <- exprparser
                     ; return (v := e)
                     }
              <|> do { m_reserved "if"
                     ; b <- exprparser
                     ; m_reserved "then"
                     ; p <- stmtparser
                     ; m_reserved "else"
                     ; q <- stmtparser
                     ; m_reserved "fi"
                     ; return (If b p q)
                     }
              <|> do { m_reserved "while"
                     ; b <- exprparser
                     ; m_reserved "do"
                     ; p <- stmtparser
                     ; m_reserved "od"
                     ; return (While b p)
                     }

{-

== Sample usage ==

Finally, here is a little piece of code for casual testing. This function parses the string parameter and outputs either a parse error or the answer.

-}

play :: String -> IO ()
play inp = case parse mainparser "" inp of
             { Left err -> print err
             ; Right ans -> print ans
             }

