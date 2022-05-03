{

-- Details of this implementation can be found at:
-- https://www.haskell.org/happy/doc/html/sec-using.html

module Parser where
import Data.Char

}

%name parser
%tokentype { Token }
%error { parseError }

%token 
  true	  { TokenTrue }
  false   { TokenFalse }
  num	    { TokenNum $$ }
  '+'	    { TokenPlus }
  '*'     { TokenMult }
  '('     { TokenOpen }
  ')'     { TokenClose }
  ','     { TokenComma }
  and     { TokenAnd }
  or      { TokenOr }
  if      { TokenIf }
  then    { TokenThen }
  else    { TokenElse }
  first   { TokenFirst }
  second  { TokenSecond }

%%

Exp	: true { BTrue }
    | false { BFalse }
    | num { Num $1 }
    | Exp '+' Exp { Add $1 $3 }
    | Exp '*' Exp { Mult $1 $3 }
    | Exp and Exp { And $1 $3 }
    | Exp or Exp { Or $1 $3 }
    | if Exp then Exp else Exp { If $2 $4 $6 }
    | '(' Exp ',' Exp ')' { Pair $2 $4 }
    | first Exp { First $2 }
    | second Exp { Second $2 }


-- Start of Lexer coding
---------------------------------
{

parseError :: [Token] -> a
parseError _ = error "Syntax error: sequência de caracteres inválida!"

-- Abstract syntax tree

data Expr = BTrue
          | BFalse
          | Num Int
          | Add Expr Expr
          | Mult Expr Expr
          | And Expr Expr
          | Or Expr Expr
          | If Expr Expr Expr
          | Pair Expr Expr
          | First Expr
          | Second Expr
          deriving (Show, Eq)

-- Tokens allowed in the language

data Token = TokenTrue
           | TokenFalse
           | TokenNum Int
           | TokenPlus
           | TokenAnd
           | TokenOr
           | TokenIf
           | TokenMult
           | TokenThen
           | TokenElse
           | TokenOpen
           | TokenClose
           | TokenComma
           | TokenFirst
           | TokenSecond
           deriving Show

-- Lexical analyzer (reads code and converts to a list of tokens)

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
     | isSpace c = lexer cs
     | isAlpha c = lexKeyWord (c:cs)
     | isDigit c = lexNum (c:cs)
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('*':cs) = TokenMult : lexer cs
lexer ('(':cs) = TokenOpen : lexer cs
lexer (')':cs) = TokenClose : lexer cs
lexer (',':cs) = TokenComma : lexer cs
lexer _ = error "Lexical error: caracter inválido!"

-- Read a boolean token
lexKeyWord cs = case span isAlpha cs of  -- if true then 1 else 0
               ("true", rest) -> TokenTrue : lexer rest
               ("false", rest) -> TokenFalse : lexer rest
               ("if", rest) -> TokenIf : lexer rest
               ("then", rest) -> TokenThen : lexer rest
               ("else", rest) -> TokenElse : lexer rest
               ("and", rest) -> TokenAnd : lexer rest
               ("or", rest) -> TokenOr : lexer rest
               ("first", rest) -> TokenFirst : lexer rest
               ("second", rest) -> TokenSecond : lexer rest

-- Read a numeric token
lexNum cs = case span isDigit cs of
              (num, rest) -> TokenNum (read num) : lexer rest

}


