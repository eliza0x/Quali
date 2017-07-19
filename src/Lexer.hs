{-# LANGUAGE OverloadedStrings #-}

module Lexer where
  
import Data.Text hiding (unwords, map)
import Text.Megaparsec hiding (Token, token, space, spaces)
import qualified Text.Megaparsec as P
import Text.Megaparsec.Text

data Expr = Bind Token [Token] [Token]
          | Load String
          | Blank

instance Show Expr where
    show (Bind a b c) = show a ++ " " ++ (unwords $ map show b) ++ " = "++ (unwords $ map show c)
    show (Load s)     = "#load " ++ show s
    show (Blank)      = ""

data Token = Mt String
           | Op String
           | Br [Token]
           | Nm String
           | Rs String
           | Nl

instance Show Token where
  show (Mt s)  = s
  show (Op s)  = s
  show (Br ss) = "(" ++ (unwords $ map show ss) ++ ")"
  show (Nm s)  = s
  show (Rs s)  = s
  show Nl      = "\n        "

lexer :: Text -> Either (ParseError (P.Token Text) Dec) [Expr]
lexer = parse expr "lexer" 

expr :: Parser [Expr]
expr = some ((bind <|> load <|> blank) <* eol) <* eof

-- spaces without eol
space :: Parser Char
space = (char ' ' <|> char '\t')
spaces, spaces1 :: Parser String
spaces = many space
spaces1 = some space

blank :: Parser Expr
blank = string "" *> return Blank

load :: Parser Expr
load = Load <$> (string "#load" *> spaces1 *> some (letterChar <|> char '/'))

bind :: Parser Expr
bind = do
  n <- name <* spaces
  args <- many (name <* spaces)
  char '=' <* spaces
  t <- some (term <* spaces)
  return $ Bind n args t

term :: Parser Token
term = brace
   <|> name
   <|> operator
   <|> num
   <|> try newLine
   
brace :: Parser Token
brace = Br <$> (char '(' *> many (term <* spaces) <* char ')')

newLine :: Parser Token
newLine = eol *> spaces1 *> return Nl

num :: Parser Token
num = Nm <$> ((:) <$> digitChar <*> many (digitChar <|> char '.'))

name :: Parser Token
name = Mt <$> some (letterChar <|> char '_')

operator :: Parser Token
operator = Op <$> some 
    (   char '!' 
    <|> char '@'
    <|> char '$'
    <|> char '~'
    <|> char '%' 
    <|> char '^' 
    <|> char '&' 
    <|> char '*' 
    <|> char '-' 
    <|> char '+' 
    <|> char '=' 
    <|> char '|' 
    <|> char '<' 
    <|> char '>' 
    <|> char '?' 
    <|> char '/')

